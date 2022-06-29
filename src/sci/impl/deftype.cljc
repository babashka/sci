(ns sci.impl.deftype
  {:no-doc true}
  (:refer-clojure :exclude [deftype])
  (:require [clojure.string :as str]
            [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]
            [sci.lang]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn assert-no-jvm-interface [protocol protocol-name expr]
     (when (and (class? protocol)
                (not (= Object protocol)))
       (utils/throw-error-with-location
        (str "defrecord/deftype currently only support protocol implementations, found: " protocol-name)
        expr))))

(defmulti to-string types/type-impl)
(defmethod to-string :default [this]
  (let [t (types/type-impl this)]
    (str (namespace t) "." (name t) "@"
         #?(:clj (Integer/toHexString (hash this))
            :cljs (.toString (hash this) 16)))))

(defn clojure-str [v]
  (let [t (types/type-impl v)]
    (str "#" t (into {} v))))

(defprotocol SciPrintMethod
  (-sci-print-method [x w]))

(defn debug [x]
  ;; (prn x)
  ;; ((requiring-resolve 'clojure.pprint/pprint) x)
  x)

(clojure.core/deftype SciType
    [rec-name
     type
     var #?(:clj ^:volatile-mutable ext-map
            :cljs ^:mutable ext-map)]
  Object
  (toString [this]
    (to-string this))

  sci.impl.types/SciTypeInstance
  (-get-type [_]
    type)

  #_#_SciPrintMethod
  (-sci-print-method [this w]
    (if-let [rv var]
      (let [m (meta @rv)]
        (if-let [pm (:sci.impl/print-method m)]
          (pm this w)
          (.write ^java.io.Writer w ^String (clojure-str this))))
      (.write ^java.io.Writer w ^String (clojure-str this))))

  types/IBox
  (getVal [_] ext-map))

(defn ->type-impl [rec-name type var m]
  (SciType. rec-name type var m))

#?(:clj
   (defmethod print-method SciType [v w]
     (-sci-print-method v w)))

(defn deftype [[fname & _ :as form] _ ctx record-name fields & raw-protocol-impls]
  (let [fname (name fname)]
    (if (:sci.impl/macroexpanding ctx)
      (cons 'clojure.core/deftype (rest form))
      (let [factory-fn-str (str "->" record-name)
            factory-fn-sym (symbol factory-fn-str)
            keys (mapv keyword fields)
            rec-type (symbol (str (munge (utils/current-ns-name)) "." (str record-name)))
            protocol-impls (utils/split-when symbol? raw-protocol-impls)
            field-set (set fields)
            protocol-impls
            (mapcat
             (fn [[protocol-name & impls] #?(:clj expr :cljs expr)]
               (let [impls (group-by first impls)
                     protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                     ;; _ (prn :protocol protocol)
                     #?@(:cljs [protocol (or protocol
                                             (when (= 'Object protocol-name)
                                               ::object))])
                     _ (when-not protocol
                         (utils/throw-error-with-location
                          (str "Protocol not found: " protocol-name)
                          expr))
                     #?@(:clj [_ (assert-no-jvm-interface protocol protocol-name expr)])
                     protocol (if (utils/var? protocol) @protocol protocol)
                     protocol-var (:var protocol)
                     _ (when protocol-var
                         ;; TODO: not all externally defined protocols might have the :var already
                         (vars/alter-var-root protocol-var update :satisfies
                                              (fnil conj #{}) (symbol (str rec-type))))
                     protocol-ns (:ns protocol)
                     pns (cond protocol-ns (str (types/getName protocol-ns))
                               (= #?(:clj Object :cljs ::object) protocol) "sci.impl.records")
                     fq-meth-name #(if (simple-symbol? %)
                                     (symbol pns (str %))
                                     %)]
                 (map (fn [[method-name bodies]]
                        (let [bodies (map rest bodies)
                              bodies (mapv (fn [impl]
                                             (let [args (first impl)
                                                   body (rest impl)
                                                   destr (utils/maybe-destructured args body)
                                                   args (:params destr)
                                                   body (:body destr)
                                                   orig-this-sym (first args)
                                                   rest-args (rest args)
                                                   shadows-this? (some #(= orig-this-sym %) rest-args)
                                                   this-sym (if shadows-this?
                                                              (gensym "this_")
                                                              orig-this-sym)
                                                   args (if shadows-this?
                                                          (vec (cons this-sym rest-args))
                                                          args)
                                                   ext-map-binding (gensym)
                                                   bindings [ext-map-binding (list 'sci.impl.deftype/-inner-impl this-sym)]
                                                   bindings (concat bindings
                                                                    (mapcat (fn [field]
                                                                              [field (list (keyword field) ext-map-binding)])
                                                                            (reduce disj field-set args)))
                                                   bindings (if shadows-this?
                                                              (concat bindings [orig-this-sym this-sym])
                                                              bindings)
                                                   bindings (vec bindings)]
                                               ;; (prn :bindings bindings)
                                               `(~args
                                                 (let ~bindings
                                                   ~@body)))) bodies)]
                          `(defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies)))
                      impls)))
             protocol-impls
             raw-protocol-impls)]
        (doto `(do
                 (declare ~record-name ~factory-fn-sym)
                 (def ~(with-meta record-name
                         {:sci/type true})
                   (sci.impl.deftype/-create-type
                    ~{:sci.impl/type-name (list 'quote rec-type)
                      :sci.impl/type rec-type
                      :sci.impl.type/constructor (list 'var factory-fn-sym)
                      :sci.impl/type-var (list 'var record-name)}))
                 (defn ~factory-fn-sym [& args#]
                   (sci.impl.deftype/->type-impl '~rec-type ~rec-type (var ~record-name) (zipmap ~keys args#)))
                 ~@protocol-impls
                 ~record-name)
          debug)))))
