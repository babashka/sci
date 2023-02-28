(ns sci.impl.deftype
  {:no-doc true}
  (:refer-clojure :exclude [deftype])
  (:require
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

(defn hex-hash [this]
  #?(:clj (Integer/toHexString (hash this))
     :cljs (.toString (hash this) 16)))

(defmulti to-string types/type-impl)
(defmethod to-string :default [this]
  (let [t (types/type-impl this)]
    (str (namespace t) "." (name t) "@"
         (hex-hash this))))

(defmulti equals (fn [this _other]
                   (types/type-impl this)))
(defmethod equals :default [this other]
  (identical? this other))

(defn clojure-str [v]
  ;; #object[user.Foo 0x743e63ce "user.Foo@743e63ce"]
  (let [n (types/type-impl v)]
    (str "#object[" n " 0x" (hex-hash v) " \"" (to-string v) "\"]")))

(defprotocol SciPrintMethod
  (-sci-print-method [x w]))

(clojure.core/deftype SciType
    [rec-name
     type
     var #?(:clj ^:volatile-mutable ext-map
            :cljs ^:mutable ext-map)]
  Object
  (toString [this]
    (to-string this))
  (equals [this other]
    (sci.impl.deftype/equals this other))

  sci.impl.types/SciTypeInstance
  (-get-type [_]
    type)
  (-mutate [_ k v]
    (set! ext-map (assoc ext-map k v))
    v)

  #?@(:clj [SciPrintMethod
            (-sci-print-method [this w]
                               (if-let [rv var]
                                 (let [m (meta rv)]
                                   (if-let [pm (:sci.impl/print-method m)]
                                     (pm this w)
                                     (.write ^java.io.Writer w ^String (clojure-str this))))
                                 (.write ^java.io.Writer w ^String (clojure-str this))))]
      :cljs [IPrintWithWriter
             (-pr-writer [this w opts]
                         (if-let [rv var]
                           (let [m (meta rv)]
                             (if-let [pm (:sci.impl/print-method m)]
                               (pm this w opts)
                               (write-all w (clojure-str this))))
                           (write-all w (clojure-str this))))])

  types/IBox
  (getVal [_] ext-map))

(defn ->type-impl [rec-name type var m]
  (SciType. rec-name type var m))

#?(:clj
   (defmethod print-method SciType [v w]
     (-sci-print-method v w)))

(defn deftype [[_fname & _ :as form] _ ctx record-name fields & raw-protocol-impls]
  (if (:sci.impl/macroexpanding ctx)
    (cons 'clojure.core/deftype (rest form))
    (let [factory-fn-str (str "->" record-name)
          factory-fn-sym (symbol factory-fn-str)
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
                                             ::object)
                                           (when (= 'IPrintWithWriter protocol-name)
                                             ::IPrintWithWriter))])
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
                             (= #?(:clj Object :cljs ::object) protocol) "sci.impl.deftype")
                   fq-meth-name #(if (simple-symbol? %)
                                   (symbol pns (str %))
                                   %)]
               (map (fn [[method-name bodies]]
                      (if #?(:cljs (and (keyword-identical? ::IPrintWithWriter protocol)
                                        (= '-pr-writer method-name))
                             :clj false)
                        #?(:cljs
                           `(alter-meta! (var ~record-name)
                                         assoc :sci.impl/print-method (fn ~(rest (first bodies))))
                           :clj nil)
                        (let [bodies (map rest bodies)
                              bodies (mapv (fn [impl]
                                             (let [args (first impl)
                                                   body (rest impl)
                                                   destr (utils/maybe-destructured args body)
                                                   args (:params destr)
                                                   body (:body destr)
                                                   orig-this-sym (first args)
                                                   rest-args (rest args)
                                                   ;; shadows-this? (some #(= orig-this-sym %) rest-args)
                                                   this-sym (if true #_shadows-this?
                                                                '__sci_this
                                                                orig-this-sym)
                                                   args (vec (cons this-sym rest-args))
                                                   ext-map-binding (gensym)
                                                   bindings [ext-map-binding (list 'sci.impl.deftype/-inner-impl this-sym)]
                                                   bindings (concat bindings
                                                                    (mapcat (fn [field]
                                                                              ;; TODO: the premature get is only necessary for immutable bindings
                                                                              ;; We could however delay the getting of these values for both immutable and mutable fields.
                                                                              ;; Currently a mutable binding is retrieved from the ext-map directly, since it can be mutated in the body we're analyzing here
                                                                              ;; See resolve.cljc. We could apply the same trick to records.
                                                                              [field (list 'get ext-map-binding (list 'quote field))])
                                                                            (reduce disj field-set args)))
                                                   bindings (concat bindings [orig-this-sym this-sym])
                                                   bindings (vec bindings)]
                                               ;; (prn :bindings bindings)
                                               `(~args
                                                 (let ~bindings
                                                   ~@body)))) bodies)]
                          (@utils/analyze (assoc ctx
                                                 :deftype-fields field-set
                                                 :local->mutator (zipmap field-set
                                                                         (map (fn [field]
                                                                                (fn [this v]
                                                                                  (types/-mutate this field v)))
                                                                              field-set)))
                           `(defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies)))))
                    impls)))
           protocol-impls
           raw-protocol-impls)]
      `(do
         (declare ~record-name ~factory-fn-sym)
         (def ~(with-meta record-name
                 {:sci/type true})
           (sci.impl.deftype/-create-type
            ~{:sci.impl/type-name (list 'quote rec-type)
              :sci.impl/type rec-type
              :sci.impl/constructor (list 'var factory-fn-sym)
              :sci.impl/var (list 'var record-name)}))
         (defn ~factory-fn-sym [& args#]
           (sci.impl.deftype/->type-impl '~rec-type ~rec-type (var ~record-name) (zipmap ~(list 'quote fields) args#)))
         ~@protocol-impls
         ~record-name))))
