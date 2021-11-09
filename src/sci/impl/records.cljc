(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

#?(:clj
   (defn assert-no-jvm-interface [protocol protocol-name expr]
     (when (and (class? protocol)
                (not (= Object protocol)))
       (utils/throw-error-with-location
        (str "Records currently only support protocol implementations, found: " protocol-name)
        expr))))

(defmulti to-string types/type-impl)
(defmethod to-string :default [this]
  (let [t (types/type-impl this)]
    (str (namespace t) "." (name t) "@"
         #?(:clj (Integer/toHexString (hash this))
            :cljs (.toString (hash this) 16)))))

(defn clojure-str [v]
  (let [t (types/type-impl v)]
    (str "#" (namespace t) "." (name t)
         (into {} v))))

(clojure.core/defrecord SciRecord []
  Object
  (toString [this]
    (to-string this)))

#?(:clj
   (defmethod print-method SciRecord [v ^java.io.Writer w]
     (.write w (clojure-str v))))

#?(:cljs ;; see https://www.mail-archive.com/clojure@googlegroups.com/msg99560.html
   (extend-type SciRecord
     IPrintWithWriter
     (-pr-writer [new-obj writer _]
       (write-all writer (clojure-str new-obj)))))

(defn ->record-impl [m]
  (map->SciRecord m))

(defn defrecord [_form _ ctx record-name fields & raw-protocol-impls]
  (let [factory-fn-str (str "->" record-name)
        factory-fn-sym (symbol factory-fn-str)
        map-factory-sym (symbol (str "map" factory-fn-str))
        keys (mapv keyword fields)
        rec-type (symbol (str (vars/current-ns-name)) (str record-name))
        protocol-impls (utils/split-when symbol? raw-protocol-impls)
        field-set (set fields)
        protocol-impls
        (mapcat
         (fn [[protocol-name & impls] #?(:clj expr :cljs expr)]
           (let [impls (group-by first impls)
                 protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                 #?@(:cljs [protocol (or protocol
                                         (when (= 'Object protocol-name)
                                           ::object))])
                 _ (when-not protocol
                     (utils/throw-error-with-location
                      (str "Protocol not found: " protocol-name)
                      expr))
                 ;; #?@(:clj [_ (assert-no-jvm-interface protocol protocol-name expr)])
                 protocol (if (vars/var? protocol) @protocol protocol)
                 protocol-ns (:ns protocol)
                 pns (cond protocol-ns (str (vars/getName protocol-ns))
                           (= #?(:clj Object :cljs ::object) protocol) "sci.impl.records")
                 fq-meth-name #(symbol pns %)]
             (map (fn [[method-name bodies]]
                    (let [bodies (map rest bodies)
                          bodies (mapv (fn [impl]
                                         (let [args (first impl)
                                               body (rest impl)
                                               destr (utils/maybe-destructured args body)
                                               args (:params destr)
                                               orig-this-sym (first args)
                                               rest-args (rest args)
                                               shadows-this? (some #(= orig-this-sym %) rest-args)
                                               this-sym (if shadows-this?
                                                          (gensym "this_")
                                                          orig-this-sym)
                                               args (if shadows-this?
                                                      (vec (cons this-sym rest-args))
                                                      args)
                                               bindings (mapcat (fn [field]
                                                                  [field (list (keyword field) this-sym)])
                                                                (reduce disj field-set args))
                                               bindings (if shadows-this?
                                                          (concat bindings [orig-this-sym this-sym])
                                                          bindings)
                                               bindings (vec bindings)]
                                           `(~args
                                             (let ~bindings
                                               ~@body)))) bodies)]
                      `(defmethod ~(fq-meth-name (str method-name)) '~rec-type ~@bodies)))
                  impls)))
         protocol-impls
         raw-protocol-impls)]
    `(do
       (defn ~map-factory-sym [m#]
         (vary-meta (clojure.core/->record-impl m#)
                    assoc
                    ;; TODO: now that we're using the SciRecord type, we could move away from these metadata keys
                    :sci.impl/record true
                    :type '~rec-type))
       (defn ~factory-fn-sym [& args#]
         (vary-meta (clojure.core/->record-impl (zipmap ~keys args#))
                    assoc
                    :sci.impl/record true
                    :type '~rec-type))
       (def ~record-name (with-meta '~rec-type
                           {:sci.impl/record true
                            :sci.impl.record/map-constructor ~map-factory-sym
                            :sci.impl.record/constructor ~factory-fn-sym}))
       ~@protocol-impls)))

(defn sci-record? [x]
  (or
   (when (map? x)
     (some-> x meta :sci.impl/record))
   (clojure.core/record? x)))

(defn resolve-record-or-protocol-class
  "A record class is represented by a symbol with metadata (currently). This is only an implementation detail.
   A protocol is represented by a map with :ns, :methods and optionally :class. This is also an implementation detail."
  ;; TODO: we should probably use munging here for namespaces with hyphens in them.
  ([ctx sym]
   (let [sym-str (str sym)
         last-dot (str/last-index-of sym-str ".")
         class-name (if last-dot
                      (subs sym-str (inc last-dot) (count sym-str))
                      sym-str)
         namespace (if last-dot
                     (symbol (subs sym-str 0 last-dot))
                     (vars/current-ns-name))]
     (resolve-record-or-protocol-class ctx namespace (symbol class-name))))
  ([ctx package class]
   (let [namespace (-> package str (str/replace "_" "-") symbol)]
     (when-let [sci-var (get-in @(:env ctx) [:namespaces namespace class])]
       (if (vars/var? sci-var)
         @sci-var
         sci-var)))))

(defn resolve-record-class
  [ctx class-sym]
  (when-let [x (resolve-record-or-protocol-class ctx class-sym)]
    (when (symbol? x) x)))
