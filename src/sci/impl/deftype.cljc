(ns sci.impl.deftype
  {:no-doc true}
  (:refer-clojure :exclude [deftype])
  (:require
   [sci.ctx-store :as store]
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

#?(:clj
   (do
     (defmulti equals (fn [this _other]
                        (types/type-impl this)))
     (defmethod equals :default [this other]
       (identical? this other))))

#?(:clj
   (do
     (defmulti hashCode types/type-impl)
     (defmethod hashCode :default [this]
       (System/identityHashCode this))))

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
  #?(:clj (equals [this other]
                  (sci.impl.deftype/equals this other)))
  #?(:clj (hashCode [this]
                    (sci.impl.deftype/hashCode this)))

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
  (getVal [_] ext-map)

  #?@(:clj [sci.impl.types.ICustomType]
      :cljs [types/ICustomType])
  (getMethods [_] nil)
  (getInterfaces [_] nil)
  (getProtocols [_] nil)
  (getFields [_] ext-map))

(defn ->type-impl [rec-name type var m]
  (SciType. rec-name type var m))

#?(:clj
   (defmethod print-method SciType [v w]
     (-sci-print-method v w)))

(defn ^:private standard-scitype-path
  "Standard SciType path for deftype — protocol-only implementations."
  [ctx form rec-type record-name factory-fn-sym fields field-set
   protocol-impls raw-protocol-impls]
  (let [protocol-impls
        (mapcat
         (fn [[protocol-name & impls] expr]
           (let [impls (group-by first impls)
                 protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                 _ (when-not protocol
                     (utils/throw-error-with-location
                      (str "Protocol not found: " protocol-name)
                      expr))
                 _ (assert-no-jvm-interface protocol protocol-name expr)
                 protocol (if (utils/var? protocol) @protocol protocol)
                 protocol-var (:var protocol)
                 _ (when protocol-var
                     (vars/alter-var-root protocol-var update :satisfies
                                          (fnil conj #{}) (symbol (str rec-type))))
                 protocol-ns (:ns protocol)
                 pns (cond protocol-ns (str (types/getName protocol-ns))
                           (= Object protocol) "sci.impl.deftype")
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
                                               this-sym (if true #_shadows-this?
                                                            '__sci_this
                                                            orig-this-sym)
                                               args (vec (cons this-sym rest-args))
                                               ext-map-binding (gensym)
                                               bindings [ext-map-binding (list 'sci.impl.deftype/-inner-impl this-sym)]
                                               bindings (concat bindings
                                                                (mapcat (fn [field]
                                                                          [field (list 'get ext-map-binding (list 'quote field))])
                                                                        (reduce disj field-set args)))
                                               bindings (concat bindings [orig-this-sym this-sym])
                                               bindings (vec bindings)]
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
                       `(defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies))))
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
       ~record-name)))

(defn deftype [[_fname & _ :as form] _ record-name fields & raw-protocol-impls]
  (let [ctx (store/get-ctx)]
    (if (:sci.impl/macroexpanding ctx)
      (cons 'clojure.core/deftype (rest form))
      (let [factory-fn-str (str "->" record-name)
            factory-fn-sym (symbol factory-fn-str)
            rec-type (symbol (str (munge (utils/current-ns-name)) "." record-name))
            protocol-impls (utils/split-when symbol? raw-protocol-impls)
            field-set (set fields)]
        #?(:clj
           (let [deftype-fn (:deftype-fn ctx)
                 ;; Resolve all protocol-impl names and collect interfaces
                 resolved-impls
                 (mapv (fn [[protocol-name & impls]]
                         (let [resolved (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                               _ (when-not resolved
                                   (utils/throw-error-with-location
                                    (str "Protocol not found: " protocol-name) form))
                               resolved-val (if (utils/var? resolved) @resolved resolved)]
                           {:protocol-name protocol-name
                            :resolved resolved-val
                            :class? (class? resolved-val)
                            :impls impls}))
                       protocol-impls)
                 interfaces (into #{} (comp (filter :class?)
                                            (remove #(= Object (:resolved %)))
                                            (map :resolved))
                                  resolved-impls)
                 ;; Try the deftype-fn if interfaces are present and a factory exists
                 custom-result (when (and (seq interfaces) deftype-fn)
                                 (deftype-fn {:ctx ctx
                                              :form form
                                              :rec-type rec-type
                                              :record-name record-name
                                              :factory-fn-sym factory-fn-sym
                                              :fields fields
                                              :field-set field-set
                                              :interfaces interfaces
                                              :protocol-impls protocol-impls
                                              :resolved-impls resolved-impls}))]
             (if custom-result
               custom-result
               (standard-scitype-path ctx form rec-type record-name factory-fn-sym
                                      fields field-set protocol-impls raw-protocol-impls)))
           :cljs
           (let [protocol-impls
                 (mapcat
                  (fn [[protocol-name & impls] expr]
                    (let [impls (group-by first impls)
                          protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                          protocol (or protocol
                                       (when (= 'Object protocol-name)
                                         ::object)
                                       (when (= 'IPrintWithWriter protocol-name)
                                         ::IPrintWithWriter))
                          _ (when-not protocol
                              (utils/throw-error-with-location
                               (str "Protocol not found: " protocol-name)
                               expr))
                          protocol (if (utils/var? protocol) @protocol protocol)
                          protocol-var (:var protocol)
                          _ (when protocol-var
                              (vars/alter-var-root protocol-var update :satisfies
                                                   (fnil conj #{}) (symbol (str rec-type))))
                          protocol-ns (:ns protocol)
                          pns (cond protocol-ns (str (types/getName protocol-ns))
                                    (= ::object protocol) "sci.impl.deftype")
                          fq-meth-name #(if (simple-symbol? %)
                                          (symbol pns (str %))
                                          %)]
                      (map (fn [[method-name bodies]]
                             (if (and (keyword-identical? ::IPrintWithWriter protocol)
                                      (= '-pr-writer method-name))
                               `(alter-meta! (var ~record-name)
                                             assoc :sci.impl/print-method (fn ~(rest (first bodies))))
                               (let [bodies (map rest bodies)
                                     bodies (mapv (fn [impl]
                                                    (let [args (first impl)
                                                          body (rest impl)
                                                          destr (utils/maybe-destructured args body)
                                                          args (:params destr)
                                                          body (:body destr)
                                                          orig-this-sym (first args)
                                                          rest-args (rest args)
                                                          this-sym (if true #_shadows-this?
                                                                       '__sci_this
                                                                       orig-this-sym)
                                                          args (vec (cons this-sym rest-args))
                                                          ext-map-binding (gensym)
                                                          bindings [ext-map-binding (list 'sci.impl.deftype/-inner-impl this-sym)]
                                                          bindings (concat bindings
                                                                           (mapcat (fn [field]
                                                                                     [field (list 'get ext-map-binding (list 'quote field))])
                                                                                   (reduce disj field-set args)))
                                                          bindings (concat bindings [orig-this-sym this-sym])
                                                          bindings (vec bindings)]
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
                ~record-name)))))))
