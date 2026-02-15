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
   (defn assert-no-jvm-interface [protocol protocol-name expr error-hint]
     (when (and (class? protocol)
                (not (= Object protocol)))
       (utils/throw-error-with-location
        (or error-hint
            (str "defrecord/deftype currently only support protocol implementations, found: " protocol-name))
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

(defn ^:private emit-deftype
  "Generate the common deftype boilerplate: declare, def type, defn factory, protocol-impls."
  [rec-type record-name factory-fn-sym factory-fn-body & [protocol-impls]]
  `(do
     (declare ~record-name ~factory-fn-sym)
     (def ~(with-meta record-name
             {:sci/type true})
       (sci.impl.deftype/-create-type
        ~{:sci.impl/type-name (list 'quote rec-type)
          :sci.impl/type rec-type
          :sci.impl/constructor (list 'var factory-fn-sym)
          :sci.impl/var (list 'var record-name)}))
     ~factory-fn-body
     ~@protocol-impls
     ~record-name))

#?(:clj
   (defn ^:private standard-scitype-path
     "Standard SciType path for deftype — protocol-only implementations."
     [ctx form rec-type record-name factory-fn-sym fields field-set
      protocol-impls error-hint]
     (let [protocol-impls
           (mapcat
            (fn [[protocol-name & impls]]
              (let [impls (group-by first impls)
                    protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                    _ (when-not protocol
                        (utils/throw-error-with-location
                         (str "Protocol not found: " protocol-name)
                         form))
                    _ (assert-no-jvm-interface protocol protocol-name form error-hint)
                    protocol (if (utils/var? protocol) @protocol protocol)
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
            protocol-impls)]
       (emit-deftype rec-type record-name factory-fn-sym
                     `(defn ~factory-fn-sym [& args#]
                        (sci.impl.deftype/->type-impl '~rec-type ~rec-type (var ~record-name) (zipmap ~(list 'quote fields) args#)))
                     protocol-impls))))

(defn deftype-macro
  "Macro expansion for deftype. Emits a deftype* form like JVM Clojure.
   Protocol names are collected into the :implements vector, methods follow."
  [[_fname] _ record-name fields & raw-protocol-impls]
  (let [ns-name (utils/current-ns-name)
        tagged-name (symbol (str ns-name) (str record-name))
        class-name (symbol (str (munge ns-name) "." record-name))
        protocol-impls (utils/split-when symbol? raw-protocol-impls)
        interfaces (mapv first protocol-impls)
        method-counts (mapv #(count (rest %)) protocol-impls)
        methods (mapcat rest protocol-impls)]
    (list* 'deftype* tagged-name class-name fields
           :implements (with-meta interfaces {:method-counts method-counts})
           methods)))

(defn analyze-deftype*
  "Analyzer handler for deftype* special form.
   Generates the type definition and protocol implementations,
   then analyzes the result."
  [ctx [_ tagged-name class-name fields _kw interfaces & methods :as form]]
  (let [record-name (symbol (name tagged-name))
        rec-type class-name
        factory-fn-str (str "->" record-name)
        factory-fn-sym (symbol factory-fn-str)
        method-counts (:method-counts (meta interfaces))
        protocol-impls
        (let [all-methods (vec methods)]
          (loop [ifaces (seq interfaces)
                 counts (seq method-counts)
                 offset (int 0)
                 result []]
            (if ifaces
              (let [cnt (int (first counts))
                    impls (subvec all-methods offset (+ offset cnt))]
                (recur (next ifaces) (next counts) (+ offset cnt)
                       (conj result (into [(first ifaces)] impls))))
              result)))
        field-set (set fields)
        result
        #?(:clj
           (let [deftype-fn (:deftype-fn ctx)
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
                 protocols (into #{}
                                 (comp (remove :class?)
                                       (remove #(= Object (:resolved %)))
                                       (map :resolved))
                                 resolved-impls)
                 _ (doseq [protocol protocols]
                     (when-let [protocol-var (:var protocol)]
                       (vars/alter-var-root protocol-var update :satisfies
                                            (fnil conj #{}) (symbol (str rec-type)))))
                 deftype-fn-result (when (and (seq interfaces) deftype-fn)
                                    (deftype-fn {:interfaces interfaces}))
                 constructor-sym (:constructor-fn deftype-fn-result)
                 error-hint (:error deftype-fn-result)]
             (if constructor-sym
               (let [all-methods (atom {})
                     _ (doseq [[_protocol-name & impls] protocol-impls]
                         (doseq [impl impls]
                           (let [mname (symbol (clojure.core/name (first impl)))
                                 args-and-body (rest impl)]
                             (swap! all-methods update mname (fnil conj []) args-and-body))))
                     method-entries
                     (mapcat
                      (fn [[mname impls]]
                        (let [arities (mapv (fn [[args & body]]
                                              `(~args ~@body)) impls)]
                          [(list 'quote mname)
                           (if (= 1 (count arities))
                             `(fn ~@(first arities))
                             `(fn ~@arities))]))
                      @all-methods)
                     field-entries (mapcat (fn [f] [(list 'quote f) f]) fields)
                     protocols-form (if (seq protocols)
                                     `#{~@(map (fn [p] (list 'deref (:var p))) protocols)}
                                     `#{})]
                 (emit-deftype rec-type record-name factory-fn-sym
                               `(defn ~factory-fn-sym [~@fields]
                                  (~constructor-sym {:methods (hash-map ~@method-entries)
                                                     :fields (hash-map ~@field-entries)
                                                     :protocols ~protocols-form}))))
               (standard-scitype-path ctx form rec-type record-name factory-fn-sym
                                      fields field-set protocol-impls error-hint)))
           :cljs
           (let [protocol-impls
                 (mapcat
                  (fn [[protocol-name & impls]]
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
                               form))
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
                  protocol-impls)]
             (emit-deftype rec-type record-name factory-fn-sym
                          `(defn ~factory-fn-sym [& args#]
                             (sci.impl.deftype/->type-impl '~rec-type ~rec-type (var ~record-name) (zipmap ~(list 'quote fields) args#)))
                          protocol-impls)))]
    (types/->EvalForm result)))
