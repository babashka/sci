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
    (str t "@"
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
     type-meta #?(:clj ^:volatile-mutable ext-map
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
                               (if-let [rv type-meta]
                                 (let [m (meta rv)]
                                   (if-let [pm (:sci.impl/print-method m)]
                                     (pm this w)
                                     (.write ^java.io.Writer w ^String (clojure-str this))))
                                 (.write ^java.io.Writer w ^String (clojure-str this))))]
      :cljs [IPrintWithWriter
             (-pr-writer [this w opts]
                         (if-let [rv type-meta]
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

(defn ->type-impl [rec-name type type-meta m]
  (SciType. rec-name type type-meta m))

#?(:clj
   (defmethod print-method SciType [v w]
     (-sci-print-method v w)))

(defn ^:private emit-deftype
  "Generate the common deftype boilerplate: declare, def type, defn factory, protocol-impls."
  [rec-type record-name factory-fn-sym factory-fn-body & [protocol-impls]]
  `(do
     (declare ~factory-fn-sym)
     (sci.impl.deftype/-create-type
      ~{:sci.impl/type-name (list 'quote rec-type)
        :sci.impl/constructor (list 'var factory-fn-sym)})
     ~factory-fn-body
     ~@protocol-impls
     ~record-name))

(defn ^:private emit-defrecord
  "Generate defrecord boilerplate: -create-record-type, constructor fns, protocol-impls."
  [rec-type record-name factory-fn-sym constructor-fn-sym map-factory-sym
   fields key-set keys nil-map protocol-impls]
  `(do
     (declare ~factory-fn-sym ~constructor-fn-sym ~map-factory-sym)
     (sci.impl.records/-create-record-type
      ~{:sci.impl/type-name (list 'quote rec-type)
        :sci.impl/record true
        :sci.impl/constructor (list 'var constructor-fn-sym)
        :sci.impl.record/map-constructor (list 'var map-factory-sym)})
     (defn ~constructor-fn-sym
       (~fields
        (~constructor-fn-sym ~@fields nil nil))
       ([~@fields meta# ext#]
        (sci.impl.records/->record-impl '~rec-type
                                        ~record-name
                                        ~key-set
                                        ~record-name
                                        (cond-> (zipmap ~keys ~fields)
                                          ext# (merge ext#)
                                          meta# (with-meta meta#)))))
     (defn ~(with-meta factory-fn-sym
              {:doc (str "Positional factory function for class " rec-type ".")})
       (~fields
        (~constructor-fn-sym ~@fields nil nil)))
     (defn ~(with-meta map-factory-sym
              {:doc (str "Factory function for class " rec-type ", taking a map of keywords to field values.")})
       [m#]
       (sci.impl.records/->record-impl '~rec-type
                                       ~record-name
                                       ~key-set
                                       ~record-name
                                       (merge '~nil-map m#)))
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
                     `(defn ~(with-meta factory-fn-sym
                               {:doc (str "Positional factory function for class " rec-type ".")})
                        ~fields
                        (sci.impl.deftype/->type-impl '~rec-type ~record-name ~record-name (zipmap ~(list 'quote fields) ~fields)))
                     protocol-impls))))

(defn ^:private standard-record-path
  "Record-specific path — protocol implementations with keyword-based field access."
  [ctx form rec-type record-name factory-fn-sym constructor-fn-sym map-factory-sym
   fields field-set key-set keys nil-map protocol-impls]
  (let [protocol-impls
        (mapcat
         (fn [[protocol-name & impls]]
           (let [impls (group-by first impls)
                 protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                 #?@(:cljs [protocol (or protocol
                                         (when (= 'Object protocol-name)
                                           ::object))])
                 _ (when-not protocol
                     (utils/throw-error-with-location
                      (str "Protocol not found: " protocol-name)
                      form))
                 #?@(:clj [_ (assert-no-jvm-interface protocol protocol-name form nil)])
                 protocol (if (utils/var? protocol) @protocol protocol)
                 protocol-var (:var protocol)
                 _ (when protocol-var
                     (vars/alter-var-root protocol-var update :satisfies
                                          (fnil conj #{}) (str rec-type)))
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
                      `(defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies)))
                  impls)))
         protocol-impls)]
    (emit-defrecord rec-type record-name factory-fn-sym constructor-fn-sym map-factory-sym
                    fields key-set keys nil-map protocol-impls)))

(defn deftype-macro
  "Macro expansion for deftype. Emits a (do (declare ->TypeName) (deftype* ...) (import ...))
   so that macroexpand reveals the constructor for static-analysis tools (e.g. Clerk).
   The declare is overwritten at analysis time by analyze-deftype*."
  [[_fname] _ record-name fields & raw-protocol-impls]
  (let [ns-name (utils/current-ns-name)
        tagged-name (symbol (str ns-name) (str record-name))
        class-name (symbol (str (munge ns-name) "." record-name))
        factory-fn-sym (symbol (str "->" record-name))
        protocol-impls (utils/split-when symbol? raw-protocol-impls)
        interfaces (mapv first protocol-impls)
        method-counts (mapv #(count (rest %)) protocol-impls)
        methods (mapcat rest protocol-impls)]
    (list 'do
          (list 'declare factory-fn-sym)
          (list* 'deftype* tagged-name class-name fields
                 :implements (with-meta interfaces {:method-counts method-counts})
                 methods)
          (list 'import (list (symbol (str ns-name)) record-name)))))

(defn analyze-deftype*
  "Analyzer handler for deftype* special form.
   Generates the type definition and protocol implementations,
   then analyzes the result."
  [ctx [_ tagged-name class-name fields _kw interfaces & methods :as form] top-level?]
  (let [record-name (symbol (name tagged-name))
        rec-type class-name
        iface-meta (meta interfaces)
        record? (:record iface-meta)
        factory-fn-str (str "->" record-name)
        factory-fn-sym (symbol factory-fn-str)
        method-counts (:method-counts iface-meta)
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
        (if record?
          ;; Record-specific code generation
          (let [constructor-fn-sym (symbol (str "__" factory-fn-str "__ctor__"))
                map-factory-sym (symbol (str "map->" record-name))
                keys (mapv keyword fields)
                key-set (set keys)
                nil-map (zipmap (map keyword field-set) (repeat nil))]
            (standard-record-path ctx form rec-type record-name factory-fn-sym
                                  constructor-fn-sym map-factory-sym
                                  fields field-set key-set keys nil-map protocol-impls))
          ;; Standard deftype code generation
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
                                 `(defn ~(with-meta factory-fn-sym
                                           {:doc (str "Positional factory function for class " rec-type ".")
                                            :arglists (list fields)})
                                    [~@fields]
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
                                 `(alter-meta! ~record-name
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
                            `(defn ~(with-meta factory-fn-sym
                                      {:doc (str "Positional factory function for class " rec-type ".")})
                               ~fields
                               (sci.impl.deftype/->type-impl '~rec-type ~record-name ~record-name (zipmap ~(list 'quote fields) ~fields)))
                            protocol-impls))))]
    (if top-level?
      (types/->EvalForm result)
      (do (swap! (:env ctx) update-in
                 [:namespaces (symbol (namespace tagged-name)) :types]
                 assoc record-name (sci.lang.Type. {:sci.impl/type-name rec-type}))
          (@utils/analyze ctx result)))))
