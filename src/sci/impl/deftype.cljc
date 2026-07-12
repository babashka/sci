(ns sci.impl.deftype
  {:no-doc true}
  (:refer-clojure :exclude [deftype])
  (:require
   [sci.ctx-store :as store]
   #?(:cljd [sci.impl.multimethods :as mm])
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]
   [sci.lang :as lang]))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

#?(:clj
   (defn assert-no-jvm-interface [protocol protocol-name expr error-hint]
     (when (and (class? protocol)
                (not (= Object protocol))
                (not (= clojure.lang.IFn protocol)))
       (utils/throw-error-with-location
        (or error-hint
            (str "defrecord/deftype currently only support protocol implementations, found: " protocol-name))
        expr))))

(defn hex-hash [this]
  #?(:cljd (.toRadixString (hash this) 16)
     :clj (Integer/toHexString (hash this))
     :cljs (.toString (hash this) 16)))

#?(:cljd
   (def to-string
     (mm/->SciMultiFn 'to-string types/type-impl :default
                      (atom {:default
                             (fn [this]
                               (str (types/type-impl this) "@" (hex-hash this)))})))
   :default
   (do (defmulti to-string types/type-impl)
       (defmethod to-string :default [this]
         (let [t (types/type-impl this)]
           (str t "@"
                (hex-hash this))))))

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
     type-meta #?(:cljd ^:mutable ext-map
                  :clj ^:volatile-mutable ext-map
                  :cljs ^:mutable ext-map)]
  Object
  (toString [this]
    (to-string this))
  #?@(:cljd []
      :clj [(equals [this other]
              (sci.impl.deftype/equals this other))
            (hashCode [this]
              (sci.impl.deftype/hashCode this))])

  sci.impl.types/SciTypeInstance
  (-get-type [_]
    type)
  (-mutate [_ k v]
    (set! ext-map (assoc ext-map k v))
    v)

  #?@(:cljd [IFn
             (-invoke [this] (types/sci-invoke this))
             (-invoke [this a] (types/sci-invoke this a))
             (-invoke [this a b] (types/sci-invoke this a b))
             (-invoke [this a b c] (types/sci-invoke this a b c))
             (-invoke [this a b c d] (types/sci-invoke this a b c d))
             (-invoke [this a b c d e] (types/sci-invoke this a b c d e))
             (-invoke [this a b c d e f] (types/sci-invoke this a b c d e f))
             (-invoke [this a b c d e f g] (types/sci-invoke this a b c d e f g))
             (-invoke [this a b c d e f g h] (types/sci-invoke this a b c d e f g h))
             (-invoke [this a b c d e f g h i] (types/sci-invoke this a b c d e f g h i))
             (-invoke-more [this a b c d e f g h i rest]
               (apply types/sci-invoke this a b c d e f g h i rest))
             (-apply [this args]
               (apply types/sci-invoke this args))]
      :clj [SciPrintMethod
            (-sci-print-method [this w]
                               (if-let [rv type-meta]
                                 (let [m (meta rv)]
                                   (if-let [pm (:sci.impl/print-method m)]
                                     (pm this w)
                                     (.write ^java.io.Writer w ^String (clojure-str this))))
                                 (.write ^java.io.Writer w ^String (clojure-str this))))
            clojure.lang.IFn
            (invoke [this] (types/sci-invoke this))
            (invoke [this a] (types/sci-invoke this a))
            (invoke [this a b] (types/sci-invoke this a b))
            (invoke [this a b c] (types/sci-invoke this a b c))
            (invoke [this a b c d] (types/sci-invoke this a b c d))
            (invoke [this a b c d e] (types/sci-invoke this a b c d e))
            (invoke [this a b c d e f] (types/sci-invoke this a b c d e f))
            (invoke [this a b c d e f g] (types/sci-invoke this a b c d e f g))
            (invoke [this a b c d e f g h] (types/sci-invoke this a b c d e f g h))
            (invoke [this a b c d e f g h i] (types/sci-invoke this a b c d e f g h i))
            (invoke [this a b c d e f g h i j] (types/sci-invoke this a b c d e f g h i j))
            (invoke [this a b c d e f g h i j k] (types/sci-invoke this a b c d e f g h i j k))
            (invoke [this a b c d e f g h i j k l] (types/sci-invoke this a b c d e f g h i j k l))
            (invoke [this a b c d e f g h i j k l m] (types/sci-invoke this a b c d e f g h i j k l m))
            (invoke [this a b c d e f g h i j k l m n] (types/sci-invoke this a b c d e f g h i j k l m n))
            (invoke [this a b c d e f g h i j k l m n o] (types/sci-invoke this a b c d e f g h i j k l m n o))
            (invoke [this a b c d e f g h i j k l m n o p] (types/sci-invoke this a b c d e f g h i j k l m n o p))
            (invoke [this a b c d e f g h i j k l m n o p q] (types/sci-invoke this a b c d e f g h i j k l m n o p q))
            (invoke [this a b c d e f g h i j k l m n o p q r] (types/sci-invoke this a b c d e f g h i j k l m n o p q r))
            (invoke [this a b c d e f g h i j k l m n o p q r s] (types/sci-invoke this a b c d e f g h i j k l m n o p q r s))
            (invoke [this a b c d e f g h i j k l m n o p q r s t] (types/sci-invoke this a b c d e f g h i j k l m n o p q r s t))
            (applyTo [this args] (types/sci-apply-to this args))]
      :cljs [IPrintWithWriter
             (-pr-writer [this w opts]
                         (types/sci-pr-writer this w opts))
             IFn
             (-invoke [this]
                      (types/sci-invoke this))
             (-invoke [this a]
                      (types/sci-invoke this a))
             (-invoke [this a b]
                      (types/sci-invoke this a b))
             (-invoke [this a b c]
                      (types/sci-invoke this a b c))
             (-invoke [this a b c d]
                      (types/sci-invoke this a b c d))
             (-invoke [this a b c d e]
                      (types/sci-invoke this a b c d e))
             (-invoke [this a b c d e f]
                      (types/sci-invoke this a b c d e f))
             (-invoke [this a b c d e f g]
                      (types/sci-invoke this a b c d e f g))
             (-invoke [this a b c d e f g h]
                      (types/sci-invoke this a b c d e f g h))
             (-invoke [this a b c d e f g h i]
                      (types/sci-invoke this a b c d e f g h i))
             (-invoke [this a b c d e f g h i j]
                      (types/sci-invoke this a b c d e f g h i j))
             (-invoke [this a b c d e f g h i j k]
                      (types/sci-invoke this a b c d e f g h i j k))
             (-invoke [this a b c d e f g h i j k l]
                      (types/sci-invoke this a b c d e f g h i j k l))
             (-invoke [this a b c d e f g h i j k l m]
                      (types/sci-invoke this a b c d e f g h i j k l m))
             (-invoke [this a b c d e f g h i j k l m n]
                      (types/sci-invoke this a b c d e f g h i j k l m n))
             (-invoke [this a b c d e f g h i j k l m n o]
                      (types/sci-invoke this a b c d e f g h i j k l m n o))
             (-invoke [this a b c d e f g h i j k l m n o p]
                      (types/sci-invoke this a b c d e f g h i j k l m n o p))
             (-invoke [this a b c d e f g h i j k l m n o p q]
                      (types/sci-invoke this a b c d e f g h i j k l m n o p q))
             (-invoke [this a b c d e f g h i j k l m n o p q r]
                      (types/sci-invoke this a b c d e f g h i j k l m n o p q r))
             (-invoke [this a b c d e f g h i j k l m n o p q r s]
                      (types/sci-invoke this a b c d e f g h i j k l m n o p q r s))
             (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                      (types/sci-invoke this a b c d e f g h i j k l m n o p q r s t))])

  types/IBox
  (getVal [_] ext-map)

  #?@(:cljd [types/ICustomType]
      :clj [sci.impl.types.ICustomType]
      :cljs [types/ICustomType])
  (getMethods [_] nil)
  (getInterfaces [_] nil)
  (getProtocols [_] nil)
  (getFields [_] ext-map))

#?(:cljs
   (defn new-js-prototype
     "Fresh prototype chaining to base-class's (SciType or SciRecord), so
  native protocol methods can be installed per sci type without affecting
  other sci types."
     ([] (new-js-prototype SciType))
     ([base-class] (js/Object.create (.-prototype base-class)))))

#?(:cljs
   (defn ensure-js-prototype [t]
     (let [data (types/getVal t)]
       (or (:sci.impl/js-prototype data)
           (let [proto (new-js-prototype)]
             (types/setVal t (assoc data :sci.impl/js-prototype proto))
             proto)))))

#?(:cljs
   (defn ^:private with-js-this
     "Adapts a sci fn to the CLJS protocol slot calling convention: the
  receiver comes in as JS `this` while the first positional argument may be
  null (`o.slot(null, k)` for ^not-native invokes)."
     [arity impl]
     (case arity
       1 (fn [_] (this-as self (impl self)))
       2 (fn [_ a] (this-as self (impl self a)))
       3 (fn [_ a b] (this-as self (impl self a b)))
       4 (fn [_ a b c] (this-as self (impl self a b c)))
       5 (fn [_ a b c d] (this-as self (impl self a b c d)))
       (fn [& args] (this-as self (apply impl self (rest args)))))))

#?(:cljs
   (defn -install-native-protocol-on!
     "Installs method impls for a native CLJS protocol (entry created with
  sci.core/copy-var on a protocol) on JS object `obj`: a per-type prototype
  for deftype, a single instance for reify."
     [obj proto-map impls]
     (let [native-methods (:native-methods proto-map)]
       ((:marker-setter proto-map) obj)
       (doseq [[msym {:keys [arities impl]}] impls]
         (let [m (or (get native-methods msym)
                     (throw (js/Error. (str "Method " msym " not found on protocol " (:name proto-map)))))]
           (doseq [arity arities]
             (let [setter (or (get (:setters m) arity)
                              (throw (js/Error. (str "Invalid arity " arity " for method " msym
                                                     " of protocol " (:name proto-map)))))]
               (setter obj (with-js-this arity impl))))))
       nil)))

#?(:cljs
   (defn -install-native-protocol!
     "Installs method impls for a native CLJS protocol on the JS prototype of
  sci type `t`."
     [t proto-map impls]
     (-install-native-protocol-on! (ensure-js-prototype t) proto-map impls)))

(defn ->type-impl [rec-name type type-meta m]
  #?(:cljs (if-let [proto (when (instance? lang/Type type)
                            (:sci.impl/js-prototype (types/getVal type)))]
             (let [obj (js/Object.create proto)]
               (.call SciType obj rec-name type type-meta m)
               obj)
             (SciType. rec-name type type-meta m))
     :default (SciType. rec-name type type-meta m)))

#?(:cljs
   (defmethod types/sci-pr-writer :default [this w opts]
     (if (cljs.core/implements? types/SciTypeInstance this)
       (if (implements? IRecord this)
         (write-all w (str "#" (types/type-impl this) (into {} this)))
         (write-all w (clojure-str this)))
       (cljs.core/-pr-writer this w opts))))

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

(defn ^:private emit-record-type
  "Generate record type creation and protocol implementations (no factory fns)."
  [rec-type record-name constructor-fn-sym map-factory-sym protocol-impls]
  `(do
     (sci.impl.records/-create-record-type
      ~{:sci.impl/type-name (list 'quote rec-type)
        :sci.impl/record true
        :sci.impl/constructor (list 'var constructor-fn-sym)
        :sci.impl.record/map-constructor (list 'var map-factory-sym)})
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
                          `(~'clojure.core/defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies))))
                     impls)))
            protocol-impls)]
       (emit-deftype rec-type record-name factory-fn-sym
                     `(defn ~(with-meta factory-fn-sym
                               {:doc (str "Positional factory function for class " rec-type ".")})
                        ~fields
                        (sci.impl.deftype/->type-impl '~rec-type ~record-name ~record-name (zipmap ~(list 'quote fields) ~fields)))
                     protocol-impls))))

(defn ^:private analyze-defrecord*
  "Record-specific path — protocol implementations with keyword-based field access."
  [ctx form rec-type record-name constructor-fn-sym map-factory-sym
   field-set protocol-impls]
  (let [transform-bodies
        (fn [bodies]
          (mapv (fn [impl]
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
                        ~@body)))) bodies))
        protocol-impls
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
                 protocol (if (utils/var? protocol) @protocol protocol)]
             (if (and (map? protocol) (:marker-setter protocol))
               ;; native CLJS protocol, entry created by sci.core/copy-var on
               ;; a protocol: install on the record type's JS prototype
               (let [method-impls
                     (into {}
                           (map (fn [[method-name bodies]]
                                  (let [bodies (map rest bodies)
                                        arities (into #{} (map (comp count first)) bodies)
                                        bodies (transform-bodies bodies)]
                                    [(list 'quote (symbol (name method-name)))
                                     {:arities arities
                                      :impl `(fn ~@bodies)}])))
                           impls)]
                 [`(sci.impl.deftype/-install-native-protocol!
                    ~rec-type ~protocol-name ~method-impls)])
               (let [protocol-var (:var protocol)
                     _ (when protocol-var
                         (vars/alter-var-root protocol-var update :satisfies
                                              (fnil conj #{}) (str rec-type)))
                     protocol-ns (:ns protocol)
                     pns (cond protocol-ns (str (types/getName protocol-ns))
                               (= #?(:clj Object :cljs ::object) protocol) "sci.impl.records"
                               #?@(:clj [(= clojure.lang.IFn protocol) "clojure.lang"]))
                     fq-meth-name #(if (simple-symbol? %)
                                     (symbol pns (str %))
                                     %)]
                 (map (fn [[method-name bodies]]
                        (let [bodies (map rest bodies)
                              bodies (transform-bodies bodies)]
                          `(~'clojure.core/defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies)))
                      impls)))))
         protocol-impls)]
    (emit-record-type rec-type record-name constructor-fn-sym map-factory-sym
                      protocol-impls)))

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
        ;; The Type object is created here, at analysis time, so that method
        ;; bodies and factories referencing the type (e.g. (instance? Foo x))
        ;; resolve to the same object instances carry. -create-type adopts it
        ;; at eval time, adding the constructor var and (on CLJS) the JS
        ;; prototype.
        _ (swap! (:env ctx) update-in
                 [:namespaces (symbol (namespace tagged-name)) :types]
                 assoc record-name (lang/->Type {:sci.impl/type-name rec-type}))
        result
        (if record?
          ;; Record-specific: only type creation + protocol impls
          ;; (factory fns are in the macro expansion)
          (let [constructor-fn-sym (symbol (str "__" factory-fn-str "__ctor__"))
                map-factory-sym (symbol (str "map->" record-name))]
            (analyze-defrecord* ctx form rec-type record-name
                                constructor-fn-sym map-factory-sym
                                field-set protocol-impls))
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
             :default
             (let [transform-bodies
                   (fn [bodies]
                     (mapv (fn [impl]
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
                                   ~@body)))) bodies))
                   analyze-ctx (assoc ctx
                                      :deftype-fields field-set
                                      :local->mutator (zipmap field-set
                                                              (map (fn [field]
                                                                     (fn [this v]
                                                                       (types/-mutate this field v)))
                                                                   field-set)))
                   protocol-impls
                   (mapcat
                    (fn [[protocol-name & impls]]
                      (let [impls (group-by first impls)
                            protocol (or (when (= 'Object protocol-name)
                                           ::object)
                                         (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name))
                            _ (when-not protocol
                                (utils/throw-error-with-location
                                 (str "Protocol not found: " protocol-name)
                                 form))
                            protocol (if (utils/var? protocol) @protocol protocol)]
                        (if (and (map? protocol) (:marker-setter protocol))
                          ;; native CLJS protocol, entry created by sci.core/copy-var on a protocol
                          (let [method-impls
                                (into {}
                                      (map (fn [[method-name bodies]]
                                             (let [bodies (map rest bodies)
                                                   arities (into #{} (map (comp count first)) bodies)
                                                   bodies (transform-bodies bodies)]
                                               [(list 'quote (symbol (name method-name)))
                                                {:arities arities
                                                 :impl `(fn ~@bodies)}])))
                                      impls)]
                            [(@utils/analyze analyze-ctx
                              `(sci.impl.deftype/-install-native-protocol!
                                ~rec-type ~protocol-name ~method-impls))])
                          (let [protocol-var (:var protocol)
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
                                   (let [bodies (map rest bodies)
                                         bodies (transform-bodies bodies)]
                                     (@utils/analyze analyze-ctx
                                      `(~'clojure.core/defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies))))
                                 impls)))))
                    protocol-impls)]
               (emit-deftype rec-type record-name factory-fn-sym
                            `(defn ~(with-meta factory-fn-sym
                                      {:doc (str "Positional factory function for class " rec-type ".")})
                               ~fields
                               (sci.impl.deftype/->type-impl '~rec-type ~record-name ~record-name (zipmap ~(list 'quote fields) ~fields)))
                            protocol-impls))))]
    (if top-level?
      (types/->EvalForm result)
      (@utils/analyze ctx result))))

(defn deftype-macro
  "Macro expansion for deftype. Emits a (do (declare ->TypeName) (deftype* ...) (import ...))
   so that macroexpand reveals the constructor for static-analysis tools (e.g. Clerk).
   The declare is overwritten at analysis time by analyze-deftype*."
  [[_fname] _ record-name fields & raw-protocol-impls]
  (let [ns-name (utils/current-ns-name)
        tagged-name (symbol (str ns-name) (str record-name))
        class-name (symbol (str #?(:cljd (.replaceAll (str ns-name) "-" "_")
                                   :default (munge ns-name)) "." record-name))
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
