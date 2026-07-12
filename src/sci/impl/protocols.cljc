(ns sci.impl.protocols
  {:no-doc true}
  ;; cljd emits reify for variadic fns, excluding it breaks compilation
  (:refer-clojure :exclude [defprotocol extend-protocol
                            extend extend-type
                            #?@(:cljd [] :default [reify])
                            satisfies?
                            extends? implements? type->str])
  (:require
   [sci.ctx-store :as store]
   #?(:clj [sci.impl.interop :as interop])
   [sci.impl.core-protocols :as core-protocols]
   [sci.impl.deftype]
   [sci.impl.multimethods :as mms]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   #?(:cljd [sci.lang :as lang] :default [sci.lang])))

#?(:cljs
   (def extend-default-val (str `default)))

(defn default? [#?(:cljd _ctx
                   :clj ctx
                   :cljs _ctx) sym]
  #?(:cljd (= 'Object sym)
     :clj (and (or (= 'Object sym)
                   (= 'java.lang.Object type))
               (= Object (interop/resolve-class ctx 'Object)))
     :cljs (= extend-default-val sym)))

(defn ->sigs [signatures]
  (into {}
        (map (fn [[name & arglists]]
               (let [l (last arglists)
                     [arglists doc] (if (string? l)
                                      [(butlast arglists) l]
                                      [arglists nil])]
                 [(keyword name) {:name name :arglists arglists :doc doc}])) signatures)))

(defn defprotocol [_ _ protocol-name & signatures]
  (let [[docstring signatures]
        (let [sig (first signatures)]
          (if (string? sig) [sig (rest signatures)]
              [nil signatures]))
        [opts signatures]
        (let [opt (first signatures)]
          (if (keyword? opt) [{opt (second signatures)} (nnext signatures)]
              [nil signatures]))
        sigs-map (->sigs signatures)
        current-ns (str (utils/current-ns-name))
        fq-protocol-name (symbol current-ns (str protocol-name))
        extend-meta (:extend-via-metadata opts)
        expansion
        `(do
           (def  ~(with-meta protocol-name
                    {:doc docstring}) (cond->
                                          {:methods #{}
                                           :name '~fq-protocol-name
                                           :ns *ns*
                                           :sigs ~(list 'quote sigs-map)
                                           :var (var ~fq-protocol-name)}
                                        ~extend-meta (assoc :extend-via-metadata true)))
           ~@(map (fn [[method-name & _]]
                    (let [fq-name (symbol current-ns (str method-name))
                          method-meta (select-keys (get sigs-map (keyword method-name)) [:doc :arglists])
                          method-meta (assoc method-meta :protocol (list 'var fq-protocol-name))
                          ; re-quote arglists
                          method-meta (update method-meta :arglists (fn [a] (list 'quote a)))
                          impls [`(~'clojure.core/defmulti ~method-name ~method-meta clojure.core/protocol-type-impl)
                                 `(~'clojure.core/defmethod ~method-name :sci.impl.protocols/reified [x# & args#]
                                    (let [methods# (clojure.core/-reified-methods x#)]
                                      (if-let [m# (get methods# '~method-name)]
                                        (apply m# x# args#)
                                        (if-let [default# (~'clojure.core/get-method ~method-name :default)]
                                          (apply default# x# args#)
                                          (throw (ex-info "No method " '~method-name " found for: " (~'clojure.core/type x#)))))))]
                          impls (if extend-meta
                                  (conj impls
                                        `(~'clojure.core/defmethod ~method-name :default [x# & args#]
                                           (let [meta# (meta x#)
                                                 method# (get meta# '~fq-name)]
                                             (if method#
                                               (apply method# x# args#)
                                               (let [method# (~'clojure.core/get-method ~method-name (#?(:cljd ~'clojure.core/type :clj class :cljs type) x#))
                                                     default# (~'clojure.core/get-method ~method-name :default)]
                                                 (if (not= method# default#)
                                                   (apply method# x# args#)
                                                   (throw (new #?(:cljd ~'Exception
                                                                  :clj IllegalArgumentException
                                                                  :cljs js/Error)
                                                               (str "No implementation of method: "
                                                                    ~(keyword method-name) " of protocol: "
                                                                    (var ~protocol-name) " found for: "
                                                                    (clojure.core/protocol-type-impl x#))))))))))
                                  (conj impls
                                        ;; fallback method for extension on IRecord
                                        `(~'clojure.core/defmethod ~method-name :default [x# & args#]
                                           (let [method# (~'clojure.core/get-method ~method-name (#?(:cljd ~'clojure.core/type :clj class :cljs type) x#))
                                                 default# (~'clojure.core/get-method ~method-name :default)]
                                             (if (not= method# default#)
                                               (apply method# x# args#)
                                               (throw (new #?(:cljd ~'Exception
                                                              :clj IllegalArgumentException
                                                              :cljs js/Error)
                                                           (str "No implementation of method: "
                                                                ~(keyword method-name) " of protocol: "
                                                                ~(list 'var fq-protocol-name) " found for: "
                                                                (clojure.core/protocol-type-impl x#)))))))))]
                      `(do
                         ~@impls
                         (clojure.core/alter-var-root (var ~protocol-name)
                                                    update :methods conj ~method-name))))
                  signatures
                  )
           ~(list 'quote protocol-name))]
    expansion))

#?(:cljs
   (defn -extend-native!
     "Extends native CLJS protocol (entry created by sci.core/copy-var on a protocol)
  to a sci type by installing the method impls on its JS prototype."
     [atype proto-map impls]
     (when-not (instance? sci.lang/Type atype)
       (throw (js/Error. (str "Protocol " (:name proto-map)
                              " can only be extended natively to types created with deftype or defrecord in sci"))))
     (sci.impl.deftype/-install-native-protocol! atype proto-map impls)))

(defn ^:private native-protocol? [proto-data]
  (boolean (and (map? proto-data) (:marker-setter proto-data))))

(defn ^:private native-method-impls
  "Builds the impls map consumed by -install-native-protocol! from
  extend-type/extend-protocol method bodies."
  [meths]
  (into {}
        (map (fn [[meth-name & fn-body]]
               (let [bodies (if (vector? (first fn-body)) [fn-body] fn-body)
                     arities (into #{} (map (comp count first)) bodies)]
                 [(list 'quote (symbol (name meth-name)))
                  {:arities arities
                   :impl `(fn ~@bodies)}])))
        meths))

;; TODO: apply patches for default override for records
(defn extend [atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)]
    (if (native-protocol? proto)
      ;; native CLJS protocol: install on the sci type's JS prototype, at
      ;; every arity the protocol declares for each given method
      #?(:cljs (-extend-native!
                atype proto
                (into {}
                      (map (fn [[meth-name f]]
                             (let [msym (symbol (name meth-name))]
                               [msym {:arities (set (keys (:setters (get (:native-methods proto) msym))))
                                      :impl f}])))
                      mmap))
         :default nil)
      (let [extend-via-metadata (:extend-via-metadata proto)
            proto-ns (:ns proto)
            pns (types/getName proto-ns)
            pns-str (when extend-via-metadata (str pns))]
        (doseq [[meth-name f] mmap]
          (let [meth-str (name meth-name)
                meth-sym (symbol meth-str)
                env (deref (:env (store/get-ctx)))
                multi-method-var (get-in env [:namespaces pns meth-sym])
                multi-method @multi-method-var]
            (mms/multi-fn-add-method-impl
             multi-method #?(:cljd (if (identical? Object atype) :default atype)
                             :default atype)
             (if extend-via-metadata
               (let [fq (symbol pns-str meth-str)]
                 (fn [this & args]
                   (if-let [m (meta this)]
                     (if-let [meth (get m fq)]
                       (apply meth this args)
                       (apply f this args))
                     (apply f this args))))
               f))))))))

(defn process-single-extend-meta
  "Processes single args+body pair for extending via metadata"
  [fq [args & body] default-method?]
  (list args (if default-method?
               `(let [farg# ~(first args)]
                  (if-let [m# (meta farg#)]
                    (if-let [meth# (get m# '~fq)]
                      (apply meth# ~args)
                      ;; look for type specific method
                      (let [meth# (~'clojure.core/get-method ~fq (#?(:cljd ~'clojure.core/type :clj class :cljs type) farg#))
                            default# (~'clojure.core/get-method ~fq :default)]
                        (if (not= default# meth#)
                          (apply meth# ~args)
                          (do ~@body))))
                    (let [meth# (~'clojure.core/get-method ~fq (#?(:cljd ~'clojure.core/type :clj class :cljs type) farg#))
                          default# (~'clojure.core/get-method ~fq :default)]
                      (if (not= default# meth#)
                        (apply meth# ~args)
                        (do ~@body)))))
               `(let [farg# ~(first args)]
                  (if-let [m# (meta farg#)]
                    (if-let [meth# (get m# '~fq)]
                      (apply meth# ~args)
                      (do ~@body))
                    (do ~@body))))))

(defn process-single
  [fq [args & body]]
  (list args `(let [farg# ~(first args)]
                (let [meth# (~'clojure.core/get-method ~fq (#?(:cljd ~'clojure.core/type :clj class :cljs type) farg#))
                      default# (~'clojure.core/get-method ~fq :default)]
                  (if (not= default# meth#)
                    (apply meth# ~args)
                    (do ~@body))))))

(defn process-methods [ctx type meths protocol-ns extend-via-metadata]
  (let [default-method? (default? ctx type)]
    (map
     (fn [[meth-name & fn-body]]
       (let [fq (symbol protocol-ns (name meth-name))
             fn-body (cond extend-via-metadata
                           (if (vector? (first fn-body))
                             (process-single-extend-meta fq fn-body default-method?)
                             (map #(process-single-extend-meta fq % default-method?) fn-body))
                           default-method?
                           (if (vector? (first fn-body))
                             (process-single fq fn-body)
                             (map #(process-single fq %) fn-body))
                           :else fn-body)]
         (if default-method?
           `(~'clojure.core/defmethod ~fq
              :default
              ~@fn-body)
           `(~'clojure.core/defmethod ~fq
              ~type
              ~@fn-body))))
     meths)))


#?(:cljs
   (def cljs-type-symbols
     {'default extend-default-val
      'object 'js/Object
      'string 'js/String
      'number 'js/Number
      'array 'js/Array
      'function 'js/Function
      'boolean 'js/Boolean}))

(defn type->str
  [t]
  (cond (nil? t) "nil"
        (and (map? t) (:class t)) (str (:class t))
        :else (str t)))

(defn extend-protocol [form _ protocol-name & impls]
  (let [ctx (store/get-ctx)
        impls (utils/split-when #(not (seq? %)) impls)
        protocol-var
        (or (@utils/eval-resolve-state ctx (:bindingx ctx) protocol-name)
            (utils/throw-error-with-location (str "Protocol not found: " protocol-name) form))
        protocol-data (if (utils/var? protocol-var)
                        (deref protocol-var)
                        protocol-var)
        extend-via-metadata (:extend-via-metadata protocol-data)
        native? (native-protocol? protocol-data)
        protocol-ns (:ns protocol-data)
        pns (str (types/getName protocol-ns))
        expansion
        `(do
           ~@(map (fn [[type & meths]]
                    (let [type #?(:cljd type
                                  :clj type
                                  :cljs (get cljs-type-symbols type type))]
                      (if native?
                        `(sci.impl.protocols/-extend-native!
                          ~type ~protocol-name ~(native-method-impls meths))
                        `(do
                           (clojure.core/alter-var-root
                            (var ~protocol-name) update :satisfies (fnil conj #{})
                            (type->str ~type))
                           ~@(process-methods ctx type meths pns extend-via-metadata)))))
                  impls))]
    expansion))

(defn extend-type [form _env atype & proto+meths]
  (let [ctx (store/get-ctx)
        #?@(:cljs [atype (get cljs-type-symbols atype atype)])
        proto+meths (utils/split-when #(not (seq? %)) proto+meths)]
    `(do ~@(map
            (fn [[proto & meths]]
              (let [protocol-var (or (@utils/eval-resolve-state ctx (:bindingx ctx) proto)
                                     (utils/throw-error-with-location (str "Protocol not found: " proto) form))
                    proto-data (if (utils/var? protocol-var)
                                 (deref protocol-var)
                                 protocol-var)]
                (if (native-protocol? proto-data)
                  `(sci.impl.protocols/-extend-native!
                    ~atype ~proto ~(native-method-impls meths))
                  (let [protocol-ns (:ns proto-data)
                        pns (str (types/getName protocol-ns))
                        extend-via-metadata (:extend-via-metadata proto-data)]
                    `(do
                       (clojure.core/alter-var-root
                        (var ~proto) update :satisfies (fnil conj #{})
                        (type->str ~atype))
                       ~@(process-methods ctx atype meths pns extend-via-metadata))))))
            proto+meths))))

;; IAtom can be implemented as a protocol on reify and defrecords in sci

(defn find-matching-non-default-method [protocol obj]
  (or (when-let [sats (:satisfies protocol)]
        (or
         #?(:cljd (contains? sats "Object")
            :clj (contains? sats "class java.lang.Object")
            :cljs (contains? sats extend-default-val))
         (when (nil? obj)
           (contains? sats "nil"))
         (when-let [t (types/type-impl obj)]
           (contains? sats (type->str t)))))
      ;; built-in protocol :methods are host multifns on cljd, only
      ;; sci-created SciMultiFns are inspectable
      #?(:cljd
         (boolean (some #(when (instance? mms/SciMultiFn %)
                           (when-let [m (mms/get-method-impl % (types/type-impl obj))]
                             (let [ms (mms/methods-impl %)
                                   default (get ms :default)]
                               (not (identical? m default)))))
                        (:methods protocol)))
         :default
         (boolean (some #(when-let [m (get-method % (types/type-impl obj))]
                           (let [ms (methods %)
                                 default (get ms :default)]
                             (not (identical? m default))))
                        (:methods protocol))))))

(defn satisfies? [protocol obj]
  (if-let [protocols (when #?(:cljd (instance? sci.impl.types/Reified obj)
                              :clj (instance? sci.impl.types.ICustomType obj)
                              ;; in CLJS we currently don't support mixing "classes" and protocols,
                              ;; hence, the instance is always a Reified, thus we can avoid calling
                              ;; the slower satisfies?
                              :cljs (instance? sci.impl.types/Reified obj))
                       (types/getProtocols obj))]
    (contains? protocols protocol)
    ;; can be record that is implementing this protocol
    ;; or a type like String, etc. that implements a protocol via extend-type, etc.
    #?(:cljd (let [p (:protocol protocol)]
               (or
                (and p
                     (condp = p
                       IDeref (clojure.core/satisfies? IDeref obj)
                       ISwap (clojure.core/satisfies? ISwap obj)
                       IReset (clojure.core/satisfies? IReset obj)
                       IRecord (clojure.core/satisfies? IRecord obj)
                       IFn (core-protocols/sci-ifn? obj)
                       nil))
                (find-matching-non-default-method protocol obj)))
       :cljs (let [p (:protocol protocol)]
               (or
                ;; native protocol entry created by sci.core/copy-var on a
                ;; protocol; IDeref, ISwap, IReset and IPrintWithWriter are
                ;; such entries since ADR 0012
                (when-let [sf (:satisfies-fn protocol)]
                  (sf obj))
                (and p
                     (condp = p
                       IRecord (cljs.core/satisfies? IRecord obj)
                       IFn (core-protocols/sci-ifn? obj)
                       nil))
                (find-matching-non-default-method protocol obj)))
       ;; NOTE: what if the protocol doesn't have any methods?
       ;; This probably needs fixing
       :clj (or
             (when-let [p (:protocol protocol)]
               (clojure.core/satisfies? p obj))
             (find-matching-non-default-method protocol obj)))))

(defn instance-impl [clazz x]
  (cond
    ;; fast path for Clojure when using normal clazz
    #?@(:clj [(class? clazz)
              (instance? clazz x)])
    (utils/sci-type? clazz)
    (if #?(:cljd (clojure.core/satisfies? sci.impl.types/SciTypeInstance x)
           :clj (instance? sci.impl.types.SciTypeInstance x)
           :cljs (cljs.core/implements? sci.impl.types.SciTypeInstance x))
      (= clazz (sci.impl.types/-get-type x))
      (= clazz (-> x meta :type)))
    ;; only in Clojure, we could be referring to clojure.lang.IDeref as a sci protocol
    (map? clazz)
    ;; cljd class registry entries are maps with an :instance? closure
    #?(:cljd (if-let [ip (:instance? clazz)]
               (ip x)
               (satisfies? clazz x))
       :clj (if-let [c (:class clazz)]
              ;; this is a protocol which is an interface on the JVM
              (or (satisfies? clazz x)
                  ;; this is the fallback because we excluded defaults for the core protocols
                  (instance? c x))
              (satisfies? clazz x))
       :cljs (satisfies? clazz x))
    ;; could we have a fast path for CLJS too? please let me know!
    :else #?(:cljd (if (dart/is? clazz Type)
                     ;; Object matches any non-nil, other Types match by exact
                     ;; runtimeType, Dart has no runtime subtype check
                     (if (identical? clazz Object)
                       (some? x)
                       (= clazz (.-runtimeType x)))
                     (throw (ex-info (str clazz " is not a class") {})))
             :clj (instance? clazz x)
             :cljs (instance? clazz x))))

(defn extends?
  "Returns true if atype extends protocol"
  [protocol atype]
  #?(:cljd (boolean (some #(when (instance? mms/SciMultiFn %)
                             (mms/get-method-impl % atype))
                          (:methods protocol)))
     :default (boolean (some #(get-method % atype) (:methods protocol)))))
