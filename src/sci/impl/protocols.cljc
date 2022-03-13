(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend-protocol
                            extend extend-type reify satisfies?
                            extends? implements?])
  (:require [sci.impl.multimethods :as mms]
            #?(:clj [sci.impl.interop :as interop])
            [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn default? [#?(:clj ctx
                   :cljs _ctx) sym]
  #?(:clj (and (or (= 'Object sym)
                   (= 'java.lang.Object type))
               (= Object (interop/resolve-class ctx 'Object)))
     :cljs (or (= 'object sym)
               (= 'default sym))))

(defn ->sigs [signatures]
  (into {}
        (map (fn [[name & arglists]]
               (let [l (last arglists)
                     [arglists doc] (if (string? l)
                                      [(butlast arglists) l]
                                      [arglists nil])]
                 [(keyword name) {:name name :arglists arglists :doc doc}])) signatures)))

(defn defprotocol [_ _ _ctx protocol-name & signatures]
  (let [[docstring signatures]
        (let [sig (first signatures)]
          (if (string? sig) [sig (rest signatures)]
              [nil signatures]))
        [opts signatures]
        (let [opt (first signatures)]
          (if (keyword? opt) [{opt (second signatures)} (nnext signatures)]
              [nil signatures]))
        sigs-map (->sigs signatures)
        current-ns (str (vars/current-ns-name))
        fq-name (symbol current-ns (str protocol-name))
        extend-meta (:extend-via-metadata opts)
        expansion
        `(do
           (def  ~(with-meta protocol-name
                    {:doc docstring}) (cond->
                                          {:methods #{}
                                           :name '~fq-name
                                           :ns *ns*
                                           :sigs ~(list 'quote sigs-map)}
                                        ~extend-meta (assoc :extend-via-metadata true)))
           ~@(map (fn [[method-name & _]]
                    (let [fq-name (symbol (str current-ns) (str method-name))
                          impls [`(defmulti ~method-name clojure.core/protocol-type-impl)
                                 `(defmethod ~method-name :sci.impl.protocols/reified [x# & args#]
                                    (let [methods# (clojure.core/-reified-methods x#)]
                                      (if-let [m# (get methods# '~method-name)]
                                        (apply m# x# args#)
                                        (if-let [default# (get-method ~method-name :default)]
                                          (apply default# x# args#)
                                          (throw (ex-info "No method " '~method-name " found for: " (type x#)))))))]
                          impls (if extend-meta
                                  (conj impls
                                        `(defmethod ~method-name :default [x# & args#]
                                           (let [meta# (meta x#)
                                                 method# (get meta# '~fq-name)]
                                             (if method#
                                               (apply method# x# args#)
                                               (let [method# (get-method ~method-name (#?(:clj class :cljs type) x#))
                                                     default# (get-method ~method-name :default)]
                                                 (if (not= method# default#)
                                                   (apply method# x# args#)
                                                   (throw (new #?(:clj IllegalArgumentException
                                                                  :cljs js/Error)
                                                               (str "No implementation of method: "
                                                                    ~(keyword method-name) " of protocol: "
                                                                    (var ~protocol-name) " found for: "
                                                                    (clojure.core/protocol-type-impl x#))))))))))
                                  (conj impls
                                        ;; fallback method for extension on IRecord
                                        `(defmethod ~method-name :default [x# & args#]
                                           (let [method# (get-method ~method-name (#?(:clj class :cljs type) x#))
                                                 default# (get-method ~method-name :default)]
                                             (if (not= method# default#)
                                               (apply method# x# args#)
                                               (throw (new #?(:clj IllegalArgumentException
                                                              :cljs js/Error)
                                                           (str "No implementation of method: "
                                                                ~(keyword method-name) " of protocol: "
                                                                (var ~protocol-name) " found for: "
                                                                (clojure.core/protocol-type-impl x#)))))))))]
                      `(do
                         ~@impls
                         #?(:clj (alter-var-root (var ~protocol-name)
                                                 update :methods conj ~method-name)
                            :cljs (def ~protocol-name
                                    (update ~protocol-name :methods conj ~method-name))))))
                  signatures))]
    expansion))

;; TODO: apply patches for default override for records
(defn extend [ctx atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)
          :let [extend-via-metadata (:extend-via-metadata proto)
                proto-ns (:ns proto)
                pns (vars/getName proto-ns)
                pns-str (when extend-via-metadata (str pns))]]
    (doseq [[meth-name f] mmap]
      (let [meth-str (name meth-name)
            meth-sym (symbol meth-str)
            env @(:env ctx)
            multi-method-var (get-in env [:namespaces pns meth-sym])
            multi-method @multi-method-var]
        (mms/multi-fn-add-method-impl
         multi-method atype
         (if extend-via-metadata
           (let [fq (symbol pns-str meth-str)]
             (fn [this & args]
               (if-let [m (meta this)]
                 (if-let [meth (get m fq)]
                   (apply meth this args)
                   (apply f this args))
                 (apply f this args))))
           f))))))

(defn process-single-extend-meta
  "Processes single args+body pair for extending via metadata"
  [fq [args & body] default-method?]
  (list args (if default-method?
               `(let [farg# ~(first args)]
                  (if-let [m# (meta farg#)]
                    (if-let [meth# (get m# '~fq)]
                      (apply meth# ~args)
                      ;; look for type specific method
                      (let [meth# (get-method ~fq (#?(:clj class :cljs type) farg#))
                            default# (get-method ~fq :default)]
                        (if (not= default# meth#)
                          (apply meth# ~args)
                          (do ~@body))))
                    (let [meth# (get-method ~fq (#?(:clj class :cljs type) farg#))
                          default# (get-method ~fq :default)]
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
                (let [meth# (get-method ~fq (#?(:clj class :cljs type) farg#))
                      default# (get-method ~fq :default)]
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
           `(defmethod ~fq
              :default
              ~@fn-body)
           `(defmethod ~fq
              ~type
              ~@fn-body))))
     meths)))

(defn extend-protocol [_ _ ctx protocol-name & impls]
  (let [impls (utils/split-when #(not (seq? %)) impls)
        protocol-var (@utils/eval-resolve-state ctx (:bindingx ctx) protocol-name)
        protocol-data (deref protocol-var)
        extend-via-metadata (:extend-via-metadata protocol-data)
        protocol-ns (:ns protocol-data)
        pns (str (vars/getName protocol-ns))
        expansion
        `(do ~@(map (fn [[type & meths]]
                      `(do
                         ~@(process-methods ctx type meths pns extend-via-metadata)))
                    impls))]
    expansion))

(defn extend-type [_ _ ctx atype & proto+meths]
  (let [proto+meths (utils/split-when #(not (seq? %)) proto+meths)]
    `(do ~@(map
            (fn [[proto & meths]]
              (let [protocol-var (@utils/eval-resolve-state ctx (:bindings ctx) proto)
                    proto-data (deref protocol-var)
                    protocol-ns (:ns proto-data)
                    pns (str (vars/getName protocol-ns))
                    extend-via-metadata (:extend-via-metadata proto-data)]
                `(do
                   ~@(process-methods ctx atype meths pns extend-via-metadata)))) proto+meths))))

;; IAtom can be implemented as a protocol on reify and defrecords in sci

(defn find-matching-non-default-method [protocol obj]
  (boolean (some #(when-let [m (get-method % (types/type-impl obj))]
                    (let [ms (methods %)
                          default (get ms :default)]
                      (not (identical? m default))))
                 (:methods protocol))))

(defn satisfies? [protocol obj]
  (if #?(:clj (instance? sci.impl.types.IReified obj)
         ;; in CLJS we currently don't support mixing "classes" and protocols,
         ;; hence, the instance is always a Reified, thus we can avoid calling
         ;; the slower satisfies?
         :cljs (instance? sci.impl.types/Reified obj))
    (contains? (types/getProtocols obj) protocol)
    ;; can be record that is implementing this protocol
    ;; or a type like String, etc. that implements a protocol via extend-type, etc.
    #?(:cljs (let [p (:protocol protocol)]
               (or
                (and p
                     (condp = p
                       IDeref (cljs.core/satisfies? IDeref obj)
                       ISwap (cljs.core/satisfies? ISwap obj)
                       IReset (cljs.core/satisfies? IReset obj)))
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
    ;; records are currently represented as a symbol with metadata
    (and (symbol? clazz) (some-> clazz meta :sci.impl/record))
    (= clazz (-> x meta :type))
    ;; only in Clojure, we could be referring to clojure.lang.IDeref as a sci protocol
    #?@(:clj [(map? clazz)
              (if-let [c (:class clazz)]
                ;; this is a protocol which is an interface on the JVM
                (or (satisfies? clazz x)
                    ;; this is the fallback because we excluded defaults for the core protocols
                    (instance? c x))
                (satisfies? clazz x))])
    ;; could we have a fast path for CLJS too? please let me know!
    :else (instance? clazz x)))

(defn extends?
  "Returns true if atype extends protocol"
  [protocol atype]
  (boolean (some #(get-method % atype) (:methods protocol))))
