(ns sci.fork
  "Host cooperation for forkable SCI runtime values.")

(defprotocol Forkable
  "Values with an application-defined realization in a forked SCI world.

  `fork-value` is called at most once for each identical value during one
  fork. Return an independent realization to isolate it, return `this` to
  share an external resource deliberately, or throw to prohibit the fork."
  (fork-value [this]
    "Return this value's realization in the child world."))

(defn- transient-value? [value]
  #?(:clj (instance? clojure.lang.ITransientCollection value)
     :cljs (satisfies? ITransientCollection value)
     :cljd (satisfies? ITransientCollection value)))

(defn- unmanaged-mutable-value? [value]
  #?(:clj (or (instance? clojure.lang.Atom value)
              (instance? clojure.lang.Ref value)
              (instance? clojure.lang.Agent value)
              (instance? clojure.lang.Volatile value)
              (instance? clojure.lang.MultiFn value))
     :cljs (or (instance? cljs.core/Atom value)
               (instance? cljs.core/Volatile value)
               (instance? cljs.core/MultiFn value))
     ;; Portable concrete runtime types for these hosts still need auditing.
     :cljd false))

(defn- default-fork-value [value]
  (cond
    (transient-value? value)
    (throw (ex-info
            "Cannot fork a SCI world containing an affine transient collection. Provide :fork-fn to define an explicit policy."
            {:type ::affine-resource
             :resource value}))

    (unmanaged-mutable-value? value)
    (throw (ex-info
            "Cannot fork a SCI world containing an unmanaged mutable host value. Implement Forkable or provide :fork-fn to define an explicit policy."
            {:type ::unmanaged-mutable-value
             :resource value}))

    #?(:clj (and (some? value) (.isArray ^Class (class value)))
       :cljs (array? value)
       :cljd false)
    #?(:clj (aclone value)
       :cljs (.slice value)
       :cljd value)

    :else value))

#?(:clj
   (defn- memoized-forker [fallback]
     (let [memo (java.util.IdentityHashMap.)
           ;; Direct protocol implementations cover the normal deftype and
           ;; defrecord cases without paying satisfies?'s extension lookup for
           ;; every Var root. Preserve extend-type support when it is in use.
           extended? (boolean (seq (extenders Forkable)))]
       (fn [value]
         (if (.containsKey memo value)
           (.get memo value)
           (let [forked
                 (if (or (instance? sci.fork.Forkable value)
                         (and extended? (satisfies? Forkable value)))
                   (fork-value value)
                   (if fallback
                     (fallback value)
                     (default-fork-value value)))]
             (.put memo value forked)
             forked)))))
   :cljs
   (defn- memoized-forker [fallback]
     (let [memo (js/Map.)]
       (fn [value]
         (if (.has memo value)
           (.get memo value)
           (let [forked (if (satisfies? Forkable value)
                          (fork-value value)
                          (if fallback
                            (fallback value)
                            (default-fork-value value)))]
             (.set memo value forked)
             forked)))))
   :cljd
   (defn- memoized-forker [fallback]
     ;; ClojureDart has no portable identity-map abstraction. Forkable host
     ;; resources are expected to be few, so keep a small identity alist.
     (let [memo (volatile! [])
           missing (Object.)]
       (fn [value]
         (let [cached (reduce (fn [_ [source forked]]
                                (if (identical? source value)
                                  (reduced forked)
                                  missing))
                              missing
                              @memo)]
           (if (identical? missing cached)
             (let [forked (if (satisfies? Forkable value)
                            (fork-value value)
                            (if fallback
                              (fallback value)
                              (default-fork-value value)))]
               (vswap! memo conj [value forked])
               forked)
             cached))))))

(defn ^:no-doc value-forker
  "Create the value transformer for one SCI fork."
  [fallback]
  (memoized-forker fallback))
