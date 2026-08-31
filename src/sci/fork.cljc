(ns sci.fork
  "Host cooperation for forkable SCI runtime values.")

(defprotocol Forkable
  "Values with an application-defined realization in a forked SCI world.

  `fork-value` is called at most once for each identical value during one
  fork. Return an independent realization to isolate it, return `this` to
  share an external resource deliberately, or throw to prohibit the fork."
  (fork-value [this]
    "Return this value's realization in the child world."))

#?(:clj
   (defn- memoized-forker [fallback]
     (let [memo (java.util.IdentityHashMap.)
           ;; Direct protocol implementations cover the normal deftype and
           ;; defrecord cases without paying satisfies?'s extension lookup for
           ;; every Var root. Preserve extend-type support when it is in use.
           extended? (boolean (seq (extenders Forkable)))]
       (fn [value]
         (if (or (instance? sci.fork.Forkable value)
                 (and extended? (satisfies? Forkable value)))
           (if (.containsKey memo value)
             (.get memo value)
             (let [forked (fork-value value)]
               (.put memo value forked)
               forked))
           (if fallback (fallback value) value)))))
   :cljs
   (defn- memoized-forker [fallback]
     (let [memo (js/Map.)]
       (fn [value]
         (if (satisfies? Forkable value)
           (if (.has memo value)
             (.get memo value)
             (let [forked (fork-value value)]
               (.set memo value forked)
               forked))
           (if fallback (fallback value) value)))))
   :cljd
   (defn- memoized-forker [fallback]
     ;; ClojureDart has no portable identity-map abstraction. Forkable host
     ;; resources are expected to be few, so keep a small identity alist.
     (let [memo (volatile! [])
           missing (Object.)]
       (fn [value]
         (if (satisfies? Forkable value)
           (let [cached (reduce (fn [_ [source forked]]
                                  (if (identical? source value)
                                    (reduced forked)
                                    missing))
                                missing
                                @memo)]
             (if (identical? missing cached)
               (let [forked (fork-value value)]
                 (vswap! memo conj [value forked])
                 forked)
               cached))
           (if fallback (fallback value) value))))))

(defn ^:no-doc value-forker
  "Create the value transformer for one SCI fork."
  [fallback]
  (memoized-forker fallback))
