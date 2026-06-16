(ns sci.interrupt
  "Opt-in, interrupt-fn aware overrides for core functions that iterate over
  host sequences without ever entering an interpreted function, and therefore
  never hit the per-fn-entry `:interrupt-fn` check.

  This namespace is NOT loaded by default and adds nothing to a standard SCI
  build unless you require it. Merge `clojure-core` into your context's
  `clojure.core` namespace to make these functions interruptible:

      (require '[sci.core :as sci]
               '[sci.interrupt :as interrupt])

      (sci/init {:interrupt-fn my-interrupt-fn
                 :namespaces {'clojure.core interrupt/clojure-core}})

  Each override reads `:interrupt-fn` from the current context at call time and
  falls back to the native function when no `:interrupt-fn` is configured, so
  merging `clojure-core` is always safe."
  (:require
   [sci.ctx-store :as store]
   [sci.impl.copy-vars :as copy-vars]))

(defn get-interrupt-fn
  "Returns the `:interrupt-fn` configured on `ctx` (as produced by `sci/init`),
  or nil when none was set. Use this from host functions you expose to make them
  interrupt-fn aware, e.g. `(get-interrupt-fn (sci.ctx-store/get-ctx))`."
  [ctx]
  (:interrupt-fn ctx))

;;; Producers - lazy sequences that fire interrupt-fn on each step

(defn- range-seq [start end step ifn]
  (let [pred (cond
               (nil? end)   (constantly true)
               (pos? step)  #(< % end)
               (neg? step)  #(> % end)
               :else        (constantly false))]
    (letfn [(gen [i]
              (lazy-seq
                (when (pred i)
                  (ifn)
                  (cons i (gen (+ i step))))))]
      (gen start))))

(defn- sci-range
  ([]    (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (range-seq 0 nil 1 ifn) (range))))
  ([end] (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (range-seq 0 end 1 ifn) (range end))))
  ([start end]
   (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (range-seq start end 1 ifn) (range start end))))
  ([start end step]
   (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (range-seq start end step ifn) (range start end step)))))

(defn- sci-repeat
  ([x]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (repeat x)
       (letfn [(gen [] (lazy-seq (ifn) (cons x (gen))))]
         (gen)))))
  ([n x]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (repeat n x)
       (letfn [(gen [i]
                 (lazy-seq
                   (when (pos? i)
                     (ifn)
                     (cons x (gen (dec i))))))]
         (gen n))))))

(defn- sci-cycle [coll]
  (let [ifn (get-interrupt-fn (store/get-ctx))]
    (if-not ifn
      (cycle coll)
      (when (seq coll)
        (letfn [(gen [s]
                  (lazy-seq
                    (ifn)
                    (let [cur (or (seq s) (seq coll))]
                      (cons (first cur) (gen (rest cur))))))]
          (gen coll))))))

(defn- sci-iterate [f x]
  (let [ifn (get-interrupt-fn (store/get-ctx))]
    (if-not ifn
      (iterate f x)
      (letfn [(gen [v]
                (lazy-seq
                  (ifn)
                  (cons v (gen (f v)))))]
        (gen x)))))

;;; Materializers - consuming functions that fire interrupt-fn per element

(defn- sci-dorun
  ([coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (dorun coll)
       (loop [s (seq coll)]
         (when s
           (ifn)
           (recur (next s)))))))
  ([n coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (dorun n coll)
       (loop [s (seq coll) i 0]
         (when (and s (< i n))
           (ifn)
           (recur (next s) (inc i))))))))

(defn- sci-doall
  ([coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (doall coll)
       (do (loop [s (seq coll)]
             (when s (ifn) (recur (next s))))
           coll))))
  ([n coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (doall n coll)
       (do (loop [s (seq coll) i 0]
             (when (and s (< i n)) (ifn) (recur (next s) (inc i))))
           coll)))))

(defn- sci-count [coll]
  (let [ifn (get-interrupt-fn (store/get-ctx))]
    (if (or (not ifn) (counted? coll))
      (count coll)
      (loop [s (seq coll) n 0]
        (if s
          (do (ifn) (recur (next s) (inc n)))
          n)))))

(defn- sci-into
  ([to from]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (into to from)
       (reduce (fn [acc x] (ifn) (conj acc x)) to from))))
  ([to xf from]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (into to xf from)
       (transduce (comp (map (fn [x] (ifn) x)) xf) conj to from)))))

(defn- sci-reduce
  ([f coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))
         s   (seq coll)]
     (if-not ifn
       (reduce f coll)
       (if s
         (loop [v (first s) s (next s)]
           (if s
             (do (ifn)
                 (let [ret (f v (first s))]
                   (if (reduced? ret) @ret (recur ret (next s)))))
             v))
         (f)))))
  ([f init coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (reduce f init coll)
       (loop [v init s (seq coll)]
         (if s
           (do (ifn)
               (let [ret (f v (first s))]
                 (if (reduced? ret) @ret (recur ret (next s)))))
           v))))))

(def clojure-core
  "Map of `clojure.core` symbol -> interrupt-fn aware replacement var. Each value
  is a real SCI var on the shared `clojure.core` namespace object (built via
  `copy-vars/new-var`, like the built-in core vars), so it carries proper
  `:ns`/`:name`/`:sci/built-in` metadata. Merge into `{:namespaces {'clojure.core ...}}`."
  {'range   (copy-vars/new-var 'range   sci-range   true)
   'repeat  (copy-vars/new-var 'repeat  sci-repeat  true)
   'cycle   (copy-vars/new-var 'cycle   sci-cycle   true)
   'iterate (copy-vars/new-var 'iterate sci-iterate true)
   'doall   (copy-vars/new-var 'doall   sci-doall   true)
   'dorun   (copy-vars/new-var 'dorun   sci-dorun   true)
   'count   (copy-vars/new-var 'count   sci-count   true)
   'into    (copy-vars/new-var 'into    sci-into    true)
   'reduce  (copy-vars/new-var 'reduce  sci-reduce  true)})
