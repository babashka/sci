(ns sci.impl.interruptible
  {:no-doc true}
  (:require
   [sci.ctx-store :as store]
   [sci.impl.utils :as utils]))

(defn- get-ifn []
  (:interrupt-fn (store/get-ctx)))

;;; Producers — lazy sequences that fire interrupt-fn on each step

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
  ([]    (let [ifn (get-ifn)] (if ifn (range-seq 0 nil 1 ifn) (range))))
  ([end] (let [ifn (get-ifn)] (if ifn (range-seq 0 end 1 ifn) (range end))))
  ([start end]
   (let [ifn (get-ifn)] (if ifn (range-seq start end 1 ifn) (range start end))))
  ([start end step]
   (let [ifn (get-ifn)] (if ifn (range-seq start end step ifn) (range start end step)))))

(defn- sci-repeat
  ([x]
   (let [ifn (get-ifn)]
     (if-not ifn
       (repeat x)
       (letfn [(gen [] (lazy-seq (ifn) (cons x (gen))))]
         (gen)))))
  ([n x]
   (let [ifn (get-ifn)]
     (if-not ifn
       (repeat n x)
       (letfn [(gen [i]
                 (lazy-seq
                   (when (pos? i)
                     (ifn)
                     (cons x (gen (dec i))))))]
         (gen n))))))

(defn- sci-cycle [coll]
  (let [ifn (get-ifn)]
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
  (let [ifn (get-ifn)]
    (if-not ifn
      (iterate f x)
      (letfn [(gen [v]
                (lazy-seq
                  (ifn)
                  (cons v (gen (f v)))))]
        (gen x)))))

;;; Materializers — consuming functions that fire interrupt-fn per element

(defn- sci-dorun
  ([coll]
   (let [ifn (get-ifn)]
     (if-not ifn
       (dorun coll)
       (loop [s (seq coll)]
         (when s
           (ifn)
           (recur (next s)))))))
  ([n coll]
   (let [ifn (get-ifn)]
     (if-not ifn
       (dorun n coll)
       (loop [s (seq coll) i 0]
         (when (and s (< i n))
           (ifn)
           (recur (next s) (inc i))))))))

(defn- sci-doall
  ([coll]
   (let [ifn (get-ifn)]
     (if-not ifn
       (doall coll)
       (do (loop [s (seq coll)]
             (when s (ifn) (recur (next s))))
           coll))))
  ([n coll]
   (let [ifn (get-ifn)]
     (if-not ifn
       (doall n coll)
       (do (loop [s (seq coll) i 0]
             (when (and s (< i n)) (ifn) (recur (next s) (inc i))))
           coll)))))

(defn- sci-count [coll]
  (let [ifn (get-ifn)]
    (if (or (not ifn) (counted? coll))
      (count coll)
      (loop [s (seq coll) n 0]
        (if s
          (do (ifn) (recur (next s) (inc n)))
          n)))))

(defn- sci-into
  ([to from]
   (let [ifn (get-ifn)]
     (if-not ifn
       (into to from)
       (reduce (fn [acc x] (ifn) (conj acc x)) to from))))
  ([to xf from]
   (let [ifn (get-ifn)]
     (if-not ifn
       (into to xf from)
       (transduce (comp (map (fn [x] (ifn) x)) xf) conj to from)))))

(defn- sci-reduce
  ([f coll]
   (let [ifn (get-ifn)
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
   (let [ifn (get-ifn)]
     (if-not ifn
       (reduce f init coll)
       (loop [v init s (seq coll)]
         (if s
           (do (ifn)
               (let [ret (f v (first s))]
                 (if (reduced? ret) @ret (recur ret (next s)))))
           v))))))

;;; Installation

(defn install!
  "Replaces dangerous host functions in the clojure.core namespace with
  interruptible versions. Called by opts/init when :interrupt-fn is set."
  [env]
  (swap! env
    (fn [e]
      (let [core   (get-in e [:namespaces 'clojure.core])
            ns-obj (:ns (meta (get core 'range)))
            mk     (fn [sym f]
                     (utils/new-var sym f {:ns ns-obj :name sym :sci/built-in true}))]
        (update-in e [:namespaces 'clojure.core] merge
                   {'range   (mk 'range   sci-range)
                    'repeat  (mk 'repeat  sci-repeat)
                    'cycle   (mk 'cycle   sci-cycle)
                    'iterate (mk 'iterate sci-iterate)
                    'doall   (mk 'doall   sci-doall)
                    'dorun   (mk 'dorun   sci-dorun)
                    'count   (mk 'count   sci-count)
                    'into    (mk 'into    sci-into)
                    'reduce  (mk 'reduce  sci-reduce)})))))
