(ns sci.bench-this-as
  (:require [sci.core :as sci]))

(defn now [] (js/performance.now))

(defn bench [label expr n]
  (let [ctx (sci/init {:classes {:allow :all}})]
    ;; warmup
    (dotimes [_ 5]
      (sci/eval-string* ctx expr))
    ;; bench
    (let [times (mapv (fn [_]
                        (let [start (now)]
                          (sci/eval-string* ctx expr)
                          (- (now) start)))
                      (range n))
          total (reduce + times)
          avg (/ total n)
          mn (apply min times)
          mx (apply max times)]
      (println (str label ": avg=" (.toFixed avg 1) "ms"
                    " min=" (.toFixed mn 1) "ms"
                    " max=" (.toFixed mx 1) "ms"
                    " (" n " runs)")))))

(defn -main []
  (bench "reduce 10M"
         "(reduce (fn [acc x] (+ acc x)) 0 (range 10000000))"
         20))

(-main)
