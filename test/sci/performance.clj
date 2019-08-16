(ns sci.performance
  (:require [criterium.core :as cc]
            [sci.core :as sci]))

(def f (sci/eval-string "#(assoc (hash-map :a 1 :b 2) %1 %2))"))

(comment
  (f :b 3))

(defn bench-sci []
  (let [f (sci/eval-string "#(assoc (hash-map :a 1 :b 2) %1 %2))")]
    (cc/quick-bench (f :b 3))))

(defn bench-clojure []
  (let [f #(assoc (hash-map :a 1 :b 2) %1 %2)]
    (cc/quick-bench (f :b 3))))

(comment
  (bench-sci) ;; Execution time mean : 11.110878 µs
  (bench-clojure) ;; Execution time mean : 416.615115 ns
  (sci/eval-string "#(inc x1)") ;; yay, error
  )
