(ns sci.performance
  (:require [criterium.core :as cc]
            [sci.core :as sci]))

(comment
  (def f (sci/eval-string "#(assoc (hash-map :a 1 :b 2) %1 %2))"))
  (meta f)
  (f :b 3)
  (f :b 3 4 4))

(defn bench-sci []
  (let [f (sci/eval-string "#(assoc (hash-map :a 1 :b 2) %1 %2))")]
    (cc/quick-bench (f :b 3))))

(defn bench-clojure []
  (let [f #(assoc (hash-map :a 1 :b 2) %1 %2)]
    (cc/quick-bench (f :b 3))))

(comment
  (bench-sci) ;; Execution time mean : 13 µs
  (bench-clojure) ;; Execution time mean : 410 ns
  (sci/eval-string "#(inc x1)") ;; yay, error
  (sci/eval-string "(#(do %1))") ;; arity error is coming from Clojure :-)
  (sci/eval-string "((fn [x]))") ;; arity error is coming from Clojure :-)
  )
