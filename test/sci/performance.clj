(ns sci.performance
  (:require
   [criterium.core :as cc]
   [sci.core :as sci]
   [clj-async-profiler.core :as prof]))

(comment
  (def f (sci/eval-string "#(assoc (hash-map :a 1 :b 2) %1 %2))"))
  (f :b 3)
  (meta f)
  (prof/profile (dotimes [_ 10000] (f :b 3))))

(defn bench-sci []
  (let [f (sci/eval-string "#(assoc (hash-map :a 1 :b 2) %1 %2))")]
    (cc/quick-bench (f :b 3))))

(defn bench-clojure []
  (let [f #(assoc (hash-map :a 1 :b 2) %1 %2)]
    (cc/quick-bench (f :b 3)))
  (let [f #(eval `(let [x# ~%1 y# ~%2] (assoc (hash-map :a 1 :b 2) x# y#)))]
    (cc/quick-bench (f :b 3))))

(comment
  (bench-sci) ;; Execution time mean : 13 µs (7µs on MBP2019 8core)
  (bench-clojure) ;; Execution time mean : 410 ns
  (sci/eval-string "#(inc x1)") ;; yay, error
  (sci/eval-string "(#(do %1))") ;; arity error is coming from Clojure :-)
  (sci/eval-string "((fn [x]))") ;; arity error is coming from Clojure :-)
  )
