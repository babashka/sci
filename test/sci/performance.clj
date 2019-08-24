(ns sci.performance
  (:require
   [criterium.core :as cc]
   [sci.core :as sci]
   [clojure.test :refer [deftest]]
   #_[clj-async-profiler.core :as prof]))

(deftest reusable-fn-test
  (println "Testing reusable function result.")
  (doseq [[example args] [["#(assoc (hash-map :a 1 :b 2) %1 %2))" [:b 3]]]]
    (let [f (sci/eval-string example)]
      (cc/quick-bench (apply f args))))
  (println))
;; 8 µs (MBA2015)

#_(deftest constant?-test
  (println "Testing (some-fn fn? number? string? keyword?)")
  (let [f (some-fn fn? number? string? keyword?)]
    (cc/quick-bench (f :a)))
  (println "Testing (or (fn? x) (number? x) (string? x) (keyword? x))")
  ;; this version is about 10x faster
  (let [f (fn [x] (or (fn? x) (number? x) (string? x) (keyword? x)))]
    (cc/quick-bench (f :a)))
  (println))

(comment
  (sci/eval-string "#(inc x1)") ;; yay, error
  (sci/eval-string "(#(do %1))") ;; arity error is coming from Clojure :-)
  (sci/eval-string "((fn [x]))") ;; arity error is coming from Clojure :-)
  )
