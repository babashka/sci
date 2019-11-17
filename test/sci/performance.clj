(ns sci.performance
  (:require
   [criterium.core :as cc]
   [sci.core :as sci]
   [sci.test-utils :refer [native?]]
   [clojure.test :refer [deftest]]))

(when (and (= "true" (System/getenv "SCI_TEST_PERFORMANCE")) (not native?))
  (deftest reusable-fn-test
    (println "Testing reusable function result.")
    (doseq [[example args] [["#(loop [x 1] (do (if (< x %2) (recur (inc x)) {%1 (+ x 1 2 3)})))" [:b 10]]]]
      ;; returns {:b 16}
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
      (println)))

(comment
  (sci/eval-string "#(inc x1)") ;; yay, error
  (sci/eval-string "(#(do %1))") ;; arity error is coming from Clojure :-)
  (sci/eval-string "((fn [x]))") ;; arity error is coming from Clojure :-)
  )
