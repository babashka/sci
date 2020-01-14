(ns sci.performance
  (:require
   [criterium.core :as cc]
   [sci.core :as sci]
   [sci.test-utils :refer [native?]]
   [clojure.test :refer [deftest]]))

;; 381a158f72c1efbb8e48229bc67c7a109dffe7c0 Execution time mean : 160.463688 µs
;; 6a63e4212567ad0304a88094eeb2970317d0bfd8 Execution time mean : 94.107237 µs

(when (and (= "true" (System/getenv "SCI_TEST_PERFORMANCE")) (not native?))
  (deftest reusable-fn-test
    (println "Testing reusable function result.")
    (doseq [[example args] [["#(loop [x 1] (do (if (< x %2) (recur (inc x)) {%1 (+ x 1 2 3)})))" [:b 10]]]]
      ;; returns {:b 16}
      (let [f (sci/eval-string example)]
        (cc/quick-bench (apply f args))))
    (println)))

(comment
  (sci/eval-string "#(inc x1)") ;; yay, error
  (sci/eval-string "(#(do %1))") ;; arity error is coming from Clojure :-)
  (sci/eval-string "((fn [x]))") ;; arity error is coming from Clojure :-)
  )
