(ns sci.performance-test
  (:require
   [criterium.core :as cc]
   [sci.core :as sci]
   [sci.test-utils :refer [native?]]
   [clojure.test :refer [deftest]]))

;; 381a158f72c1efbb8e48229bc67c7a109dffe7c0 Execution time mean : 160.463688 µs
;; 6a63e4212567ad0304a88094eeb2970317d0bfd8 Execution time mean : 94.107237 µs
;; c2ffce590f8185b9fd5fdbb7e3746ba37f2ae02f Execution time mean : 82.026964 µs
;; 33bc0b0a14d3f9d95ad9ecf9f1b29e95527153a3 Execution time mean : 48.430510 µs
;; d556416a1e932e334ffc77ac3c8158cb88d6165b Execution time mean : 38.172856 µs
;; 08ed47722ccd65e60ca9f24991e94fe549ea0585 Execution time mean : 30.940262 µs
;; 4f228a5914f91512866b31a2b0bcf55b0b35a0fe Execution time mean : 39.262859 µs

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
