(ns sci.array-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [sci.core :as sci]))

(deftest Integer-Type-test
  (is (= 2 (sci/eval-string
            "(def arr (make-array Integer/TYPE 10)) (aset arr 1 2) (aget arr 1)"))))

(deftest aset-primitive-types-test
  (testing "aset writes to an array of every primitive type"
    (is (= [1.5 2.5 7 8 9 10 \x true :kw]
           (sci/eval-string "
[(let [a (double-array 2)]  (aset a 0 1.5)          (aget a 0))
 (let [a (float-array 2)]   (aset a 0 (float 2.5))  (aget a 0))
 (let [a (long-array 2)]    (aset a 0 7)            (aget a 0))
 (let [a (int-array 2)]     (aset a 0 8)            (aget a 0))
 (let [a (short-array 2)]   (aset a 0 (short 9))    (aget a 0))
 (let [a (byte-array 2)]    (aset a 0 (byte 10))    (aget a 0))
 (let [a (char-array 2)]    (aset a 0 \\x)          (aget a 0))
 (let [a (boolean-array 2)] (aset a 0 true)         (aget a 0))
 (let [a (object-array 2)]  (aset a 0 :kw)          (aget a 0))]"))))
  (testing "the index is coerced, as clojure.core/aset coerces it with (int idx)"
    (is (= [1.5 1.5]
           (sci/eval-string "
(let [a (double-array 4)]
  [(do (aset a 1 1.5) (aget a 1))
   (do (aset a 2.0 1.5) (aget a 2))])"))))
  (testing "multi-dimensional arrays"
    (is (= :changed
           (sci/eval-string
            "(let [a (to-array-2d [[1 2] [3 4]])] (aset a 1 0 :changed) (aget a 1 0))")))))

(defn- ns-per-write
  "Fastest of a few timings of `expr`, in nanoseconds per iteration."
  [expr n]
  (let [code (str "(let [arr (double-array 8)] (dotimes [_ " n "] " expr "))")]
    (sci/eval-string code)
    (apply min (repeatedly 3 (fn []
                               (let [t0 (System/nanoTime)]
                                 (sci/eval-string code)
                                 (/ (double (- (System/nanoTime) t0)) n)))))))

(deftest aset-on-primitive-array-is-not-reflective-test
  ;; aset on a primitive array once fell through to an unhinted
  ;; clojure.lang.RT/aset, whose overloads then had to be resolved
  ;; reflectively on every write: microseconds per write, against tens of
  ;; nanoseconds for aset-double. Compare the two in the same run rather
  ;; than against a fixed time, so this survives a slow or loaded machine;
  ;; the gap it guards against was three orders of magnitude.
  (let [n 100000
        generic (ns-per-write "(aset arr 3 1.5)" n)
        typed (ns-per-write "(aset-double arr 3 1.5)" n)]
    (is (< generic (* 25 typed))
        (format "aset %.0fns/write vs aset-double %.0fns/write: aset looks reflective again"
                generic typed))))
