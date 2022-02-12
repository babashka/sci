(ns sci.impl.binding-array-refactor-test
  (:require  [clojure.test :as t :refer [deftest is]]
             [sci.core :as sci]))

(deftest self-returning-fn-test
  (is (fn? (sci/eval-string "((fn f [] f))"))))

(deftest enclosing-array-test
  (is (= 1 (sci/eval-string "(let [x 1] (defn foo [] x)) (foo)")))
  (is (= [:foo :bar] (sci/eval-string "(let [x :foo y :bar z nil] ((fn ([] z) ([_] [x y])) 1))")))
  (is (= 3 (sci/eval-string  "(let [x 1 y 2] ((fn [] (let [g (fn [] y)] (+ x (g))))))"))))

(deftest areduce-test
  (is (= 6.0 (sci/eval-string "
((fn [xs]
  (let [x 3]
    (clojure.core/loop [i 0 ret (float 0)]
      (if (clojure.core/< i x)
        (recur (inc i) (+ ret (aget xs i))) ret)))) (into-array [1.0 2.0 3.0]))"))))

(deftest shadowed-let-binding-test
  (is (= 2 (sci/eval-string "(let [x 1 x (+ x 1)] x)"))))

(deftest multi-arity-self-call-test
  (is (= 2 (sci/eval-string "((fn f ([x] (f x 1)) ([x y] (+ x y))) 1)"))))

(deftest multi-arity-test
  (is (= :foo (sci/eval-string "
(defn sh
  ([x] (sh x nil))
  ([x y]
    (let [[g h i] nil]
     :foo))
  ([x y z]
   :bar))

(sh 1 2)
")))
  )

(deftest order-of-arguments-test
  (is (= [1 2 3 4 5 6 7 8 9] (sci/eval-string "(defn foo [a1 a2 a3 a4 a5 a6 a7 a8 a9] [a1 a2 a3 a4 a5 a6 a7 a8 a9]) (foo 1 2 3 4 5 6 7 8 9)"))))

(deftest varargs-test
  (is (nil? (sci/eval-string "(defn foo [x y & xs] xs) (foo 1 2)"))))
