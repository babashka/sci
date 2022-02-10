(ns sci.impl.binding-array-refactor-test
  (:require  [clojure.test :as t :refer [deftest is]]
             [sci.core :as sci]))

(deftest self-returning-fn-test
  (is (fn? (sci/eval-string "((fn f [] f))"))))

(deftest enclosing-array-test
  (is (= 1 (sci/eval-string "(let [x 1] (defn foo [] x)) (foo)")))
  (is (= [:foo :bar] (sci/eval-string "(let [x :foo y :bar z nil] ((fn ([] z) ([_] [x y])) 1))")))
  (is (= 3 (sci/eval-string  "(let [x 1 y 2] ((fn [] (let [g (fn [] y)] (+ x (g))))))")))
  )

(deftest areduce-test
  (is (= 6.0 (sci/eval-string "
(let [xs (into-array [1.0 2.0 3.0]) l__6542__auto__ (clojure.core/alength xs)]
  (clojure.core/loop [i 0 ret (float 0)]
    (if (clojure.core/< i l__6542__auto__)
        (recur (clojure.core/unchecked-inc-int i) (+ ret (aget xs i))) ret)))")))
  (is (= 6.0 (sci/eval-string "
((fn [xs]
  (let [x 3]
    (clojure.core/loop [i 0 ret (float 0)]
      (if (clojure.core/< i x)
        (recur (inc i) (+ ret (aget xs i))) ret)))) (into-array [1.0 2.0 3.0]))"))))

