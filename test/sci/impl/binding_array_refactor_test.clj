(ns sci.impl.binding-array-refactor-test
  (:require  [clojure.test :as t :refer [deftest is]]
             [sci.core :as sci]))

(deftest self-returning-fn-test
  (is (fn? (sci/eval-string "((fn f [] f))"))))

(deftest enclosing-array-test
  (is (= 1 (sci/eval-string "(let [x 1] (defn foo [] x)) (foo)")))
  (is (= [:foo :bar] (sci/eval-string "(let [x :foo y :bar z nil] ((fn ([] z) ([_] [x y])) 1))")))
  (is (= 3 (sci/eval-string  "(let [x 1 y 2] ((fn [] (let [g (fn [] y)] (+ x (g))))))"))))

