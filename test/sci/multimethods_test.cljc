(ns sci.multimethods-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   [sci.test-utils :as tu]))

(defn eval* [expr]
  (tu/eval* expr {}))

(deftest default-test
  (is (= :default (eval* "(defmulti foo type) (defmethod foo :default [c] :default) (foo :foo)"))))

(deftest defmethod-test
  (is (= "Hello" (eval* "
(defmulti greeting (fn [x] (x \"language\")))
(defmethod greeting \"English\" [params] \"Hello\") (greeting {\"id\" \"1\", \"language\" \"English\"})"))))

(deftest remove-method-test
  (is (= "Default" (eval* "
(defmulti greeting (fn [x] (x \"language\")))
(defmethod greeting \"English\" [params] \"Hello\")
(defmethod greeting :default [params] \"Default\")
(remove-method greeting \"English\")
(greeting {\"id\" \"1\", \"language\" \"English\"})"))))

(deftest prefer-method-test
  (is (= :rect-shape
         (eval* "
(derive ::rect ::shape)
(defmulti bar (fn [x y] [x y]))
(defmethod bar [::rect ::shape] [x y] :rect-shape)
(defmethod bar [::shape ::rect] [x y] :shape-rect)
(prefer-method bar [::rect ::shape] [::shape ::rect])
(bar ::rect ::rect)"))))

(deftest multi-arity-test
  (is (= [:default :one :two :three :more]
         (eval* "
(defmulti foo (fn [x & _] x))

(defmethod foo :default [_ & _] :default)

;; Like a standar multi-arity function
(defmethod foo :bar
  ([_ _] :one)
  ([_ _ _] :two)
  ([_ _ _ _] :three)
  ([_ _ _ _ & more] :more))

[(foo :baz 1)
 (foo :bar 1)
 (foo :bar 1 2)
 (foo :bar 1 2 3)
 (foo :bar 1 2 3 4)]
"))))
