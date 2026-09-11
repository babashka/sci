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

#?(:clj
   (deftest interface-dispatch-value-test
     (is (= [:deref :deref :default]
            (eval* "
(defmulti kind type)
(defmethod kind clojure.lang.IDeref [_] :deref)
(defmethod kind :default [_] :default)
[(kind (atom 1)) (kind (delay 1)) (kind 1)]")))
     (is (= [true :default]
            (eval* "
(defmulti kind type)
(defmethod kind clojure.lang.IDeref [_] :deref)
(defmethod kind :default [_] :default)
(let [found (some? (get-method kind clojure.lang.IDeref))]
  (remove-method kind clojure.lang.IDeref)
  [found (kind (atom 1))])")))
     (is (= :deref
            (eval* "
(defmulti kind type)
(defmethod kind clojure.lang.IDeref [_] :deref)
(defmethod kind clojure.lang.IAtom [_] :atom)
(prefer-method kind clojure.lang.IDeref clojure.lang.IAtom)
(kind (atom 1))")))))

;; TODO:cljd no hierarchies
#?(:cljd nil :default
(deftest prefer-method-test
  (is (= :rect-shape
         (eval* "
(derive ::rect ::shape)
(defmulti bar (fn [x y] [x y]))
(defmethod bar [::rect ::shape] [x y] :rect-shape)
(defmethod bar [::shape ::rect] [x y] :shape-rect)
(prefer-method bar [::rect ::shape] [::shape ::rect])
(bar ::rect ::rect)")))))

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
