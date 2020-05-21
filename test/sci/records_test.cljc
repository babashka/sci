(ns sci.records-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.test-utils :as tu]))

(deftest protocol-test
  (let [prog "
(defrecord Foo [a b])
(let [r (->Foo 1 2)]
  [(:a r) (:b r)])"]
    (is (= [1 2] (tu/eval* prog {}))))
  (testing "protocols"
    (let [prog "
(defprotocol Foo (foo [_] 42))

(defrecord FooRecord [a b]
  Foo (foo [_] a))

(defrecord BarRecord [a b]
  Foo (foo [_] b))

(let [a (->FooRecord 1 2) b (->BarRecord 1 2)]
  [(foo a) (foo b)])"]
      (is (= [1 2] (tu/eval* prog {}))))))
