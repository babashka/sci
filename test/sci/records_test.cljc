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

(defrecord Foo [a b]
  Foo (foo [_] a))

(defrecord Bar [a b]
  Foo (foo [_] b))

(let [a (->Foo 1 2) b (->Foo 1 2)]
  [(foo a) (foo b)])"]
                        (is (= [1 2] (tu/eval* prog {}))))))
