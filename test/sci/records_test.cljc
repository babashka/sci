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
  Foo (foo [_] (dec a)))

(defrecord BarRecord [a b]
  Foo (foo [_] (inc b)))

(defprotocol Graph (graph [_]))

(extend FooRecord
  Graph {:graph (fn [x] {:from (:a x) :to (:b x)})})

(let [a (->FooRecord 1 2) b (BarRecord. 1 2)]
  [(foo a) (foo b) (graph a)])"]
      (is (= [0 3 {:from 1, :to 2}] (tu/eval* prog {}))))))
