(ns sci.impl.destructure-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.impl.destructure :as d]))

(deftest destructure-test
  (testing "symbol bindings pass through"
    (is (= '[a 1] (d/destructure '[a 1]))))
  (testing "sequential destructuring binds all names"
    (let [res (d/destructure '[[a b] [1 2]])]
      (is (even? (count res)))
      (is (some #(= 'a %) res))
      (is (some #(= 'b %) res))))
  (testing "map destructuring binds keys"
    (let [res (d/destructure '[{:keys [a b]} {:a 1 :b 2}])]
      (is (some #(= 'a %) res))
      (is (some #(= 'b %) res)))))
