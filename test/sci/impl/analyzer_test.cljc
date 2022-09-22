(ns sci.impl.analyzer-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.analyzer :as ana]
            [sci.impl.parser :as parser]
            [sci.impl.types :as types]))

(defn constant-node? [s]
  (let [ctx (sci/init {:bindings {'foo 1}})
        v (parser/parse-string ctx s)
        a (ana/analyze ctx v)
        e (types/eval a ctx nil)]
    [e
     #?(:clj (instance? sci.impl.types.ConstantNode a)
        :cljs (not (instance? sci.impl.types.NodeR a)))]))

(deftest analyze-constant-test
  (is (= [1 true] (constant-node? "1")))
  (is (= [[1 2 3] true] (constant-node? "[1 2 3]")))
  (is (= [[1 2 [1 2 3]] true] (constant-node? "[1 2 [1 2 3]]")))
  (is (= [#{1 2 3} true] (constant-node? "#{1 2 3}")))
  (is (= [{:a 1} true] (constant-node? "{:a 1}")))
  (is (= [{:a {:a 1}} true] (constant-node? "{:a {:a 1}}")))
  (testing "global values are inlined if they are constants"
    (is (= [[1] true] (constant-node? "[foo]")))
    (is (= [{:a 1} true](constant-node? "{:a foo}"))))
  (is (not (second (constant-node? "(+ 1 2 3)"))))
  (is (not (second(constant-node? "^{:a (+ 1 2 3)} []"))))
  )

(deftest analyze-def-metadata-test
  (let [m (meta (first (constant-node? "(def a 1)")))]
    (is (= 1 (:line m)))
    (is (= 1 (:column m))))
  (let [m (meta (first (constant-node? "(def ^:foo a 1)")))]
    (is (= true (:foo m)))
    (is (= 1 (:line m)))
    (is (= 1 (:column m)))))
