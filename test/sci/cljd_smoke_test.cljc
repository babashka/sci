(ns sci.cljd-smoke-test
  "First namespaces that work on ClojureDart. Runs on all platforms.
  Also serves as a compilation canary: requires namespaces that have no
  cljd tests yet."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.impl.callstack]
            [sci.impl.faster]
            [sci.impl.fns]
            [sci.impl.records]
            [sci.impl.resolve]
            [sci.impl.types :as types]))

(deftest eval-form-test
  (is (= '(+ 1 2) (types/getVal (types/->EvalForm '(+ 1 2))))))

(deftest constant-test
  (testing "constants evaluate to themselves"
    (is (= 42 (types/eval (types/->constant 42) nil nil)))))

(deftest eval-node-test
  (is (false? (types/eval-node? 42))))

(deftest stack-test
  (testing "default stack is nil"
    (is (nil? (types/stack 42)))))

(deftest type-impl2-test
  (is (= :foo (types/type-impl2 (with-meta {} {:type :foo})))))
