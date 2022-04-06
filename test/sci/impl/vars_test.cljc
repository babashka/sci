(ns sci.impl.vars-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.impl.vars :as vars]))

(deftest unqualify-symbol-test
  (testing "passing a simple symbol returns the symbol"
    (is (= 'foo (vars/unqualify-symbol 'foo))))
  (testing "passing a namespaced symbol returns the symbol without the namespace"
    (is (= 'foo (vars/unqualify-symbol 'some.random.ns/foo)))))
