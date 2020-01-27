(ns sci.multimethods-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   [sci.test-utils :as tu]))

(defn eval* [expr]
  (tu/eval* expr {}))

(deftest default-test
  (is (= :default (eval* "(defmulti foo type) (defmethod foo :default [c] :default) (foo :foo)"))))

