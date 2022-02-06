(ns sci.impl.binding-array-refactor-test
  (:require  [clojure.test :as t :refer [deftest is]]
             [sci.core :as sci]))

(deftest enclosing-array-test
  (is (= [:foo :bar] (sci/eval-string "(let [x :foo y :bar z nil] ((fn ([] z) ([_] [x y])) 1))"))))

