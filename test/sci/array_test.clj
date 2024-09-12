(ns sci.array-test
  (:require [clojure.test :as t :refer [deftest is]]
            [sci.core :as sci]))

(deftest Integer-Type-test
  (is (= 2 (sci/eval-string
            "(def arr (make-array Integer/TYPE 10)) (aset arr 1 2) (aget arr 1)"))))
