(ns sci.protocols-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.test-utils :as tu]))

(deftest protocol-test
  (let [prog "
(defprotocol AbstractionA
               (foo [obj]))

(extend-protocol AbstractionA
  nil
  (foo [s] (str \"foo-A!\"))
  String
  (foo [s] (str \"foo-A-\" (.toUpperCase s))))

[(foo nil)
 (foo \"Bar\")]"]
    (is (= ["foo-A!" "foo-A-BAR"] (tu/eval* prog {})))))
