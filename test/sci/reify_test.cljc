(ns sci.reify-test
  (:require [clojure.test :refer #?(:clj  [deftest is testing]
                                    :cljs [deftest])]
            [sci.test-utils :as tu]))

(defn eval* [prog]
  (tu/eval* prog {}))

(deftest reify-test
  #?(:clj
     (testing "reifying Object"
       (is (eval* "(str (reify Object (toString [this] \"this!\")))")))))
