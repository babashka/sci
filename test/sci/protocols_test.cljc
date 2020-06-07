(ns sci.protocols-test
  (:require #?(:cljs [clojure.string :as str])
            [clojure.test :refer [deftest is]]
            [sci.test-utils :as tu]))

(deftest protocol-test
  (let [prog "
(defprotocol AbstractionA
               (foo [obj])
               (bar [obj]))

(extend Number AbstractionA
  {:foo (fn [_] :number)
   :bar (fn [_] :bar/number)})

(extend-protocol AbstractionA
  nil
  (foo [s] (str \"foo-A!\"))
  String
  (foo [s] (str \"foo-A-\" (.toUpperCase s))))

[(foo nil)
 (foo \"Bar\")
 (foo 1)
 (bar 1)]"
        prog #?(:clj prog
                :cljs (str/replace prog "String" "js/String"))]
    (is (= ["foo-A!" "foo-A-BAR" :number :bar/number]
           (tu/eval* prog #?(:clj {}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String}}}))))))

