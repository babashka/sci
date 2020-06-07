(ns sci.protocols-test
  (:require #?(:cljs [clojure.string :as str])
            [clojure.test :refer [deftest is]]
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
 (foo \"Bar\")]"
        prog #?(:clj prog
                :cljs (str/replace prog "String" "js/String"))]
    (is (= ["foo-A!" "foo-A-BAR"]
           (tu/eval* prog #?(:clj {}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String}}}))))))

#_(deftest protocol2-test
  (let [prog "
(defprotocol2 AbstractionA
               (foo [obj]))

(extend-protocol2 AbstractionA
  nil
  (foo [s] (str \"foo-A!\"))
  String
  (foo [s] (str \"foo-A-\" (.toUpperCase s))))

[(foo nil)
 (foo \"Bar\")]"
        prog #?(:clj prog
                :cljs (str/replace prog "String" "js/String"))]
    (is (= ["foo-A!" "foo-A-BAR"]
           (tu/eval* prog #?(:clj {}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String}}}))))))
