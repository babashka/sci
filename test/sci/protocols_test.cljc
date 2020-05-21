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
           (tu/eval* prog #?(:clj {'prn #(.println System/out %)}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String}}}))))))
