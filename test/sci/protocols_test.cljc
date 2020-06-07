(ns sci.protocols-test
  (:require #?(:cljs [clojure.string :as str])
            [clojure.test :refer [deftest is]]
            [sci.test-utils :as tu]))

(deftest protocol-test
  (let [prog "
(defprotocol AbstractionA
               (foo [obj])
               (bar [obj]))

(defprotocol AbstractionB
               (fooB [obj]))

(extend Number AbstractionA
  {:foo (fn [_] :number)
   :bar (fn [_] :bar/number)})

(extend-protocol AbstractionA
  nil
  (foo [s] (str \"foo-A!\"))
  (bar [s] (str \"bar-A!\"))
  String
  (foo [s] (str \"foo-A-\" (.toUpperCase s)))
  (bar [s] (str \"bar-A-\" (.toUpperCase s))))

(extend-type Object
  AbstractionA
  (foo [_] :foo/object)
  (bar [_] :bar/object)
  AbstractionB
  (fooB [_] :fooB/object))

[(foo nil)
 (bar nil)
 (foo \"Bar\")
 (bar \"Bar\")
 (foo 1)
 (bar 1)
 (foo {})
 (bar {})
 (fooB {})]"
        prog #?(:clj prog
                :cljs (-> prog
                          (str/replace "String" "js/String")
                          (str/replace "Number" "js/Number")
                          (str/replace "Object" ":default")))]
    (is (= ["foo-A!"
            "bar-A!"
            "foo-A-BAR"
            "bar-A-BAR"
            :number
            :bar/number
            :foo/object
            :bar/object
            :fooB/object]
           (tu/eval* prog #?(:clj {}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String
                                                       :Number js/Number}}}))))))

