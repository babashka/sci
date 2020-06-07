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
               (fooB [obj x]))

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
  (fooB [_ x] x))

[(foo nil)
 (bar nil)
 (foo \"Bar\")
 (bar \"Bar\")
 (foo 1)
 (bar 1)
 (foo {})
 (bar {})
 (fooB {} :fooB/object)
 (satisfies? AbstractionA 1)]"
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
            :fooB/object
            true]
           (tu/eval* prog #?(:clj {}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String
                                                       :Number js/Number}}}))))))

(deftest reify-test
  (let [prog "
(defprotocol Fruit (subtotal [item]))
(def x (reify Fruit (subtotal [_] 1)))
(subtotal x)"]
    (is (= 1 (tu/eval* prog {})))))
