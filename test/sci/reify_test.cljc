(ns sci.reify-test
  (:require [clojure.test :refer #?(:clj [deftest is testing]
                                    :cljs [deftest is testing])]
            [sci.test-utils :as tu])
  #?(:clj (:import [sci.impl.types IReified])))

(deftest reify-test
  #?(:clj
     (testing "reifying Object"
       (is (= "this!" (tu/eval* "(str (reify Object (toString [this] \"this!\")))" nil))))))

(deftest reify-mixed-protocol-class-test
  #?(:clj
     (when-not tu/native?
       (testing "reifying Object and IDeref"
         (is (= ["obj-str" "ideref-deref" "protocol-custom"]
                (tu/eval* "
(defprotocol Protocol
  (custom [this]))
(let [r (reify
          Object (toString [this] \"obj-str\")
          clojure.lang.IDeref (deref [this] \"ideref-deref\")
          Protocol (custom [this] \"protocol-custom\")
          )]
  [(str r) (deref r) (custom r)])"
                          {:reify {'#{java.lang.Object sci.impl.types.IReified}
                                   (fn [interfaces methods protocols]
                                     (reify
                                       Object
                                       (toString [this]
                                         ((get methods 'toString) this))
                                       IReified
                                       (getInterfaces [this]
                                         interfaces)
                                       (getMethods [this]
                                         methods)
                                       (getProtocols [this]
                                         protocols)))}})))))))

(deftest reify-multiple-protocols
  (testing "reifying two custom protocols"
    (is (= ["A" "B"]
           (tu/eval* "
(defprotocol ProtocolA
  (customA [this]))
(defprotocol ProtocolB
  (customB [this]))
(let [r (reify
          ProtocolA (customA [this] \"A\")
          ProtocolB (customB [this] \"B\")
          )]
  [(customA r) (customB r)])"
                     nil)))))

(deftest reify-protocols-with-matching-method-names
  (testing "reifying two protocols that have the same named methods"
     (is (= ["Protocol1" "Protocol2"]
            (tu/eval* "
(defprotocol Protocol1
  (method [this first second]))
(defprotocol Protocol2
  (method [this first second third]))
(let [r (reify
          Protocol1  (method [this first second] \"Protocol1\")
          Protocol2  (method [this first second third] \"Protocol2\")
          )]
  [(method r 1 2) (method r 1 2 3)])"
                      nil)))))

#?(:clj
   (do (definterface Interface1 (method []))
       (definterface Interface2 (method [first]))))

(deftest reify-with-matching-method-names
  #?(:clj
     (when-not tu/native?
       (testing "reifying two interfaces and two protocols that have the same named methods"
         (is (= ["Interface1" "Interface2" "Protocol1" "Protocol2"]
                (tu/eval* "
(defprotocol Protocol1
  (method [this first second]))
(defprotocol Protocol2
  (method [this first second third]))
(let [r (reify
          Interface1 (method [this] \"Interface1\")
          Interface2 (method [this first] \"Interface2\")
          Protocol1  (method [this first second] \"Protocol1\")
          Protocol2  (method [this first second third] \"Protocol2\")
          )]
  [(method r) (method r 1) (method r 1 2) (method r 1 2 3)])"
                          {:classes {'Interface1 Interface1
                                     'Interface2 Interface2}
                           :reify {'#{sci.reify_test.Interface1
                                      sci.reify_test.Interface2
                                      sci.impl.types.IReified}
                                   (fn [interfaces methods protocols]
                                     (reify
                                       Interface1
                                       (method [this]
                                         ((get methods 'method) this))

                                       Interface2
                                       (method [this first]
                                         ((get methods 'method) this first))

                                       IReified
                                       (getInterfaces [this]
                                         interfaces)
                                       (getMethods [this]
                                         methods)
                                       (getProtocols [this]
                                         protocols)))}})))))))
