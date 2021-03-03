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
                                   (fn [methods]
                                     (reify
                                       Object
                                       (toString [this]
                                         ((get-in methods '[java.lang.Object toString]) this))

                                       IReified
                                       (getMethods [this]
                                         ((get-in methods '[sci.impl.types.IReified getMethods]) this))
                                       (getInterfaces [this]
                                         ((get-in methods '[sci.impl.types.IReified getInterfaces]) this))))}})))))))

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

(deftest reify-almost-clashing-protocols
  (testing "reifying two custom protocols that have similar signatures"
    (is (= ["A" "B"]
           (tu/eval* "
(defprotocol ProtocolA
  (customA [this first-arg]))
(defprotocol ProtocolB
  (customA [this first-arg second-arg]))
(let [r (reify
          ProtocolA (customA [this _] \"A\")
          ProtocolB (customA [this _ _] \"B\")
          )]
  [(customA r 1) (customA r 1 2)])"
                     nil)))))
