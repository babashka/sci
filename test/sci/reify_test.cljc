(ns sci.reify-test
  (:require [clojure.test :refer #?(:clj [deftest is testing]
                                    :cljs [deftest is testing])]
            #?(:clj [sci.impl.types :as t])
            [sci.test-utils :as tu])
  #?(:clj (:import [sci.impl.types IReified])))

(deftest reify-test
  #?(:clj
     (do (testing "reifying Object"
           (is (= "this!" (tu/eval* "(str (reify Object (toString [this] \"this!\")))" nil))))
         (testing "metadata"
           (is (= {:line 1, :column 7, :k :v}
                  (tu/eval* "(meta ^{:k :v} (reify Object (toString [this] \"this!\")))" nil)))))))

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
  [(str r) (deref r) (custom r)])" nil)))))))

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
       (let [mixed-opts
             {:classes {'Interface1 Interface1
                        'Interface2 Interface2
                        'clojure.lang.Associative clojure.lang.Associative
                        'clojure.lang.ILookup clojure.lang.ILookup}
              :reify-fn
              (fn [{:keys [interfaces methods protocols]}]
                (reify
                  Interface1
                  (method [this]
                    ((get methods 'method) this))

                  Interface2
                  (method [this first]
                    ((get methods 'method) this first))

                  clojure.lang.Associative ;; Associative is also ILookup
                  (assoc [this k v]
                    ((get methods 'assoc) this k v))

                  IReified
                  (getInterfaces [this]
                    interfaces)
                  (getMethods [this]
                    methods)
                  (getProtocols [this]
                    protocols)))}
             prog "
(defprotocol Protocol1
  (method [this first second]))
(defprotocol Protocol2
  (method [this first second third]))
(defprotocol Protocol3) ;; no methods
(defprotocol Protocol4) ;; not implemented
(let [r (reify
          Interface1 (method [this] \"Interface1\")
          Interface2 (method [this first] \"Interface2\")
          clojure.lang.Associative (assoc [this k v] {k v})
          Protocol1  (method [this first second] \"Protocol1\")
          Protocol2  (method [this first second third] \"Protocol2\")
          Protocol3
          )]
  %s)"]
         (testing "reifying two interfaces and two protocols that have the same named methods"
           (is (= ["Interface1" "Interface2" "Protocol1" "Protocol2"]
                  (tu/eval* (format prog (str '[(method r)
                                                (method r 1)
                                                (method r 1 2)
                                                (method r 1 2 3)]))
                            mixed-opts))))
         (testing "reified interfaces are instances of those interfaces"
           (is (= [true true true true]
                  (tu/eval* (format prog
                                    (str '[(instance? Interface1 r)
                                           (instance? Interface2 r)
                                           (instance? clojure.lang.Associative r)
                                           (instance? clojure.lang.ILookup r)]))
                            mixed-opts))))
         (testing "reifying protocols together with classes satisfies the protocols"
           (is (= [true true true false]
                  (tu/eval* (format prog
                                    (str '[(satisfies? Protocol1 r)
                                           (satisfies? Protocol2 r)
                                           (satisfies? Protocol3 r)
                                           (satisfies? Protocol4 r)]))
                            mixed-opts))))))))

(deftest reify-form-from-macro-test
  #?(:clj
     (when-not tu/native?
       (let [opts
             {:classes {:public-class (fn [x]
                                        (when (instance? IReified x)
                                          (first (t/getInterfaces x))))
                        'java.util.function.Function java.util.function.Function}
              :reify-fn
              (fn [{:keys [interfaces methods protocols]}]
                (reify
                  java.util.function.Function
                  (apply [this arg]
                    ((get methods 'apply) this arg))

                  IReified
                  (getInterfaces [this]
                    interfaces)
                  (getMethods [this]
                    methods)
                  (getProtocols [this]
                    protocols)))
              }]
         (testing "reify form from macro has (incorrectly) fully qualifed method name"
           (is (= 2
                  (tu/eval*
                   (pr-str '(do
                              (defmacro clj-fn->function [f]
                                `(reify java.util.function.Function
                                   (apply [_# x#]
                                     (~f x#))))
                              (.apply (clj-fn->function inc) 1)))
                   opts))))))))
