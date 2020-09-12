(ns sci.deref-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.test-utils :as tu]))

(defn eval* [prog]
  (tu/eval* prog {}))

(deftest deref-test
  (testing "fully qualified"
    (is (= (eval* #?(:clj  "@(reify clojure.lang.IDeref (deref [_] :value))"
                     :cljs "@(reify cljs.core/IDeref (-deref [_] :value))"))
           :value)))
  (testing "with import"
    (is (= (eval* #?(:clj  "(import 'clojure.lang.IDeref)
                            @(reify IDeref (deref [_] :value))"
                        :cljs "(import 'cljs.core.IDeref)
                             @(reify IDeref (-deref [_] :value))"))
           :value)))
  (testing "record implemenation"
    (is (= (eval* #?(:clj  "(defrecord Foo [x] clojure.lang.IDeref (deref [this] x))
                            @(->Foo :value)"
                     :cljs "(defrecord Foo [x] cljs.core/IDeref (deref [this] x))
                            @(->Foo :value)"))
           :value))
    (is (= (eval* #?(:clj  "(import 'clojure.lang.IDeref)
                            (defrecord Foo [x] IDeref (deref [this] x))
                            @(->Foo :value)"
                     :cljs "(defrecord Foo [x] IDeref (deref [this] x))
                            @(->Foo :value)"))
           :value))))
