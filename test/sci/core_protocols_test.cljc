(ns sci.core-protocols-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.test-utils :as tu]))

(defn eval* [prog]
  (tu/eval* prog {}))

(deftest deref-test
  (testing "fully qualified"
    (is (= :value
           (eval* #?(:clj  "@(reify clojure.lang.IDeref (deref [_] :value))"
                     :cljs "@(reify cljs.core/IDeref (-deref [_] :value))")))))
  (testing "with import / unqualified"
    (is (= :value
           (eval* #?(:clj  "(import 'clojure.lang.IDeref)
                            @(reify IDeref (deref [_] :value))"
                     :cljs "@(reify IDeref (-deref [_] :value))")))))
  (testing "record implementation"
    (is (= :value
           (eval* #?(:clj  "(defrecord Foo [x] clojure.lang.IDeref (deref [this] x))
                            @(->Foo :value)"
                     :cljs "(defrecord Foo [x] cljs.core/IDeref (-deref [this] x))
                            @(->Foo :value)"))))
    (is (= :value
           (eval* #?(:clj  "(import 'clojure.lang.IDeref)
                            (defrecord Foo [x] IDeref (deref [this] x))
                            @(->Foo :value)"
                     :cljs "(defrecord Foo [x] IDeref (-deref [this] x))
                            @(->Foo :value)"))))))

(deftest swap-test
  (testing "fully qualified"
    (is (= 2 (eval* #?(:clj "(def x (reify clojure.lang.IAtom (swap [_ f] (f 1)))) (swap! x inc)"
                       :cljs "(def x (reify cljs.core/ISwap (-swap! [_ f] (f 1)))) (swap! x inc)")))))
  (testing "record implementation of swap"
    (is (= 2 (eval* #?(:clj "(defrecord Foo [x] clojure.lang.IAtom (swap [this f] (f x)))
                             (swap! (->Foo 1) inc)"
                       :cljs "(defrecord Foo [x] cljs.core/ISwap (-swap! [this f] (f x)))
                              (swap! (->Foo 1) inc)"))))

    #?(:clj
       (is (= [1 2]
              (eval* #?(:clj "(defrecord Foo [x] clojure.lang.IAtom (reset [this v] [x v]))
                              (reset! (->Foo 1) 2)"))))
       :cljs ::TODO)))

#?(:clj
   (deftest instance-test
     (is (true? (eval* "(instance? clojure.lang.IDeref (atom 0))")))
     (is (true? (eval* "(defrecord Foo [x] clojure.lang.IDeref (deref [this] x))
                        (instance? clojure.lang.IDeref (->Foo 1))")))))

#?(:cljs
   (deftest satisfies-test
     (is (true? (eval* "(satisfies? IDeref (atom 0))")))))
