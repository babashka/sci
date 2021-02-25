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
   (deftest multi-arity-swap-test
     (let [prog "
(defrecord Example []
  clojure.lang.IDeref
  (deref [this] :deref)
  clojure.lang.IAtom
  (reset [this new-value] :reset)
  (swap  [this f]          :swap1)
  (swap  [this f a]        :swap2)
  (swap  [this f a b]      :swap3)
  (swap  [this f a b args] :swap4)
  (compareAndSet [this oldv newv] :compare-and-set))
[@(->Example)
 (reset! (->Example) 1)
 (swap! (->Example) inc)
 (swap! (->Example) + 1)
 (swap! (->Example) + 1 2)
 (swap! (->Example) + 1 2 3)
 (compare-and-set! (->Example) 1 2)]"]
       (is (= [:deref :reset :swap1 :swap2 :swap3 :swap4 :compare-and-set]
              (eval* prog))))))

#?(:cljs
   (deftest multi-arity-swap-test
     (let [prog "
(defrecord Example []
  IDeref
  (-deref [this] :deref)
  IReset
  (-reset! [this new-value] :reset)
  ISwap
  (-swap!  [this f]          :swap1)
  (-swap!  [this f a]        :swap2)
  (-swap!  [this f a b]      :swap3)
  (-swap!  [this f a b args] :swap4))
[@(->Example)
 (reset! (->Example) 1)
 (swap! (->Example) inc)
 (swap! (->Example) + 1)
 (swap! (->Example) + 1 2)
 (swap! (->Example) + 1 2 3)]"]
       (is (= [:deref :reset :swap1 :swap2 :swap3 :swap4]
              (eval* prog))))))

#?(:clj
   (deftest iatom2-test
     (let [prog "
(defrecord Example [x]
  clojure.lang.IAtom2
  (swapVals [this f] [:swap-vals (f x)])
  (resetVals [this y] [:reset-vals x y]))
[(reset-vals! (->Example 1) 2)
 (swap-vals! (->Example 1) inc)]"]
       (is (= [[:reset-vals 1 2] [:swap-vals 2]]
              (eval* prog))))))

#?(:clj
   (deftest instance-test
     (is (true? (eval* "(instance? clojure.lang.IDeref (atom 0))")))
     (is (true? (eval* "(defrecord Foo [x] clojure.lang.IDeref (deref [this] x))
                        (instance? clojure.lang.IDeref (->Foo 1))")))
     (is (true? (eval* "(instance? clojure.lang.IAtom (atom nil))")))
     (is (false? (eval* "(instance? clojure.lang.IAtom 1)")))))

#?(:cljs
   (deftest satisfies-test
     (is (true? (eval* "(satisfies? IDeref (atom 0))")))))
