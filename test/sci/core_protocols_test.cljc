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

#?(:cljs
   (deftest ifn-reify-test
     (testing "reify IFn with single arity"
       (is (= "called" (eval* "(def x (reify IFn (-invoke [_] \"called\"))) (x)"))))
     (testing "reify IFn with multiple arities"
       (is (= [0 1 2] (eval* "(def x (reify IFn
                                        (-invoke [_] 0)
                                        (-invoke [_ a] a)
                                        (-invoke [_ a b] (+ a b))))
                               [(x) (x 1) (x 1 1)]"))))
     (testing "reify IFn and IDeref together"
       (is (= [:deref :invoke]
              (eval* "(def x (reify IDeref (-deref [_] :deref)
                                    IFn (-invoke [_] :invoke)))
                      [@x (x)]"))))))

#?(:cljs
   (deftest ifn-deftype-test
     (testing "deftype with IFn"
       (is (= 42 (eval* "(deftype Foo [f]
                            IFn
                            (-invoke [_] (f)))
                          (def foo (->Foo (fn [] 42)))
                          (foo)"))))
     (testing "deftype with IFn multiple arities"
       (is (= [0 1 3]
              (eval* "(deftype Foo [f]
                        IFn
                        (-invoke [_] (f))
                        (-invoke [_ a] (f a))
                        (-invoke [_ a b] (f a b)))
                      (def foo (->Foo +))
                      [(foo) (foo 1) (foo 1 2)]"))))
     (testing "deftype with IFn and IDeref"
       (is (= [:val :invoked]
              (eval* "(deftype LazyVar [f]
                        IDeref (-deref [_] (f))
                        IFn (-invoke [_] :invoked))
                      (def lv (->LazyVar (fn [] :val)))
                      [@lv (lv)]"))))
     (testing "ifn? false for deftype without IFn"
       (is (false? (eval* "(deftype Dude []) (ifn? (->Dude))"))))))

#?(:cljs
   (deftest ifn-no-false-positive-test
     (testing "reify without IFn is not ifn?"
       (is (false? (eval* "(ifn? (reify IDeref (-deref [_] :val)))"))))
     (testing "reify with IFn is ifn?"
       (is (true? (eval* "(ifn? (reify IFn (-invoke [_] 42)))"))))
     (testing "satisfies? IFn correct for reify"
       (is (false? (eval* "(satisfies? IFn (reify IDeref (-deref [_] :val)))")))
       (is (true? (eval* "(satisfies? IFn (reify IFn (-invoke [_] 42)))"))))))
