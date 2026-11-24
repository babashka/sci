(ns sci.core-protocols-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.test-utils :as tu]))

(defn eval* [prog]
  (tu/eval* prog {}))

(deftest deref-test
  (testing "fully qualified"
    (is (= :value
           (eval* #?(:cljd "@(reify cljd.core/IDeref (-deref [_] :value))"
                     :clj  "@(reify clojure.lang.IDeref (deref [_] :value))"
                     :cljs "@(reify cljs.core/IDeref (-deref [_] :value))")))))
  (testing "with import / unqualified"
    (is (= :value
           (eval* #?(:cljd "@(reify IDeref (-deref [_] :value))"
                     :clj  "(import 'clojure.lang.IDeref)
                            @(reify IDeref (deref [_] :value))"
                     :cljs "@(reify IDeref (-deref [_] :value))")))))
  (testing "record implementation"
    (is (= :value
           (eval* #?(:cljd "(defrecord Foo [x] cljd.core/IDeref (-deref [this] x))
                            @(->Foo :value)"
                     :clj  "(defrecord Foo [x] clojure.lang.IDeref (deref [this] x))
                            @(->Foo :value)"
                     :cljs "(defrecord Foo [x] cljs.core/IDeref (-deref [this] x))
                            @(->Foo :value)"))))
    (is (= :value
           (eval* #?(:cljd "(defrecord Foo [x] IDeref (-deref [this] x))
                            @(->Foo :value)"
                     :clj  "(import 'clojure.lang.IDeref)
                            (defrecord Foo [x] IDeref (deref [this] x))
                            @(->Foo :value)"
                     :cljs "(defrecord Foo [x] IDeref (-deref [this] x))
                            @(->Foo :value)"))))))

#?(:cljd nil
   :clj
   (deftest blocking-deref-test
     (testing "defrecord implements IDeref and IBlockingDeref"
       (is (= [:value [10 :timeout] true]
              (eval* "(defrecord Foo [x]
                        clojure.lang.IDeref (deref [_] x)
                        clojure.lang.IBlockingDeref (deref [_ ms v] [ms v]))
                      [@(->Foo :value) (deref (->Foo :value) 10 :timeout)
                       (instance? clojure.lang.IBlockingDeref (->Foo :value))]"))))
     (testing "deftype implements IBlockingDeref before IDeref"
       (is (= [:value [10 :timeout]]
              (eval* "(deftype Foo [x]
                        clojure.lang.IBlockingDeref (deref [_ ms v] [ms v])
                        clojure.lang.IDeref (deref [_] x))
                      [@(Foo. :value) (deref (Foo. :value) 10 :timeout)]"))))
     (testing "reify implements IDeref and IBlockingDeref"
       (is (= [:value [10 :timeout]]
              (eval* "(def r (reify
                               clojure.lang.IDeref (deref [_] :value)
                               clojure.lang.IBlockingDeref (deref [_ ms v] [ms v])))
                      [@r (deref r 10 :timeout)]"))))
     (testing "deref with timeout on a promise returns :timeout"
       (is (= :timeout (eval* "(deref (promise) 1 :timeout)"))))))

#?(:cljd nil
   :clj
   (deftest type-hint-test
     (when-not tu/native?
       (testing "a clojure.lang.IDeref hint on an expression, an arg and a let binding"
         (is (= [1 2 3]
                (tu/eval* "(defn f [x] (.deref ^clojure.lang.IDeref x))
                           (defn g [^clojure.lang.IDeref x] (.deref x))
                           [(f (atom 1)) (g (delay 2)) (let [^clojure.lang.IAtom a (atom 2)] (.swap a inc))]"
                          {:classes {:allow :all}})))))))

(deftest swap-test
  (testing "swap! passes 0 to 4 extra args to f"
    (is (= [1 2 5 11 21]
           (eval* "(let [a (atom 0)]
                     [(swap! a inc) (swap! a + 1) (swap! a + 1 2) (swap! a + 1 2 3) (swap! a + 1 2 3 4)])"))))
  #?(:clj
     (testing "swap-vals! passes 0 to 4 extra args to f"
       (is (= [[0 1] [1 2] [2 5] [5 11] [11 21]]
              (eval* "(let [a (atom 0)]
                        [(swap-vals! a inc) (swap-vals! a + 1) (swap-vals! a + 1 2)
                         (swap-vals! a + 1 2 3) (swap-vals! a + 1 2 3 4)])")))))
  #?(:clj
     (testing "swap! passes extra args to a reify of IAtom"
       (is (= [2 6 9]
              (eval* "(def x (reify clojure.lang.IAtom
                               (swap [_ f] (f 1))
                               (swap [_ f a] (f 1 a))
                               (swap [_ f a b] (f 1 a b))))
                      [(swap! x inc) (swap! x + 5) (swap! x + 5 3)]")))))
  #?(:clj
     (testing "swap! passes the third and later extra args as one seq to the 5-arg swap of a deftype, defrecord and reify"
       (is (= [1 2 5 11 21]
              (eval* "(deftype A [^:volatile-mutable v]
                        clojure.lang.IAtom
                        (swap [_ f] (set! v (f v)))
                        (swap [_ f x] (set! v (f v x)))
                        (swap [_ f x y] (set! v (f v x y)))
                        (swap [_ f x y args] (set! v (apply f v x y args))))
                      (let [a (->A 0)]
                        [(swap! a inc) (swap! a + 1) (swap! a + 1 2) (swap! a + 1 2 3) (swap! a + 1 2 3 4)])")))
       (is (= [10 [10 16]]
              (eval* "(defrecord R [st]
                        clojure.lang.IAtom2
                        (swap [_ f x y args] (apply swap! st f x y args))
                        (swapVals [_ f x y args] (apply swap-vals! st f x y args)))
                      (let [a (->R (atom 0))]
                        [(swap! a + 1 2 3 4) (swap-vals! a + 1 2 3)])")))
       (is (= [[1 2 [3 4]] [1 2 [3]]]
              (eval* "(def x (reify clojure.lang.IAtom2
                               (swap [_ f a b args] [a b args])
                               (swapVals [_ f a b args] [a b args])))
                      [(swap! x + 1 2 3 4) (swap-vals! x + 1 2 3)]")))))
  (testing "fully qualified"
    (is (= 2 (eval* #?(:cljd "(def x (reify cljd.core/ISwap (-swap! [_ f] (f 1)))) (swap! x inc)"
                       :clj "(def x (reify clojure.lang.IAtom (swap [_ f] (f 1)))) (swap! x inc)"
                       :cljs "(def x (reify cljs.core/ISwap (-swap! [_ f] (f 1)))) (swap! x inc)")))))
  (testing "record implementation of swap"
    (is (= 2 (eval* #?(:cljd "(defrecord Foo [x] cljd.core/ISwap (-swap! [this f] (f x)))
                              (swap! (->Foo 1) inc)"
                       :clj "(defrecord Foo [x] clojure.lang.IAtom (swap [this f] (f x)))
                             (swap! (->Foo 1) inc)"
                       :cljs "(defrecord Foo [x] cljs.core/ISwap (-swap! [this f] (f x)))
                              (swap! (->Foo 1) inc)"))))

    #?(:clj
       (is (= [1 2]
              (eval* #?(:clj "(defrecord Foo [x] clojure.lang.IAtom (reset [this v] [x v]))
                              (reset! (->Foo 1) 2)"))))
       :cljs ::TODO)))

#?(:cljd nil
   :clj
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

#?(:clj nil
   :default
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

#?(:cljd nil
   :clj
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

#?(:cljd nil
   :clj
   (deftest instance-test
     (is (true? (eval* "(instance? clojure.lang.IDeref (atom 0))")))
     (is (true? (eval* "(defrecord Foo [x] clojure.lang.IDeref (deref [this] x))
                        (instance? clojure.lang.IDeref (->Foo 1))")))
     (is (true? (eval* "(instance? clojure.lang.IAtom (atom nil))")))
     (is (false? (eval* "(instance? clojure.lang.IAtom 1)")))))

#?(:clj nil
   :default
   (deftest satisfies-test
     (is (true? (eval* "(satisfies? IDeref (atom 0))")))))

#?(:clj nil
   :default
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

#?(:clj nil
   :default
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

#?(:clj nil
   :default
   (deftest ifn-no-false-positive-test
     (testing "reify without IFn is not ifn?"
       (is (false? (eval* "(ifn? (reify IDeref (-deref [_] :val)))"))))
     (testing "reify with IFn is ifn?"
       (is (true? (eval* "(ifn? (reify IFn (-invoke [_] 42)))"))))
     (testing "satisfies? IFn correct for reify"
       (is (false? (eval* "(satisfies? IFn (reify IDeref (-deref [_] :val)))")))
       (is (true? (eval* "(satisfies? IFn (reify IFn (-invoke [_] 42)))"))))))

#?(:cljd nil
   :default
   (deftest inst-test
     (testing "record implementation"
       (is (= 42 (eval* "(defrecord Foo [t] Inst (inst-ms* [_] t)) (inst-ms (->Foo 42))")))
       (is (true? (eval* "(defrecord Foo [t] Inst (inst-ms* [_] t)) (inst? (->Foo 42))"))))
     (testing "deftype implementation"
       (is (= 42 (eval* "(deftype Foo [t] Inst (inst-ms* [_] t)) (inst-ms (->Foo 42))"))))
     (testing "reify implementation"
       (is (= 42 (eval* "(inst-ms (reify Inst (inst-ms* [_] 42)))")))
       (is (true? (eval* "(inst? (reify Inst (inst-ms* [_] 42)))"))))
     (testing "extend-protocol"
       (is (= 42 (eval* "(defrecord Foo [t])
                         (extend-protocol Inst Foo (inst-ms* [x] (:t x)))
                         (inst-ms (->Foo 42))")))
       (is (true? (eval* "(defrecord Foo [t])
                          (extend-protocol Inst Foo (inst-ms* [x] (:t x)))
                          (inst? (->Foo 42))"))))
     (testing "extend-type"
       (is (= 42 (eval* "(defrecord Foo [t])
                         (extend-type Foo Inst (inst-ms* [x] (:t x)))
                         (inst-ms (->Foo 42))"))))
     (testing "host date is unaffected"
       (is (= 1577836800000 (eval* "(inst-ms #inst \"2020\")")))
       (is (true? (eval* "(inst? #inst \"2020\")")))
       (is (false? (eval* "(inst? 1)"))))
     (testing "satisfies?"
       (is (true? (eval* "(defrecord Foo [t] Inst (inst-ms* [_] t)) (satisfies? Inst (->Foo 1))")))
       ;; NOTE: a name that no other test extends Inst to: :satisfies is
       ;; tracked on the shared Inst var by type name, so extending Inst to
       ;; user.Foo in one context is visible from another
       (is (false? (eval* "(defrecord NotAnInst [t]) (satisfies? Inst (->NotAnInst 1))"))))))
