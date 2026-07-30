(ns sci.core-protocols-test
  (:require [clojure.test :refer [deftest is testing]]
            #?(:clj [sci.core :as sci])
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

(deftest swap-test
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

#?(:clj
   (defrecord HostSideInst [ms]))

#?(:clj
   (deftest inst-host-protocol-test
     (testing "an unrestricted context reroutes the host protocol method, so
               compiled host code dispatches into sci implementations"
       (let [ctx (sci/init {:unrestricted true})]
         (is (= 42 (inst-ms (sci/eval-string* ctx "(defrecord Hosted [t] Inst (inst-ms* [_] t)) (->Hosted 42)"))))
         (is (= 7 (inst-ms (sci/eval-string* ctx "(reify Inst (inst-ms* [_] 7))"))))
         (is (= 9 (inst-ms (sci/eval-string* ctx "(deftype HostedT [t] Inst (inst-ms* [_] t)) (->HostedT 9)"))))
         (is (thrown-with-msg?
              IllegalArgumentException #"No implementation of method"
              (inst-ms (sci/eval-string* ctx "(defrecord Unhosted [t]) (->Unhosted 1)"))))
         (testing "host types keep working after the reroute"
           (is (= 1577836800000 (inst-ms #inst "2020")))
           (is (true? (inst? #inst "2020"))))
         (testing "host satisfies? is class-keyed and cannot see sci
                   implementations, but it does not over-report either"
           (is (false? (inst? (sci/eval-string* ctx "(defrecord Hosted [t] Inst (inst-ms* [_] t)) (->Hosted 1)")))))
         (testing "a host-side extend rebinds the method var; the next sci
                   implementation repairs the reroute and keeps the host
                   extension visible"
           (clojure.core/extend-protocol Inst HostSideInst (inst-ms* [r] (:ms r)))
           (let [hosted (sci/eval-string* ctx "(defrecord Repaired [t] Inst (inst-ms* [_] t)) (->Repaired 42)")]
             (is (= 42 (inst-ms hosted)))
             (is (= 5 (inst-ms (->HostSideInst 5))))
             (is (= 1577836800000 (inst-ms #inst "2020")))))))
     (testing "a restricted context keeps working through sci's own inst-ms"
       (let [ctx (sci/init {})]
         (is (= 5 (sci/eval-string* ctx "(defrecord Sandboxed [t] Inst (inst-ms* [_] t)) (inst-ms (->Sandboxed 5))")))))))
