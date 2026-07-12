(ns sci.native-protocols-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]))

(def cljs-core-ns (sci/create-ns 'cljs.core))

(def protocol-entries
  {'ILookup (sci/copy-var ILookup cljs-core-ns)
   'IAssociative (sci/copy-var IAssociative cljs-core-ns)
   'ICounted (sci/copy-var ICounted cljs-core-ns)
   'IEquiv (sci/copy-var IEquiv cljs-core-ns)})

(def opts
  {:namespaces {'clojure.core protocol-entries}})

(defn eval* [s]
  (sci/eval-string s opts))

(deftest copy-var-protocol-entry-test
  (testing "copy-var wraps a protocol entry in a sci var, sharing the ns object"
    (is (identical? cljs-core-ns (:ns @(get protocol-entries 'ILookup))))
    (is (identical? (:ns @(get protocol-entries 'ILookup))
                    (:ns @(get protocol-entries 'IAssociative))))
    (is (identical? ILookup (:protocol @(get protocol-entries 'ILookup)))))
  (testing "IFn is not turned into a protocol entry"
    (is (identical? IFn @(sci/copy-var IFn cljs-core-ns)))))

(deftest deftype-native-protocol-host-side-test
  (let [v (eval* "
(deftype Foo [m]
  ILookup
  (-lookup [this k] (get m k))
  (-lookup [this k not-found] (get m k not-found)))
(->Foo {:a 1})")]
    (testing "host get goes through the native protocol"
      (is (= 1 (get v :a)))
      (is (nil? (get v :b)))
      (is (= :nf (get v :b :nf))))
    (testing "host satisfies?/implements? see the protocol marker"
      (is (true? (satisfies? ILookup v)))
      (is (true? (implements? ILookup v)))
      (is (false? (satisfies? IAssociative v))))))

(deftest deftype-native-protocol-sci-side-test
  (is (= [1 :nf true false]
         (eval* "
(deftype Foo [m]
  ILookup
  (-lookup [this k] (get m k))
  (-lookup [this k not-found] (get m k not-found)))
(def f (->Foo {:a 1}))
[(get f :a) (get f :b :nf) (satisfies? ILookup f) (satisfies? IAssociative f)]"))))

(deftest deftype-native-protocol-fields-test
  (is (= [2 :x0 :y1]
         (eval* "
(deftype Pt [x y]
  ICounted
  (-count [_] 2)
  ILookup
  (-lookup [_ k] (if (= k 0) x y)))
(def p (->Pt :x0 :y1))
[(count p) (get p 0) (get p 1)]"))))

(deftest deftype-native-protocol-mutable-field-test
  (let [v (eval* "
(deftype Counter [^:mutable n]
  ICounted
  (-count [_] (set! n (inc n)) n))
(->Counter 0)")]
    (is (= 1 (count v)))
    (is (= 2 (count v)))))

(deftest deftype-native-protocol-assoc-test
  (let [v (eval* "
(deftype Foo [m]
  IAssociative
  (-contains-key? [this k] (contains? m k))
  (-assoc [this k val] (->Foo (assoc m k val)))
  ILookup
  (-lookup [this k] (get m k)))
(->Foo {:a 1})")]
    (is (= 2 (get (assoc v :b 2) :b)))
    (is (true? (contains? v :a)))
    (is (= [1 2] (eval* "
(deftype Foo [m]
  IAssociative
  (-contains-key? [this k] (contains? m k))
  (-assoc [this k val] (->Foo (assoc m k val)))
  ILookup
  (-lookup [this k] (get m k)))
(def f (assoc (->Foo {:a 1}) :b 2))
[(get f :a) (get f :b)]")))))

(deftest extend-type-native-protocol-test
  (testing "extend-type after deftype affects existing instances"
    (let [v (eval* "
(deftype Foo [m])
(def f (->Foo {:a 1}))
(extend-type Foo
  ILookup
  (-lookup [this k] :extended))
f")]
      (is (= :extended (get v :a)))
      (is (true? (satisfies? ILookup v)))))
  (testing "sci-side"
    (is (= [:extended true]
           (eval* "
(deftype Foo [m])
(def f (->Foo {:a 1}))
(extend-type Foo
  ILookup
  (-lookup [this k] :extended))
[(get f :a) (satisfies? ILookup f)]")))))

(deftest extend-protocol-native-protocol-test
  (let [v (eval* "
(deftype Foo [m])
(def f (->Foo {:a 1}))
(extend-protocol ILookup
  Foo
  (-lookup [this k] :extended))
f")]
    (is (= :extended (get v :a)))))

(deftest native-protocol-in-sandboxed-ctx-test
  (testing "no :classes opt needed, providing the protocol entry is the opt-in"
    (is (= 1 (get (sci/eval-string "(deftype Foo [] ILookup (-lookup [this k] 1)) (->Foo)"
                                   {:namespaces {'clojure.core protocol-entries}})
                  :x)))))

(deftest native-and-sci-protocol-coexist-test
  (is (= [:bar :x true true]
         (eval* "
(defprotocol Bar (bar [_]))
(deftype Foo []
  Bar
  (bar [_] :bar)
  ILookup
  (-lookup [this k] k))
(def f (->Foo))
[(bar f) (get f :x) (satisfies? Bar f) (satisfies? ILookup f)]"))))

(deftest native-iequiv-host-equality-test
  (testing "host = dispatches into a natively implemented -equiv"
    (let [[a b c] (eval* "
(deftype Box [v]
  IEquiv
  (-equiv [_ o] (and (instance? Box o) (= v (.-v o)))))
[(->Box 1) (->Box 1) (->Box 2)]")]
      (is (= a b))
      (is (not= a c)))))

(deftest native-method-not-on-protocol-test
  (is (thrown-with-msg?
       js/Error #"not found"
       (eval* "(deftype Foo [] ILookup (-lookupX [this k] k))"))))

(deftest redefine-native-method-via-extend-type-test
  (is (= :new
         (eval* "
(deftype Foo [] ILookup (-lookup [_ k] :old))
(def f (->Foo))
(extend-type Foo ILookup (-lookup [_ k] :new))
(get f :k)"))))

(deftest multi-type-extend-protocol-native-test
  (is (= [1 2]
         (eval* "
(deftype A [])
(deftype B [])
(extend-protocol ICounted
  A (-count [_] 1)
  B (-count [_] 2))
[(count (->A)) (count (->B))]"))))

(deftest non-top-level-deftype-test
  (is (= 1 (eval* "
(when true
  (deftype Foo [m]
    ILookup
    (-lookup [this k] (get m k)))
  (get (->Foo {:a 1}) :a))"))))

(deftest deftype-extended-later-test
  (testing "extend-type reaches instances created before the extension (the
  Type object is created at analysis time and adopted by -create-type, so
  every reference is the same object)"
    (is (= 7 (eval* "
(do
  (deftype Foo [])
  (def inst (->Foo))
  (extend-type Foo ICounted (-count [_] 7))
  (count inst))")))))

(deftest reify-native-protocol-not-supported-test
  (is (thrown-with-msg?
       js/Error #"not yet supported"
       (eval* "(reify ILookup (-lookup [this k] k))"))))

(deftest defrecord-native-protocol-not-supported-test
  (is (thrown-with-msg?
       js/Error #"not yet supported"
       (eval* "(defrecord Foo [] ICounted (-count [_] 1))")))
  (is (thrown-with-msg?
       js/Error #"deftype"
       (eval* "(defrecord Foo []) (extend-type Foo ICounted (-count [_] 1))"))))
