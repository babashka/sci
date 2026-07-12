(ns sci.native-protocols-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.protocol-lib :as plib]))

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

(def plib-sci-ns (sci/create-ns 'sci.protocol-lib))

(def plib-opts
  {:namespaces {'sci.protocol-lib (sci/copy-ns sci.protocol-lib plib-sci-ns)}})

(deftest copy-ns-protocol-test
  (testing "copy-ns turns protocol publics into protocol entries"
    (let [v (sci/eval-string "
(require '[sci.protocol-lib :as plib])
(deftype T [w]
  plib/IShout
  (shout [_] (str w \"!\"))
  (shout2 [_ n] (str w n)))
(->T \"hey\")" plib-opts)]
      (testing "host-side protocol fn dispatches into sci impl"
        (is (= "hey!" (plib/shout v)))
        (is (= "hey3" (plib/shout2 v 3)))
        (is (= "hey!" (plib/shout-loudly v)))
        (is (true? (satisfies? plib/IShout v)))
        (is (false? (satisfies? plib/IMarker v))))))
  (testing "sci-side satisfies?, marker protocol, extend-type"
    (is (= [true "yo!" true]
           (sci/eval-string "
(require '[sci.protocol-lib :as plib])
(deftype T [] plib/IShout (shout [_] \"yo!\") (shout2 [_ n] n))
(deftype M [])
(extend-type M plib/IMarker)
[(satisfies? plib/IShout (->T)) (plib/shout (->T)) (satisfies? plib/IMarker (->M))]"
                            plib-opts))))
  (testing "non-protocol publics still copied as plain vars"
    (is (fn? (sci/eval-string "
(require '[sci.protocol-lib :as plib]) plib/shout-loudly" plib-opts)))))

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

(deftest reify-native-protocol-test
  (testing "host side"
    (let [v (eval* "
(reify
  ILookup
  (-lookup [_ k] [:got k])
  (-lookup [_ k nf] nf)
  ICounted
  (-count [_] 42))")]
      (is (= [:got :x] (get v :x)))
      (is (= :nf (get v :missing :nf)))
      (is (= 42 (count v)))
      (is (true? (satisfies? ILookup v)))
      (is (false? (satisfies? IAssociative v)))))
  (testing "sci side, mixed with a sci protocol"
    (is (= [[:got :x] 1 true true :bar]
           (eval* "
(defprotocol Bar (bar [_]))
(def r (reify
         ILookup
         (-lookup [_ k] [:got k])
         ICounted
         (-count [_] 1)
         Bar
         (bar [_] :bar)))
[(get r :x) (count r) (satisfies? ILookup r) (satisfies? Bar r) (bar r)]"))))
  (testing "partial implementation installs only implemented arities"
    (let [v (eval* "(reify ILookup (-lookup [_ k] :two))")]
      (is (= :two (get v :x)))))
  (testing "closed-over locals"
    (let [v (eval* "(let [x 10] (reify ICounted (-count [_] x)))")]
      (is (= 10 (count v))))))

(deftest defrecord-native-protocol-test
  (testing "custom protocol on a record, host side, map behavior intact"
    (let [v (sci/eval-string "
(require '[sci.protocol-lib :as plib])
(defrecord R [w]
  plib/IShout
  (shout [_] (str w \"!\"))
  (shout2 [this n] (str (:w this) n)))
(->R \"hey\")" plib-opts)]
      (is (= "hey!" (plib/shout v)))
      (is (= "hey3" (plib/shout2 v 3)))
      (is (true? (satisfies? plib/IShout v)))
      (is (= "hey" (:w v)))
      (testing "assoc/dissoc/with-meta/clone preserve native slots"
        (is (= "hey!" (plib/shout (assoc v :x 1))))
        (is (= 1 (:x (assoc v :x 1))))
        (is (= "hey!" (plib/shout (dissoc (assoc v :x 1) :x))))
        (is (= "hey!" (plib/shout (with-meta v {:m 1}))))
        (is (= {:m 1} (meta (with-meta v {:m 1})))))))
  (testing "shadowing a SciRecord class-level protocol"
    (is (= 42 (eval* "(defrecord R [] ICounted (-count [_] 42)) (count (->R))")))
    (testing "other records keep the default"
      (is (= 2 (eval* "
(defrecord R [] ICounted (-count [_] 42))
(defrecord S [a])
(count (assoc (->S 1) :b 2))")))))
  (testing "sci side + extend-type on a record type, retroactive"
    (is (= ["ext" true]
           (sci/eval-string "
(require '[sci.protocol-lib :as plib])
(defrecord R [])
(def r (->R))
(extend-type R
  plib/IShout
  (shout [_] \"ext\")
  (shout2 [_ n] n))
[(plib/shout r) (satisfies? plib/IShout r)]" plib-opts)))))

(deftest core-protocols-native-host-side-test
  (testing "IDeref: host @ on sci deftype and reify, no opts needed"
    (is (= 42 @(sci/eval-string "(deftype Box [x] IDeref (-deref [_] x)) (->Box 42)" nil)))
    (is (= :r @(sci/eval-string "(reify IDeref (-deref [_] :r))" nil))))
  (testing "ISwap/IReset: host swap! and reset! on a sci deftype"
    (let [v (sci/eval-string "
(deftype Cell [^:mutable v]
  ISwap
  (-swap! [_ f] (set! v (f v)) v)
  IReset
  (-reset! [_ nv] (set! v nv) v))
(->Cell 1)" nil)]
      (is (= 2 (swap! v inc)))
      (is (= 9 (reset! v 9)))))
  (testing "IPrintWithWriter: host pr-str on a sci deftype"
    (is (= "#P[]" (pr-str (sci/eval-string "
(deftype P []
  IPrintWithWriter
  (-pr-writer [_ w _] (-write w \"#P[]\")))
(->P)" nil)))))
  (testing "sci-side deref/swap!/reset! still work"
    (is (= [42 2 9]
           (sci/eval-string "
(deftype Box [x] IDeref (-deref [_] x))
(deftype Cell [^:mutable v]
  ISwap
  (-swap! [_ f] (set! v (f v)) v)
  IReset
  (-reset! [_ nv] (set! v nv) v))
(let [c (->Cell 1)]
  [@(->Box 42) (swap! c inc) (reset! c 9)])" nil))))
  (testing "host atoms unaffected"
    (is (= 2 (sci/eval-string "(let [a (atom 1)] (swap! a inc) @a)" nil)))))

(deftest default-protocols-test
  (testing "cljs.core protocols are available by default, no opts"
    (is (= [1 :nf 2 false]
           (sci/eval-string "
(deftype B [m]
  ILookup
  (-lookup [_ k] (get m k))
  (-lookup [_ k nf] (get m k nf))
  ICounted
  (-count [_] (count m))
  IAssociative
  (-contains-key? [_ k] (contains? m k))
  (-assoc [_ k v] (->B (assoc m k v))))
(def b (->B {:a 1}))
[(get b :a) (get b :x :nf) (count (assoc b :b 2)) (satisfies? ISeqable b)]" nil))))
  (testing "transients"
    (is (= [1] (sci/eval-string "
(deftype T [^:mutable v]
  ITransientCollection
  (-conj! [this x] (set! v (conj v x)) this)
  (-persistent! [_] v))
(deftype E [v]
  IEditableCollection
  (-as-transient [_] (->T v)))
(persistent! (conj! (transient (->E [])) 1))" nil))))
  (testing "INamed drives host name/namespace"
    (let [v (sci/eval-string "
(deftype N []
  INamed
  (-name [_] \"nm\")
  (-namespace [_] \"nsp\"))
(->N)" nil)]
      (is (= "nm" (name v)))
      (is (= "nsp" (namespace v)))))
  (testing "IHash drives host hash"
    (is (= 42 (hash (sci/eval-string "(deftype H [] IHash (-hash [_] 42)) (->H)" nil)))))
  (testing "Inst drives inst-ms"
    (is (= 42 (sci/eval-string "(deftype I [] Inst (inst-ms* [_] 42)) (inst-ms (->I))" nil))))
  (testing "marker protocol via extend-type"
    (is (true? (sci/eval-string "
(deftype M [])
(extend-type M IList)
(satisfies? IList (->M))" nil)))))

(deftest default-protocol-var-name-test
  (testing "default entries have plain var names like copy-var, so consumers
  building symbols from the var name resolve them"
    (is (= 'ICounted (sci/eval-string "(:name (meta #'ICounted))" nil)))))

(deftest protocol-method-vars-test
  (testing "protocol method fns are exposed as vars in sci"
    (is (= 1 (sci/eval-string "(-lookup {:a 1} :a)" nil)))
    (is (= 3 (sci/eval-string "(-count [1 2 3])" nil)))
    (is (= 2 (sci/eval-string "(deftype B [] ILookup (-lookup [_ k] 2)) (-lookup (->B) :x)" nil)))
    (is (= "x" (sci/eval-string "(deftype N [] INamed (-name [_] \"x\") (-namespace [_] nil)) (-name (->N))" nil)))))

(deftest extend-fn-native-test
  (testing "extend (the fn) with native protocols"
    (is (= [:looked 2]
           (sci/eval-string "
(deftype T [])
(extend T
  ILookup {:-lookup (fn [_ k] :looked)}
  ICounted {:-count (fn [_] 2)})
(def t (->T))
[(get t :x) (count t)]" nil)))))
