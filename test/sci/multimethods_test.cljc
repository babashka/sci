(ns sci.multimethods-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   #?(:clj [sci.core :as sci])
   #?(:clj [sci.impl.deftype])
   #?(:clj [sci.impl.core-protocols])
   [sci.test-utils :as tu]))

(defn eval* [expr]
  (tu/eval* expr {}))

(deftest default-test
  (is (= :default (eval* "(defmulti foo type) (defmethod foo :default [c] :default) (foo :foo)"))))

(deftest defmethod-test
  (is (= "Hello" (eval* "
(defmulti greeting (fn [x] (x \"language\")))
(defmethod greeting \"English\" [params] \"Hello\") (greeting {\"id\" \"1\", \"language\" \"English\"})"))))

(deftest remove-method-test
  (is (= "Default" (eval* "
(defmulti greeting (fn [x] (x \"language\")))
(defmethod greeting \"English\" [params] \"Hello\")
(defmethod greeting :default [params] \"Default\")
(remove-method greeting \"English\")
(greeting {\"id\" \"1\", \"language\" \"English\"})"))))

#?(:cljd nil
   :clj
   (deftest interface-dispatch-value-test
     (is (= [:deref :deref :default]
            (eval* "
(defmulti kind type)
(defmethod kind clojure.lang.IDeref [_] :deref)
(defmethod kind :default [_] :default)
[(kind (atom 1)) (kind (delay 1)) (kind 1)]")))
     (is (= [true :default]
            (eval* "
(defmulti kind type)
(defmethod kind clojure.lang.IDeref [_] :deref)
(defmethod kind :default [_] :default)
(let [found (some? (get-method kind clojure.lang.IDeref))]
  (remove-method kind clojure.lang.IDeref)
  [found (kind (atom 1))])")))
     (is (= :deref
            (eval* "
(defmulti kind type)
(defmethod kind clojure.lang.IDeref [_] :deref)
(defmethod kind clojure.lang.IAtom [_] :atom)
(prefer-method kind clojure.lang.IDeref clojure.lang.IAtom)
(kind (atom 1))")))))

;; TODO:cljd no hierarchies
#?(:cljd nil :default
(deftest prefer-method-test
  (is (= :rect-shape
         (eval* "
(derive ::rect ::shape)
(defmulti bar (fn [x y] [x y]))
(defmethod bar [::rect ::shape] [x y] :rect-shape)
(defmethod bar [::shape ::rect] [x y] :shape-rect)
(prefer-method bar [::rect ::shape] [::shape ::rect])
(bar ::rect ::rect)")))))

(deftest multi-arity-test
  (is (= [:default :one :two :three :more]
         (eval* "
(defmulti foo (fn [x & _] x))

(defmethod foo :default [_ & _] :default)

;; Like a standar multi-arity function
(defmethod foo :bar
  ([_ _] :one)
  ([_ _ _] :two)
  ([_ _ _ _] :three)
  ([_ _ _ _ & more] :more))

[(foo :baz 1)
 (foo :bar 1)
 (foo :bar 1 2)
 (foo :bar 1 2 3)
 (foo :bar 1 2 3 4)]
"))))

#?(:cljd nil
   :clj
   (deftest internal-multimethods-test
     (when-not tu/native?
       (test/testing "defmethod on clojure.lang/deref for String throws without :unrestricted"
         (is (thrown-with-msg? Exception #"Access to clojure.lang/deref requires the :unrestricted option"
                               (eval* "(defmethod clojure.lang/deref String [_] 42)"))))
       (test/testing "defmethod on clojure.lang/deref for reified objects throws"
         (is (thrown-with-msg? Exception #"clojure.lang/deref"
                               (eval* "(defmethod clojure.lang/deref :sci.impl.protocols/reified [_] 42)"))))
       (test/testing "defmethod on inst-ms* :default throws"
         (is (thrown-with-msg? Exception #"clojure.core/inst-ms\*"
                               (eval* "(defmethod inst-ms* :default [_] 42)"))))
       (test/testing "extend-type String Inst throws"
         (is (thrown-with-msg? Exception #"clojure.core/inst-ms\*"
                               (eval* "(extend-type String Inst (inst-ms* [_] 42))"))))
       (test/testing "remove-all-methods on sci.impl.deftype/toString throws"
         (is (thrown-with-msg? Exception #"sci.impl.deftype/toString"
                               (eval* "(remove-all-methods sci.impl.deftype/toString)"))))
       (test/testing "methods on clojure.lang/deref throws"
         (is (thrown-with-msg? Exception #"clojure.lang/deref"
                               (eval* "(methods clojure.lang/deref)"))))
       (test/testing "deftype with IDeref and toString returns [1 \"t\"]"
         (is (= [1 "t"] (eval* "(deftype T [x] clojure.lang.IDeref (deref [_] x) Object (toString [_] \"t\")) [@(T. 1) (str (T. 1))]"))))
       (test/testing "defrecord with Inst returns 3 from inst-ms"
         (is (= 3 (eval* "(defrecord R [t] Inst (inst-ms* [_] t)) (inst-ms (->R 3))"))))
       (test/testing "defmethod on clojure.lang/deref for String returns 42 with :unrestricted"
         (is (= 42 (tu/eval* "(defmethod clojure.lang/deref String [_] 42) (let [r (deref \"x\")] (remove-method clojure.lang/deref String) r)"
                             {:unrestricted true})))))))

#?(:cljd nil
   :clj
   (deftest type-methods-test
     (when-not tu/native?
       (test/testing "deftype in 3 contexts leaves the size of the toString and deref method tables unchanged"
         (let [sizes #(mapv (comp count methods) [sci.impl.deftype/to-string sci.impl.core-protocols/deref])
               before (sizes)]
           (dotimes [_ 3]
             (sci/eval-string "(deftype T [x] clojure.lang.IDeref (deref [_] x) Object (toString [_] \"t\")) [@(T. 1) (str (T. 1))]"))
           (is (= before (sizes)))))
       (test/testing "remove-method on a deftype's toString falls back to the default"
         (is (= [true false]
                (tu/eval* "(deftype T [] Object (toString [_] \"t\"))
(let [a (= \"t\" (str (T.)))]
  (remove-method sci.impl.deftype/toString T)
  [a (= \"t\" (str (T.)))])" {})))))))
