(ns sci.records-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.test-utils :as tu]
            [clojure.string :as str]))

(deftest protocol-test
  (let [prog "
(defrecord Foo [a b])
(let [r1 (->Foo 1 2)
      r2 (map->Foo {:a 1 :b 2})]
  [(:a r1) (:b r1) (:a r2) (:b r2)])"]
    (is (= [1 2 1 2] (tu/eval* prog {}))))
  (testing "protocols"
    (let [prog "
(ns foo)
(defprotocol Foo (foo [_] 42))
(defprotocol Graph (graph [_]))

(ns bar (:require [foo :as f]))
(defrecord FooRecord [a b]
  f/Foo (foo [_] (dec a)))

(defrecord BarRecord [a b]
  f/Foo (foo [_] (inc b)))

(extend FooRecord
  f/Graph {:graph (fn [x] {:from (:a x) :to (:b x)})})

(let [a (->FooRecord 1 2) b (BarRecord. 1 2)]
  [(f/foo a) (f/foo b) (f/graph a) (satisfies? f/Graph a)])"]
      (is (= [0 3 {:from 1, :to 2} true] (tu/eval* prog {}))))))

(deftest destructuring-protocol-fn-test
  (let [prog "
(defprotocol Foo (sayhello [_ name] \"print a name\"))
(defrecord Greeter [state] Foo (sayhello [{:keys [state]} name] [state name]))
(sayhello (Greeter. \"test\") \"john\")"]
    (is (= ["test" "john"] (tu/eval* prog {})))))

(deftest shadow-record-field-protocol-fn-test
  (let [prog "
(defprotocol Foo (sayhello [_ name] \"print a name\"))
(defrecord Greeter [x y] Foo (sayhello [_ x] x))
(sayhello (Greeter. \"x\" \"y\") \"john\")"]
    (is (= "john" (tu/eval* prog {}))))
  (let [prog "
(defprotocol Foo (sayhello [_ name] \"print a name\"))
(defrecord Greeter [x y] Foo (sayhello [x _] x))
(into {} (sayhello (Greeter. \"x\" \"y\") \"john\"))"]
    (is (= {:x "x" :y "y"} (tu/eval* prog {}))))
  (testing "protocol impl arg shadows this arg (the first one)"
    (let [prog "
(defprotocol Foo (sayhello [_ name] \"print a name\"))
(defrecord Greeter [x y] Foo (sayhello [_ _] x))
(sayhello (Greeter. \"x\" \"y\") \"john\")"]
      (is (= "x" (tu/eval* prog {}))))))

(deftest extends?-test
  (let [prog "
(defprotocol Area (get-area [this]))
(defrecord Rectangle [width height]
                  Area
                  (get-area [this]
                    (* width height)))
(extends? Area Rectangle)"]
    (is (true? (tu/eval* prog {})))))

(deftest multiple-functions-test
  (let [prog "
(defprotocol Area (get-area [this])
                  (get-perimeter [this]))
(defrecord Rectangle [width height]
                  Area
                  (get-area [this]
                    (* width height))
                  (get-perimeter [this]
                    (+ (* 2 width) (* 2 height))))
[(get-perimeter (Rectangle. 10 10)) (get-area (Rectangle. 10 10))]"]
    (is (= [40 100] (tu/eval* prog {})))))

(deftest instance-test
  (let [prog "
(defrecord Rectangle [width height])
(def x (Rectangle. 0 0))
(instance? Rectangle x)"]
    (is (true? (tu/eval* prog {})))))

(deftest record?-test
  (let [prog "
(defrecord Rectangle [width height])
(record? (->Rectangle 0 0))"]
    (is (true? (tu/eval* prog {})))))

(deftest import-test
  (let [prog "
(ns foo)
(defrecord Rectangle [width height])

(ns bar
  (:require [foo])
  (:import [foo Rectangle]))

(instance? Rectangle (foo/->Rectangle 10 10))"]
    (is (true? (tu/eval* prog {})))))

(deftest field-access-test
  (let [prog "
(ns foo)
(defrecord Rectangle [width height])

(defn width [^Rectangle rect]
  (.-width rect))

(width (Rectangle. 10 10))"]
    (is (= 10 (tu/eval* prog {}))))
  (let [prog "(ns foo) (defrecord Rectangle [width height]) (ns bar (:import [foo Rectangle])) (defn width [^Rectangle rect] (.-width rect)) (def rect (Rectangle. 10 10)) (width rect)"]
    (is (= 10 (tu/eval* prog {})))))

(deftest constructor-test
  (let [prog "
(ns foo)
(defprotocol Area (area [_]))
(defrecord Rectangle [width height]
  Area
  (area [_] (* width height)))

(ns bar (:require [foo]) (:import [foo Rectangle]))
(let [a 6 b 2
      rect (Rectangle. a b)]
  (foo/area rect))"]
    (is (= 12 (tu/eval* prog {}))))
  (testing "constructor can be used in protocol impls"
    (is (= {:x 1}
           (tu/eval*
            "(defprotocol IFoo (foo [this]))
             (defrecord Foo [x] IFoo (foo [this] (Foo. x)))
             (into {} (foo (Foo. 1)))" {})))))

(deftest namespace-test
  (let [prog "
(ns foo) (defrecord Foo []) (ns bar (:import [foo Foo])) Foo"]
    (is (= 'foo/Foo (tu/eval* prog {})))))

(deftest type-test
  (let [prog "
(ns foo) (defrecord Foo []) (= foo.Foo (type (->Foo)))"]
    (is (true? (tu/eval* prog {}))))
  (let [prog "
(ns foo) (defrecord Foo []) (= 'foo/Foo (type (->Foo)))"]
    (is (true? (tu/eval* prog {})))))

(deftest derive-test
  (let [prog "
(ns foo) (defrecord Foo []) (derive Foo ::bar) (isa? (type (Foo.)) ::bar)"]
    (is (true? (tu/eval* prog {}))))
  (let [prog "
(ns foo) (defrecord Foo []) (derive Foo ::bar) (isa? (type {}) ::bar)"]
    (is (false? (tu/eval* prog {})))))

(deftest to-string-test
  (let [prog "(defrecord A [x] Object (toString [x] (str \"ARecord@\" (into {} x)))) (str (->A 1))"]
    (is (= "ARecord@{:x 1}" (tu/eval* prog {})))))

(deftest roundtrip-test
  (let [prog "(ns foo) (defrecord A [x y z]) (str [#foo.A{:x 1 :y 2 :z 3} (read-string \"#foo.A{:x 1 :y 2 :z 3}\")])"]
    (is (= "[#foo.A{:x 1, :y 2, :z 3} #foo.A{:x 1, :y 2, :z 3}]" (tu/eval* prog {})))))
