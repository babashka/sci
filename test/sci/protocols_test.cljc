(ns sci.protocols-test
  (:require [clojure.core.protocols :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.test-utils :as tu]))

(deftest protocol-test
  (let [prog "
(ns foo)
(defprotocol AbstractionA
  (foo [obj])
  (bar [obj]))

(ns bar)
(defprotocol AbstractionB
  \"A cool protocol\"
  (fooB [obj x]))

(ns baz)
(require '[foo :as f :refer [AbstractionA]]
         '[bar :refer [AbstractionB fooB]])

(extend Number AbstractionA
  {:foo (fn [_] :number)
   :bar (fn [_] :bar/number)})

(extend-protocol AbstractionA
  nil
  (foo [s] (str \"foo-A!\"))
  (bar [s] (str \"bar-A!\"))
  String
  (foo [s] (str \"foo-A-\" (.toUpperCase s)))
  (bar [s] (str \"bar-A-\" (.toUpperCase s))))

(extend-type Object
  AbstractionA
  (foo [_] :foo/object)
  (bar [_] :bar/object)
  AbstractionB
  (fooB [_ x] x))

[(f/foo nil)
 (f/bar nil)
 (f/foo \"Bar\")
 (f/bar \"Bar\")
 (f/foo 1)
 (f/bar 1)
 (f/foo {})
 (f/bar {})
 (fooB {} :fooB/object)
 (satisfies? AbstractionA 1)]"
        prog #?(:clj prog
                :cljs (-> prog
                          (str/replace "String" "js/String")
                          (str/replace "Number" "js/Number")
                          (str/replace "Object" ":default")))]
    (is (= ["foo-A!"
            "bar-A!"
            "foo-A-BAR"
            "bar-A-BAR"
            :number
            :bar/number
            :foo/object
            :bar/object
            :fooB/object
            true]
           (tu/eval* prog #?(:clj {}
                             :cljs {:classes {:allow :all
                                              'js #js {:String js/String
                                                       :Number js/Number}}}))))))


(defn eval* [prog]
  (tu/eval* #?(:clj prog
               :cljs (str/replace prog "Object" ":default"))
            #?(:clj {}
               :cljs {:classes {:allow :all
                                'js #js {:String js/String}}})))

(deftest docstring-test
  (is (= "-------------------------\nuser/Foo\n  cool protocol\n" (tu/eval* "
(defprotocol Foo \"cool protocol\" (foo [_]))
(with-out-str (clojure.repl/doc Foo))
" {}))))

(deftest reify-test
  (let [prog "
(defprotocol Fruit (subtotal [item]))
(def x (reify Fruit (subtotal [_] 1)))
(subtotal x)"]
    (is (= 1 (tu/eval* prog {})))))

(deftest extends-test
  (let [prog "
(defprotocol Area (get-area [this]))
(extend-type String Area (get-area [_] 0))
(extends? Area String)"
        prog #?(:clj prog
                :cljs (-> prog
                          (str/replace "String" "js/String")))]
    (is (true? (eval* prog))))
  (testing "Aliases are allowed and ignored"
    (testing "extent-type"
      (let [prog "
(ns foo) (defprotocol Foo (foo [this]))
(ns bar (:require [foo :as f]))

(extend-type String
  f/Foo
  (f/foo [this] (subs this 0 1)))

(= \"f\" (f/foo \"foo\"))
"
            prog #?(:clj prog
                    :cljs (-> prog
                              (str/replace "String" "js/String")))]
        (is (true? (eval* prog)))))
    (testing "extend-protocol"
      (let [prog "
(ns foo) (defprotocol Foo (foo [this]))
(ns bar (:require [foo :as f]))

(extend-protocol f/Foo
  String
  (f/foo [this] (subs this 0 1)))

(= \"f\" (f/foo \"foo\"))
"
            prog #?(:clj prog
                    :cljs (-> prog
                              (str/replace "String" "js/String")))]
        (is (true? (eval* prog)))))))

(deftest extend-via-metadata-test
  (let [prog "
(defprotocol Foo :extend-via-metadata true
  (foo [_]))

(def x (with-meta {} {`foo (fn [_] 1)}))
(foo x)"]
    (is (= 1 (tu/eval* prog {}))))
  (let [prog "
(defprotocol Foo \"docstring\"
  :extend-via-metadata true
  (foo [_]))

(def x (with-meta {} {`foox (fn [_] 1)}))
(foo x)"]
    (is (thrown-with-msg? #?(:clj Exception
                             :cljs js/Error)
                          #"No implementation of method: :foo of protocol: #'user/Foo found for"
                          (tu/eval* prog {})))))

(deftest multi-arity-test
  (let [prog (fn [expr]
               (str/replace "
(defprotocol IFruit (subtotal [item] [item subtotal]))
(defrecord Apple [price] IFruit (subtotal [_] price) (subtotal [_ discount] (- price discount)))
{{expr}}
[(subtotal (->Apple 100)) (subtotal (->Apple 100) 5) (subtotal \"foo\") (subtotal \"foo\" 2)]
"
                            "{{expr}}" expr))]
    (doseq [expr ["(extend-type String IFruit (subtotal ([s] (count s)) ([s discount] (- (count s) discount))))"
                  "(extend String IFruit {:subtotal (fn ([s] (count s)) ([s discount] (- (count s) discount)))})"
                  "(extend-protocol IFruit String (subtotal ([s] (count s)) ([s discount] (- (count s) discount))))"]
            :let [prog (prog expr)
                  prog #?(:clj prog
                          :cljs (-> prog
                                    (str/replace "String" "js/String")))]]
      (is (= [100 95 3 1] (eval* prog))))))

#?(:clj
   (deftest import-test
     (testing "namespace with hyphen"
       (let [prog "
(ns foo-bar)
(defprotocol Foo)
(ns bar)
(import 'foo_bar.Foo)
(instance? Foo (reify Foo))
"] (is (true? (tu/eval* prog {})))))))

(deftest satisfies-test
  (testing "No methods"
    (let [prog "
(defprotocol Foo)
(defprotocol Bar)
[(satisfies? Foo (reify Foo))
 (satisfies? Bar (reify Foo))]
"] (is (= [true false] (tu/eval* prog {}))))))

(deftest order-test
  (testing "extend-via-metadata overrides extend-protocol, only if option given"
    (is (= :object (eval* "(defprotocol Foo (foo [this]))
                           (extend-protocol Foo Object (foo [this] :object))
                           (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (is (= :meta (eval* "(defprotocol Foo :extend-via-metadata true (foo [this]))
                         (extend-protocol Foo Object (foo [this] :object))
                         (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (is (= :object (eval* "(defprotocol Foo (foo [this]))
                           (extend Object Foo {:foo (fn foo [this] :object)})
                           (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (is (= :object (eval* "(defprotocol Foo (foo [this]))
                           (extend Object Foo {:foo (fn foo [this] :object)})
                           (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (is (= :meta (eval* "(defprotocol Foo :extend-via-metadata true (foo [this]))
                         (extend Object Foo {:foo (fn foo [this] :object)})
                         (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (is (= :object (eval* "(defprotocol Foo (foo [this]))
                           (extend-type Object Foo (foo [_] :object))
                           (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (is (= :meta (eval* "(defprotocol Foo :extend-via-metadata true (foo [this]))
                         (extend-type Object Foo (foo [_] :object))
                         (foo (vary-meta {} assoc `foo (fn [_] :meta)))"))))
  (testing "defrecord protocol is preferred over extend-via-metata"
    (is (= :record (eval* "(defprotocol IFoo (foo [this]))
                           (defrecord Foo [] IFoo (foo [_] :record))
                           (foo (vary-meta (->Foo) assoc `foo (fn [_] :meta)))")))
    (is (= :record (eval* "(defprotocol IFoo :extend-via-metadata true (foo [this]))
                           (defrecord Foo [] IFoo (foo [_] :record))
                           (foo (vary-meta (->Foo) assoc `foo (fn [_] :meta)))"))))
  (testing "defrecord protocol is preferred over extend object"
    (is (= :record (eval* "(defprotocol IFoo (foo [this]))
                           (extend-type Object IFoo (foo [this] :object))
                           (defrecord Foo [] IFoo (foo [_] :record))
                           (foo (->Foo))")))))

#?(:clj
   (deftest satisfies-host-protocol
     (let [ns (sci/create-ns 'clojure.core.protocols)]
       (is (true? (sci/eval-string "(satisfies? clojure.core.protocols/IKVReduce {})"
                                   {:namespaces {'clojure.core.protocols
                                                 {'IKVReduce (sci/new-var 'clojure.core.protocols/IKVReduce
                                                                          {:protocol p/IKVReduce
                                                                          :ns ns}
                                                                          {:ns ns})}}}))))))
