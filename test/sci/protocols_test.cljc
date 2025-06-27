(ns sci.protocols-test
  (:require
   [clojure.core.protocols :as p]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.test-utils :as tu])
  #?(:clj (:import [java.lang Long])))

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
               :cljs (-> prog
                         (str/replace "Object" ":default")
                         (str/replace "js/:default" "js/Object"))) ;lol
            #?(:clj {}
               :cljs {:classes {:allow :all
                                'js #js {:Object js/Object
                                         :String js/String
                                         :Number js/Number
                                         :Array js/Array
                                         :Function js/Function
                                         :Boolean js/Boolean}}})))

(deftest docstring-test
  (is (= "-------------------------\nuser/Foo\n  cool protocol\n" (tu/eval* "
(defprotocol Foo \"cool protocol\" (foo [_]))
(with-out-str (clojure.repl/doc Foo))
" {})))
  (is (= :object (eval* "
(defprotocol Foo (foo [_] \"docstring\"))
(extend-type Object Foo (foo [_] :object))
(foo 1)
"))))

(deftest fn-docstring-test
  (testing "protocol functions get docstrings and arglists"
                            ; based on running defprotocol and doc on fns in clojure
    (let [expected-output ["-------------------------" 
                           "user/doced" 
                           "([this])" 
                           "  awesome docs" 
                           "-------------------------" 
                           "user/arities" 
                           "([this a] [this a b])" 
                           "  arity docs" 
                           "-------------------------" 
                           "user/nodocs" 
                           "([this n] [_ o p])"
                             ; not technically valid in Clojure, but won't break anything in sci
                           "-------------------------"
                           "user/just-name"]
          prog            '(do
                             (defprotocol Foo
                               (doced [this] "awesome docs")
                               (arities [this a] [this a b] "arity docs")
                               (nodocs [this n] [_ o p])
                               (just-name))
                             (with-out-str (clojure.repl/doc doced)
                                           (clojure.repl/doc arities)
                                           (clojure.repl/doc nodocs)
                                           (clojure.repl/doc just-name)))]
      (is (= expected-output (str/split-lines (tu/eval* (pr-str prog) {})))))))

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
    (doseq [expr ["(extend-type String IFruit (subtotal ([s] (count s))
                                                        ([s discount] (- (count s) discount))))"
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

(deftest satisfies-default
  (is (= '([true :object]
           [true :object]
           [true :object]
           [true :object]
           [true :object]
           [true :object])
         (eval* (str "(defprotocol IFoo (foo [_]))
(extend-type " #?(:clj "Object" :cljs "default") " IFoo (foo [_] :object))
(map
 #(vector (satisfies? IFoo %) (foo %))
 [\"\" 1 inc true [] {}])")))))


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
                         (foo (vary-meta {} assoc `foo (fn [_] :meta)))")))
    (testing "multi-arity protocol"
      (is (= :meta (eval* "(defprotocol Foo :extend-via-metadata true (foo [this] [this x]))
                           (extend-protocol Foo Object (foo ([_] :object) ([_ x] :object2)))
                           (foo (vary-meta {} assoc `foo (fn ([_] :meta) ([_ _] :meta))))")))
      (is (= :meta (eval* "(defprotocol Foo :extend-via-metadata true (foo [this] [this x]))
                           (extend-type Object Foo (foo ([_] :object) ([_ x] :object2)))
                           (foo (vary-meta {} assoc `foo (fn ([_] :meta) ([_ _] :meta))))")))))
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

(deftest fallback-on-reified-object-test
  (is (= :object
         (eval* "(defprotocol Foo (foo [_])) (defprotocol Bar) (extend-protocol Foo Object (foo [_] :object)) (foo (reify Bar))")))
  (is (= [:object :meta]
         (eval* "(defprotocol Foo :extend-via-metadata true (foo [_])) (defprotocol Bar) (extend-protocol Foo Object (foo [_] :object)) [(foo 1) (foo (with-meta {} {'user/foo (fn [_] :meta)}))]"))))

(deftest fallback-on-record
  (is (= :object
         (eval* "(defprotocol Dude (foo [_])) (extend-type Object Dude (foo [_] :object)) (defrecord Rec []) (foo (->Rec))")))
  (is (= [:object :meta]
         (eval* "(defprotocol Dude :extend-via-metadata true (foo [_])) (extend-type Object Dude (foo [_] :object)) (defrecord Rec []) [(foo (->Rec)) (foo (with-meta {} {'user/foo (fn [_] :meta)}))]"))))

#?(:clj
   (deftest IRecord-extension-test
     (testing "without default override"
       (is (= :record (sci/eval-string "(defprotocol Dude (dude [_])) (extend-protocol Dude clojure.lang.IRecord (dude [_] :record)) (defrecord Foo []) (dude (->Foo))"
                                       {:classes {'clojure.lang.IRecord clojure.lang.IRecord}}))))
     (testing "without default override + meta"
       (is (= :record (sci/eval-string "(defprotocol Dude :extend-via-metadata true (dude [_])) (extend-protocol Dude clojure.lang.IRecord (dude [_] :record)) (defrecord Foo []) (dude (->Foo))"
                                       {:classes {'clojure.lang.IRecord clojure.lang.IRecord}}))))
     (testing "with default override"
       (is (= :record (sci/eval-string "(defprotocol Dude (dude [_])) (extend-protocol Dude clojure.lang.IRecord (dude [_] :record) Object (dude [_] :object)) (defrecord Foo []) (dude (->Foo))"
                                       {:classes {'clojure.lang.IRecord clojure.lang.IRecord}}))))
     (testing "with default override + multi-arity"
       (is (= :record (sci/eval-string "(defprotocol Dude (dude [_] [_ x])) (extend-protocol Dude clojure.lang.IRecord (dude [_] :record) Object (dude ([_] :object) ([_ x] :object2))) (defrecord Foo []) (dude (->Foo))"
                                       {:classes {'clojure.lang.IRecord clojure.lang.IRecord}}))))
     (testing "with default override + meta"
       (is (= :record (sci/eval-string "(defprotocol Dude :extend-via-metadata true (dude [_])) (extend-protocol Dude clojure.lang.IRecord (dude [_] :record) Object (dude [_] :object)) (defrecord Foo []) (dude (->Foo))"
                                       {:classes {'clojure.lang.IRecord clojure.lang.IRecord}}))))))

(deftest sigs-test
  (is (= '{:foo {:name foo, :arglists ([_] [_ x]), :doc nil}}
         (eval* "(defprotocol Dude (foo [_] [_ x])) (:sigs Dude)")))
  (is (= '{:foo {:name foo, :arglists ([_] [_ x]), :doc "docstring"}}
         (eval* "(defprotocol Dude (foo [_] [_ x] \"docstring\")) (:sigs Dude)"))))

(deftest marker-protocol-test
  (is (true? (eval* "(defprotocol Marker)

(defrecord Foo []
  Marker)

(satisfies? Marker (->Foo))
")))
  (is (true? (eval* "
(defprotocol Marker)

(defrecord Foo [])

(extend-type Foo Marker)
(satisfies? Marker (->Foo))
")))
  (is (true? (eval* "
(defprotocol Marker)

(defrecord Foo [])

(extend-protocol Marker Foo)
(satisfies? Marker (->Foo))
")))
  (is (true?
       (sci/binding [sci/out *out*]
         (-> "(defprotocol Marker)
(extend-type java.lang.Long Marker)
(satisfies? Marker 1)"
             #?(:cljs (str/replace "java.lang.Long" "number"))
             (sci/eval-string
              #?(:clj {:classes {'java.lang.Long java.lang.Long}}
                 :cljs {:classes {'js #js {:Number js/Number}}}))))))
  (is (true?
       (sci/binding [sci/out *out*]
         (-> "(defprotocol Marker)
(extend-protocol Marker java.lang.Long)
(satisfies? Marker 1)"
             #?(:cljs (str/replace "java.lang.Long" "number"))
             (sci/eval-string
              #?(:clj {:classes {'java.lang.Long java.lang.Long}}
                 :cljs {:classes {'js #js {:Number js/Number}}})))))))


(deftest return-value-test
  (is (true? (eval* "(= 'P (defprotocol P))"))))

(deftest instance-test
  (is (true? (eval* "(defprotocol Registry) (defn reg? [x] (instance? Registry x)) (reg? (reify Registry))"))))


#?(:cljs
   (deftest cljs-type-symbols
     (testing "extend-type"
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type default IFoo (foo [_] \"bar\"))
(def o (js/Object.create nil))
(satisfies? IFoo o)")) "no prototype")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type object IFoo (foo [_] \"bar\"))
(satisfies? IFoo #js {})")) "object")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type string IFoo (foo [_] \"bar\"))
(satisfies? IFoo \"baz\")")) "string")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type number IFoo (foo [_] \"bar\"))
(satisfies? IFoo 1)")) "number")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type array IFoo (foo [_] \"bar\"))
(satisfies? IFoo #js [1 2 3])")) "array")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type function IFoo (foo [_] \"bar\"))
(satisfies? IFoo inc)")) "function")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-type boolean IFoo (foo [_] \"bar\"))
(and (satisfies? IFoo true) (satisfies? IFoo false))")) "boolean"))

     (testing "extend-protocol"
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo default (foo [_] \"bar\"))
(def o (js/Object.create nil))
(satisfies? IFoo o)")) "no prototype")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo object (foo [_] \"bar\"))
(satisfies? IFoo #js {})")) "object")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo string (foo [_] \"bar\"))
(satisfies? IFoo \"baz\")")) "string")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo number (foo [_] \"bar\"))
(satisfies? IFoo 1)")) "number")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo array (foo [_] \"bar\"))
(satisfies? IFoo #js [1 2 3])")) "array")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo function (foo [_] \"bar\"))
(satisfies? IFoo inc)")) "function")
       (is (true? (eval* "(defprotocol IFoo (foo [_]))
(extend-protocol IFoo boolean (foo [_] \"bar\"))
(and (satisfies? IFoo true) (satisfies? IFoo false))")) "boolean"))))

(deftest issue-975-protocol-meta
  (is (true? (eval* "(defprotocol IFoo (foo [_])) (identical? #'IFoo (:protocol (meta #'foo)))"))))

(deftest protocol-satisfies-nil-and-boolean-test
  (is (true? (eval* "(defprotocol IFoo) (extend-type nil IFoo) (satisfies? IFoo nil)")))
  (is (true? (sci/eval-string "(defprotocol IFoo) (extend-type #?(:clj (class true) :cljs boolean) IFoo) (satisfies? IFoo false)"
                              {:classes #?(:clj nil :cljs {'js #js {:Boolean js/Boolean}})
                               :features #?(:clj #{:clj} :cljs #{:cljs})}))))
