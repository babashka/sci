(ns sci.defrecords-and-deftype-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing]]
   [sci.core :as sci]
   #?(:clj [sci.impl.deftype :as deftype])
   [sci.test-utils :as tu]))

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
    (is (= ["test" "john"] (tu/eval* prog {}))))
  (let [prog "
(defprotocol Foo (foo [x])) (defrecord Dude [a] Foo (foo [{:keys [a]}] a)) (foo (->Dude 1))"]
    (is (= 1 (tu/eval* prog {})))))

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
             (into {} (foo (Foo. 1)))" {}))))
  (testing "defrecord inside non-top-level form"
    (is (= {:a 1} (tu/eval* "(let [x 1] (defrecord Foo [a]) (into {} (->Foo 1)))" {})))
    (is (= {:a 1} (tu/eval* "(let [x 1] (defrecord Foo [a]) (into {} (map->Foo {:a 1})))" {})))
    (is (str/includes? (tu/eval* "(let [x 1] (defrecord Foo [a]) (str (->Foo 1)))" {}) "Foo")))
  (testing "meta and ext"
    (is (true? (tu/eval* "(defrecord Dude [x]) (let [x (new Dude 1 {:meta 1} {:ext 2})]
(and (= 1 (:x x)) (= 1 (:meta (meta x))) (= 2 (:ext x))))" {})))))

(deftest repr-test
  (let [prog "
(ns foo) (defrecord Foo []) (ns bar (:import [foo Foo])) Foo"]
    (is (= (str 'foo.Foo) (str (tu/eval* prog {}))))))

(deftest type-test
  (let [prog "
(ns foo) (defrecord Foo []) (= foo.Foo (type (->Foo)))"]
    (is (true? (tu/eval* prog {}))))
  (let [prog "
(ns foo) (defrecord Foo []) (instance? sci.lang.Type (type (->Foo)))"]
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

#?(:clj
   (deftest print-method-test
     (let [prog "(ns foo) (defrecord A [x y z]) (defmethod print-method A [x writer] (.write writer \"<A>\")) (pr-str [(->A 1 2 3)])"]
       (is (= "[<A>]" (tu/eval* prog {}))))))

#?(:cljs
   (deftest IPrintWithWriter-test
     (is (= "dude" (sci/eval-string "(defrecord Foo [x]) (extend-protocol IPrintWithWriter Foo  (-pr-writer [o w opts] (-write w \"dude\"))) (pr-str (->Foo))")))
     (is (= "dude" (sci/eval-string "(deftype Foo [] IPrintWithWriter (-pr-writer [_ w opts] (-write w \"dude\"))) (pr-str (->Foo))")))))

#?(:cljs (def Exception js/Error))

(deftest deftype-test
  (is (= 1 (tu/eval* "(defprotocol GetX (getX [_])) (deftype Foo [x y] GetX (getX [_] x)) (getX (->Foo 1)) " {})))
  (let [prog "(deftype Foo [a b]) (let [x (->Foo :a :b)] [(.-a x) (.-b x)])"]
    (is (= [:a :b] (tu/eval* prog {}))))
  (is
   (= 10
      (tu/eval* (str/replace "(defprotocol IFoo (setField [_]) (getField [_])) (deftype Foo [^:volatile-mutable a] IFoo (setField [_] (set! a 10)) (getField [_] a)) (getField (doto (->Foo) (setField)))"
                             "^:volatile-mutable" #?(:clj "^:volatile-mutable"
                                                     :cljs "^:mutable")) {})))
  (is (= [1 2 2]
         (tu/eval* (str/replace "(defprotocol ICounter (inc! [_])) (deftype Cnt [^:volatile-mutable n] ICounter (inc! [_] (let [old-n n new-n-set (set! n (inc n)) new-n n] [old-n new-n-set new-n]))) (inc! (->Cnt 1))"
                                "^:volatile-mutable" #?(:clj "^:volatile-mutable"
                                                        :cljs "^:mutable"))
                   {})))
  (testing "deftype inside non-top-level form (issue #1936)"
    (is (nil? (tu/eval* "(let [x 1] (deftype MyType [field]) (empty (->MyType \"\")))" {})))
    (is (str/includes? (tu/eval* "(let [x 1] (deftype MyType [field]) (str (->MyType \"hello\")))" {})
                        "MyType")))
  (testing "getting value of shadowed field"
    (is (= 10
           (tu/eval* (str/replace "(defprotocol ICounter (inc! [_])) (deftype Cnt [^:volatile-mutable n] ICounter (inc! [_] (let [n 10] n))) (inc! (->Cnt 1))"
                                  "^:volatile-mutable" #?(:clj "^:volatile-mutable"
                                                          :cljs "^:mutable"))
                     {}))))
  (testing "can't assign shadowed field"
    (is (thrown? Exception
                 (tu/eval* (str/replace "(defprotocol ICounter (inc! [_])) (deftype Cnt [^:volatile-mutable n] ICounter (inc! [_] (let [n n] (set! n 10)))) (inc! (->Cnt 1))"
                                        "^:volatile-mutable" #?(:clj "^:volatile-mutable"
                                                                :cljs "^:mutable"))
                           {}))))
  (testing "TODO fix"
    #_(is (= 2 (sci/eval-string (str/replace "(defprotocol GetX (getX [_])) (deftype Foo [^:volatile-mutable x y] GetX (getX [_] (set! x inc) (x 1))) (getX (->Foo identity 0))"
                                             "^:volatile-mutable" #?(:clj "^:volatile-mutable"
                                                                     :cljs "^:mutable"))))))

  #?(:clj
     (is (re-find #"#object\[user.Foo" (sci/with-out-str (sci/eval-string "(deftype Foo []) (prn (->Foo))" {:namespaces {'clojure.core {'print-method print-method}} :classes {:allow :all}})))))
  #?(:clj
     (is (re-find #":hello" (sci/with-out-str (sci/eval-string "(deftype Foo []) (defmethod print-method Foo [this writer] (.write writer (str :hello))) (prn (->Foo))" {:namespaces {'clojure.core {'print-method print-method}} :classes {:allow :all}})))))
  (is (true? (tu/eval* "(deftype Foo []) (instance? Foo (->Foo))" {})))
  (is (true? (tu/eval* "(ns bar) (deftype Foo []) (ns foo (:import [bar Foo])) (instance? Foo (Foo.))" {})))
  (is (= "dude" (tu/eval* "(deftype Dude [] Object (toString [_] \"dude\")) (str (->Dude))" {})))
  #?(:clj (is (= [true false] (tu/eval* "(deftype Dude [x] Object (toString [_] (str x)) (equals [this other] (= (str this) (str other)))) [(= (->Dude 1) (->Dude 1)) (= (->Dude 1) (->Dude 2))]" {}))))
  #?(:clj (is (true? (tu/eval* "(deftype Dude [x] Object (hashCode [_] 1))
(deftype Dude2 [x]) (and (= 1 (hash (Dude. 1337))) (not= 1 (hash (Dude2.))))" {})))))

(deftest equiv-test
  (let [prog "(defrecord Foo [a]) (defrecord Bar [a]) [(= (->Foo 1) (->Foo 1)) (= (->Foo 1) (->Bar 1)) (= (->Foo 1) {:a 1})]"]
    (is (= [true false false] (tu/eval* prog {})))))

(deftest with-meta-preserves-type-test
  (let [prog "(defrecord Foo [a] Object (toString [_] \"!!FOO!!\")) (let [wm (with-meta (->Foo 1) {:a 1})] [(meta wm) (str wm)])"]
    (is (= [{:a 1} "!!FOO!!"] (tu/eval* prog {})))))

(deftest syntax-quote-test
  (is (= "foo.Foo" (str (tu/eval* "(ns foo) (defrecord Foo []) `Foo" {}))))
  (is (= "foo.Foo" (str (tu/eval* "(ns foo) (defrecord Foo []) (ns bar) (import foo.Foo) `Foo" {}))))
  (is (true? (tu/eval* "(ns foo) (defrecord Foo []) (ns bar) (import foo.Foo) (symbol? `Foo)" {}))))

(deftest satisfies-IRecord-test
  #?(:cljs (is (true? (sci/eval-string "(defrecord Foo []) (satisfies? IRecord (->Foo))")))))

(deftest map-constructor-nil-test
  (is (true? (sci/eval-string "(defrecord Foo [a]) (and (contains? (map->Foo {}) :a)
(nil? (:a (map->Foo {}))))"))))

(deftest dissoc-test
  (testing "dissoc returns a map if any record basis field is removed"
    (are [expected prog] (= expected (tu/eval* prog {}))
                         "{:a 1, :b 2}" "(defrecord Foo [a b c])
                                         (def r (->Foo 1 2 3))
                                         (print-str (dissoc r :c))"
                         "{:a 1, :b 2}" "(defrecord Foo [a b c])
                                         (def r (->Foo 1 2 3))
                                         (print-str (dissoc r :c :d))"
                         "{}" "(defrecord Foo [a b c])
                               (def r (->Foo 1 2 3))
                               (print-str (dissoc r :a :b :c))"))
  (testing "dissoc keeps record type if no record basis field is removed"
    (are [expected prog] (= expected (tu/eval* prog {}))
                         "#user.Foo{:a 1, :b 2, :c 3}" "(defrecord Foo [a b c])
                                                        (def r (->Foo 1 2 3))
                                                        (print-str (dissoc r :d))"
                         "#user.Foo{:a 1, :b 2, :c 3, :e 5}" "(defrecord Foo [a b c])
                                                              (def r (->Foo 1 2 3))
                                                              (-> r (assoc :d 4 :e 5)
                                                                    (dissoc :d)
                                                                    print-str)"))
  (testing "babashka #1899"
    (is (= {} (sci/eval-string "(defrecord MyRecord [a]) (dissoc (with-meta (->MyRecord 1) {:foo :bar}) :a)")))))

#?(:clj
   (deftest deftype-fn-test
     (when-not tu/native?
       (testing "deftype-fn hook is called when interfaces are present"
         (let [;; A constructor fn that creates a SciType from the map.
               ;; Registered in the SCI ctx under test.helpers/my-ctor.
               my-ctor
               (fn [{:keys [fields]}]
                 (deftype/->type-impl nil nil nil fields))
               ;; deftype-fn returns a map with :constructor, or nil.
               deftype-fn
               (fn [{:keys [interfaces]}]
                 (when (contains? interfaces clojure.lang.ILookup)
                   {:constructor-fn 'test.helpers/my-ctor}))
               opts {:classes {'clojure.lang.ILookup clojure.lang.ILookup}
                     :namespaces {'test.helpers {'my-ctor my-ctor}}
                     :deftype-fn deftype-fn}]
           (testing "field access works via ICustomType.getFields"
             (is (= [:a :b]
                    (tu/eval*
                     "(deftype MyType [x y]
                        clojure.lang.ILookup
                        (valAt [_ k] (get {:x x :y y} k)))
                      (let [t (->MyType :a :b)]
                        [(.-x t) (.-y t)])"
                     opts))))
           (testing "deftype-fn inside non-top-level form"
             (is (= [:a :b]
                    (tu/eval*
                     "(let [x 1]
                        (deftype MyType [x y]
                          clojure.lang.ILookup
                          (valAt [_ k] (get {:x x :y y} k)))
                        [(.-x (->MyType :a :b)) (.-y (->MyType :a :b))])"
                     opts))))
           (testing "deftype-fn receives correct interfaces set"
             (let [received (atom nil)
                   spy-fn (fn [{:keys [interfaces] :as args}]
                            (reset! received interfaces)
                            (deftype-fn args))]
               (tu/eval*
                "(deftype T [x] clojure.lang.ILookup (valAt [_ k] k))"
                (assoc opts :deftype-fn spy-fn))
               (is (= #{clojure.lang.ILookup} @received))))))
       (testing "deftype-fn returning nil falls through to standard path"
         (let [deftype-fn (constantly nil)
               opts {:deftype-fn deftype-fn}]
           (is (= 1
                  (tu/eval*
                   "(defprotocol GetX (getX [_]))
                    (deftype Foo [x] GetX (getX [_] x))
                    (getX (->Foo 1))"
                   opts)))))
       (testing "deftype-fn error replaces default error message"
         (let [deftype-fn (fn [_] {:error "custom hint: use IPersistentMap"})
               opts {:classes {'clojure.lang.ILookup clojure.lang.ILookup}
                     :deftype-fn deftype-fn}]
           (is (thrown-with-msg? Exception #"custom hint: use IPersistentMap"
                  (tu/eval*
                   "(deftype Foo [x] clojure.lang.ILookup (valAt [_ k] k))"
                   opts))))))))

(deftest deftype-macro-expanding-to-deftype-test
  (testing "macro in another ns that expands to deftype with namespace-qualified method names"
    (is (= 1
           (tu/eval*
            "(ns my.proto)
             (defprotocol GetX (getX [_]))

             (ns my.macros (:require [my.proto]))
             (defmacro defwrapper [name fields & impls]
               `(deftype ~name ~fields my.proto/GetX ~@impls))

             (ns my.user (:require [my.macros :refer [defwrapper]]
                                   [my.proto :refer [getX]]))
             (defwrapper Foo [x] (getX [_] x))
             (getX (->Foo 1))"
            {})))))

(deftest deftype-via-defcache-like-macro-test
  (testing "macro that expands to deftype with multiple protocols, like core.cache/defcache"
    (is (= [:hit :miss]
           (tu/eval*
            "(ns my.cache)
             (defprotocol CacheP
               (lookup [_ k])
               (has? [_ k]))

             (defprotocol Stringy
               (to-str [_]))

             (defmacro defcache [type-name fields & specifics]
               (let [base (first fields)]
                 `(deftype ~type-name ~fields
                    ~@specifics
                    Stringy
                    (to-str [_] (str ~base)))))

             (ns my.app (:require [my.cache :refer [defcache lookup has? to-str]]))
             (defcache MyCache [data]
               my.cache/CacheP
               (lookup [_ k] (get data k))
               (has? [_ k] (contains? data k)))

             (let [c (->MyCache {:a :hit})]
               [(lookup c :a) (if (has? c :b) :hit :miss)])"
            {})))))

(deftest deftype-macroexpand-1-produces-deftype*-test
  (do
    (testing "macroexpand-1 of deftype contains deftype* and constructor declare"
      (is (true?
           (tu/eval*
            "(defprotocol IFoo (foo [_]))
             (let [expanded (macroexpand-1 '(deftype Bar [x] IFoo (foo [_] x)))
                   forms (tree-seq seq? seq expanded)]
               (and (some #(and (seq? %) (= 'deftype* (first %))) forms)
                    (some #(and (seq? %) (= 'declare (first %)) (= '->Bar (second %))) forms)))"
            {})))
      (testing "deftype* form contains expected structure"
        (is (true?
             (tu/eval*
              "(defprotocol IFoo (foo [_]))
               (let [expanded (macroexpand-1 '(deftype Bar [x] IFoo (foo [_] x)))
                     dt (first (filter #(and (seq? %) (= 'deftype* (first %)))
                                       (tree-seq seq? seq expanded)))]
                 (and dt
                      (= '[x] (nth dt 3))
                      (= :implements (nth dt 4))
                      (vector? (nth dt 5))))"
              {}))))
      (testing "macroexpand does not expand deftype* further (it is a special form)"
        (is (true?
             (tu/eval*
              "(defprotocol IFoo (foo [_]))
               (let [expanded (macroexpand '(deftype Bar [x] IFoo (foo [_] x)))
                     forms (tree-seq seq? seq expanded)]
                 (some #(and (seq? %) (= 'deftype* (first %))) forms))"
              {})))))))

(deftest deftype-resolve-test
  (testing "resolve returns Type, not Var, in defining namespace"
    (is (true? (tu/eval* "(deftype Foo [x]) (instance? sci.lang.Type (resolve 'Foo))" {})))
    (is (false? (tu/eval* "(deftype Foo [x]) (var? (resolve 'Foo))" {}))))
  (testing "resolve returns Type after cross-namespace import"
    (is (true?
         (tu/eval*
          "(ns a) (deftype Foo [x])
           (ns b (:import [a Foo]))
           (instance? sci.lang.Type (resolve 'Foo))"
          {})))
    (is (false?
         (tu/eval*
          "(ns a) (deftype Foo [x])
           (ns b (:import [a Foo]))
           (var? (resolve 'Foo))"
          {}))))
  (testing "resolve returns Type for defrecord too"
    (is (true? (tu/eval* "(defrecord Bar [x]) (instance? sci.lang.Type (resolve 'Bar))" {})))
    (is (false? (tu/eval* "(defrecord Bar [x]) (var? (resolve 'Bar))" {}))))
  (testing "#'Foo throws for deftype (no var, matching Clojure)"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"Unable to resolve var"
                          (tu/eval* "(deftype Foo [x]) #'Foo" {}))))
  (testing "#'Bar throws for defrecord (no var, matching Clojure)"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"Unable to resolve var"
                          (tu/eval* "(defrecord Bar [x]) #'Bar" {}))))
  #?(:clj
     (testing "class? returns true for resolved types"
       (is (true? (tu/eval* "(deftype Foo [x]) (class? (resolve 'Foo))" {})))
       (is (true? (tu/eval* "(defrecord Bar [x]) (class? (resolve 'Bar))" {})))))
  #?(:clj
     (let [opts {:classes {:allow :all 'java.lang.Class {:class java.lang.Class}}}]
       (testing ".getName returns fully qualified name, like Class"
         (is (= "user.Foo" (sci/eval-string "(deftype Foo [x]) (.getName (resolve 'Foo))" opts)))
         (is (= "user.Bar" (sci/eval-string "(defrecord Bar [x]) (.getName (resolve 'Bar))" opts))))
       (testing ".getName works even with ^Class type hint"
         (is (= "user.Foo" (sci/eval-string "(deftype Foo [x]) (let [^java.lang.Class c (resolve 'Foo)] (.getName c))" opts)))))))

(deftest deftype-macroexpand-constructor-visible-test
  (testing "macroexpand of deftype contains a (declare ->Foo) form"
    (is (true?
         (tu/eval*
          "(let [expanded (macroexpand '(deftype Foo [x]))]
             (some #(and (seq? %) (= 'declare (first %)) (= '->Foo (second %)))
                   (tree-seq seq? seq expanded)))"
          {})))))

(deftest deftype*-uses-flat-methods-not-metadata-test
  (testing "modifying flat methods in deftype* form takes effect (code walkers)"
    (is (= 42
           (tu/eval*
            "(defprotocol IVal (get-val [_]))
             (let [expanded (macroexpand-1 '(deftype Foo [] IVal (get-val [_] 0)))
                   ;; Find the deftype* form inside the expansion
                   dt-form (first (filter #(and (seq? %) (= 'deftype* (first %)))
                                          (tree-seq seq? seq expanded)))
                   ;; Replace the method body: change 0 to 42
                   modified (concat (butlast dt-form) [(list 'get-val ['_] 42)])]
               (eval (list 'do modified '(get-val (->Foo)))))"
            {})))))

(deftest symbol-on-resolved-type-test
  (testing "symbol on str of resolved deftype works"
    (is (= 'user.Foo
           (tu/eval* "(deftype Foo [x]) (symbol (str (resolve 'Foo)))" {}))))
  (testing "symbol on str of resolved defrecord works"
    (is (= 'user.Bar
           (tu/eval* "(defrecord Bar [x]) (symbol (str (resolve 'Bar)))" {})))))

(deftest syntax-quote-type-produces-dotted-form-test
  (testing "syntax-quote on a defrecord/deftype name produces munged dotted form (matching Clojure)"
    (is (= 'user.Foo (tu/eval* "(defrecord Foo [x]) `Foo" {})))
    (is (= 'user.Foo (tu/eval* "(deftype Foo [x]) `Foo" {}))))
  (testing "cross-namespace syntax-quote after import"
    (is (= 'my_ns.Foo
           (tu/eval*
            "(ns my-ns) (defrecord Foo [x])
             (ns other (:import [my-ns Foo]))
             `Foo"
            {})))
    (is (= 'my_ns.Foo
           (tu/eval*
            "(ns my-ns) (deftype Foo [x])
             (ns other (:import [my-ns Foo]))
             `Foo"
            {})))))

(deftest use-does-not-bring-constructor-test
  (testing "deftype constructor requires :import, not just :use"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"Unable to resolve"
                          (tu/eval* "(ns foo) (deftype Bar [x])
                                     (ns baz (:use [foo]))
                                     (Bar. 1)" {}))))
  (testing "deftype constructor works with :import"
    (is (= 1 (tu/eval* "(ns foo) (deftype Bar [x])
                         (ns baz (:require [foo]) (:import [foo Bar]))
                         (.-x (Bar. 1))" {}))))
  (testing "import with munged (underscored) namespace name"
    (is (= 1 (tu/eval* "(ns my-ns) (deftype Bar [x])
                         (ns other (:require [my-ns]) (:import [my_ns Bar]))
                         (.-x (Bar. 1))" {}))))
  (testing "import defrecord with munged namespace name"
    (is (= 1 (tu/eval* "(ns my-ns) (defrecord Bar [x])
                         (ns other (:require [my-ns]) (:import [my_ns Bar]))
                         (:x (Bar. 1))" {})))))

(deftest re-eval-deftype-test
  (testing "re-evaluating deftype in the same namespace works"
    (is (= 2 (tu/eval* "(deftype Foo [x]) (deftype Foo [x y]) (.-y (->Foo 1 2))" {}))))
  (testing "re-evaluating defrecord in the same namespace works"
    (is (= 2 (tu/eval* "(defrecord Foo [x]) (defrecord Foo [x y]) (:y (->Foo 1 2))" {})))))

(deftest constructor-self-reference-test
  (testing "deftype method body can call its own constructor"
    (is (= 42 (tu/eval* "
(defprotocol IWrap
  (unwrap [this])
  (rewrap [this v]))
(deftype Wrap [x]
  IWrap
  (unwrap [_] x)
  (rewrap [_ v] (Wrap. v)))
(unwrap (rewrap (->Wrap 1) 42))" {}))))
  (testing "defrecord method body can call its own constructor"
    (is (= 42 (tu/eval* "
(defprotocol IBox
  (unbox [this])
  (rebox [this v]))
(defrecord Box [x]
  IBox
  (unbox [_] x)
  (rebox [_ v] (->Box v)))
(unbox (rebox (->Box 1) 42))" {})))))
