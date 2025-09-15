(ns sci.interop-test
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.string :as str]
   [clojure.test :as test :refer [are deftest is testing #?(:cljs async)]]
   [sci.core :as sci]
   [sci.test-utils :as tu]
   #?(:cljs [goog.object :as gobj]))
  #?(:clj (:import PublicFields)))

(defn eval* [expr]
  (tu/eval* expr {}))

#?(:clj
   (deftest instance-methods
     (is (= 3 (eval* "(.length \"foo\")")))
     (testing "calling instance methods on unconfigured classes is not allowed"
       (is (thrown-with-msg? Exception #"getName.*Class.*allowed"
                             (eval* "(-> \"foo\" .getClass .getName)"))))
     (is (= "hii" (eval* "(def x \"foo\") (-> x (.replace \\o \\i) (.replace \"f\" \"h\"))")))
     (is (false? (eval* "(. \"foo\" isEmpty)")))
     (is (= "fbb" (eval* "(. \"foo\" replace \"o\" \"b\")")))
     (testing "error on type hint with unknown class"
       (is (thrown-with-msg? Exception #"Foo"
                             (eval* "(let [^Foo x ^Foo {}] (.foo x))"))))
     (testing "can get the name of arbitrary class by type hinting it as Object"
       (when-not tu/native?
         (is (= "clojure.core$int_QMARK_" (tu/eval* "(defn foo [^Object x] (.getClass x)) (.getName (foo int?))"
                                                    {:classes {'java.lang.Class Class}})))))
     (testing "resolve target class at analysis time"
       (is (= "message" (eval* "
(ns foo (:import [clojure.lang ExceptionInfo]))
(defn foo [e] (.getMessage ^ExceptionInfo e))
(ns bar (:require [foo]))
(foo/foo (ex-info \"message\" {}))"))))
     (testing "map interop"
       (when-not tu/native?
         (is (= #{:a} (tu/eval* "(.keySet {:a 1})"
                                {:classes {'java.util.Map 'java.util.Map
                                           :public-class (fn [o]
                                                           (when (instance? java.util.Map o) java.util.Map))}})))))))

#?(:clj
   (deftest instance-fields
     (is (= 3 (sci/eval-string "(.-x (PublicFields.))" {:classes {'PublicFields PublicFields}})))))

#?(:clj
   (deftest clojure-parity-tests
     (let [classes {:classes {'PublicFields PublicFields}}]
       (doseq [expr ['(.-x (PublicFields.))
                     '(.x (PublicFields.))
                     '(. (PublicFields.) instanceFoo)
                     '(. (PublicFields.) -instanceFoo)
                     '(.instanceFoo (PublicFields.))
                     '(.-instanceFoo (PublicFields.))]]
         (testing (pr-str expr)
           (is (= (eval expr) (sci/eval-string (pr-str expr) classes)))))
       (testing "non-zero arg count doesn't find field"
         (is (thrown-with-msg? Exception #"No matching method" 
                               (sci/eval-string "(.x (PublicFields.) 1)" classes)))))
     (when-not tu/native?
       (let [classes {:classes {'clojure.lang.Ratio 'clojure.lang.Ratio}}]
         (doseq [expr ['(. 4/5 numerator)
                       '(. 4/5 -numerator)
                       '(.-numerator 4/5)
                       '(.numerator 4/5)]]
           (testing (pr-str expr)
             (is (= (eval expr) (tu/eval* (pr-str expr) classes)))))))))

#?(:clj
   (deftest static-fields
     (is (= 32 (eval* "Integer/SIZE")))
     (is (= 32 (eval* "(Integer/SIZE)")))
     (is (= 32 (eval* "(. Integer -SIZE)")))
     (is (= 32 (eval* "(. Integer SIZE)")))
     (testing "calling static field on unconfigured classes is not allowed"
       (is (thrown-with-msg? Exception #"Unable to resolve"
                             (eval* "clojure.lang.Var/rev"))))))

#?(:clj
   (deftest constructor-test
     (is (= "dude" (eval* "(String. (str \"dude\"))")))
     (is (= "dude" (eval* "(new String (str \"dude\"))")))))

#?(:clj
   (deftest import-test
     (is (true? (eval* "(class? (import clojure.lang.ExceptionInfo))")))
     (is (some? (eval* "(import clojure.lang.ExceptionInfo) ExceptionInfo")))
     (is (thrown-with-msg? Exception #"resolve"
                           (eval* "(import foo.bar.Baz)")))
     (when-not tu/native?
       (is (thrown-with-data? {:line 1}
                              (eval* "(import foo.bar.Baz)"))))))

#?(:cljs
   (deftest instance-methods
     (is (= 102 (tu/eval* "(.charCodeAt \"foo\" 0)" {:classes {'String js/String}})))))

#?(:cljs
   (deftest instance-fields
     (is (= 1 (tu/eval* "(.-x (js-obj \"x\" 1))" {:classes {:allow :all}})))
     (is (= {"a" 2} (tu/eval* "(def obj #js {:a 1}) (set! (.-a obj) 2) (js->clj obj)"
                              {:classes {:allow :all}})))
     (is (= {:foo {:bar :baz}}
            (tu/eval* "(def x #js {:foo #js {}}) (set! (.. x -foo -bar) :baz) (js->clj x :keywordize-keys true)" {:classes {:allow :all}})))))

#?(:clj
   (deftest static-methods
     (is (= 123 (eval* "(Integer/parseInt \"123\")")))
     (is (= 123 (eval* "(. Integer (parseInt \"123\"))")))
     (is (= 123 (eval* "(. Integer parseInt \"123\")")))
     (is (= 123 (eval* "(Integer/parseInt (str \"12\" \"3\") (inc 9))")))
     (is (= 123 (eval* "(defmacro parse-int [x] `(. Integer (parseInt ~x)))
                        (parse-int \"123\")")))
     (testing "calling static methods on unconfigured classes is not allowed"
       (is (thrown-with-msg? Exception #"Unable to resolve"
                             (eval* "(clojure.lang.Var/find 'clojure.core/int)"))))
     (is (= :dude (sci/eval-string
                   "(Class/forName \"java.lang.String\")"
                   {:imports {'Class 'java.lang.Class}
                    :classes {'java.lang.Class {:class Class
                                                :static-methods {'forName (fn [_Class _forName] :dude)}}}})))
     (is (= :dude (sci/eval-string
                   "(sci.lang.Var/cloneThreadBindings)"
                   {:classes {'sci.lang.Var {:class sci.lang.Var
                                             :static-methods {'cloneThreadBindings (fn [_Class] :dude)}}}})))))

#?(:clj
   (deftest clojure-1_12-interop-test
     (is (= [1 2 3] (eval* "(map Integer/parseInt [\"1\" \"2\" \"3\"])")))
     (is (= [1 2 3] (eval* "(map String/.length [\"1\" \"22\" \"333\"])")))
     (is (= ["1" "22" "333"] (eval* "(map String/new [\"1\" \"22\" \"333\"])")))
     (is (= 3 (eval* "(String/.length \"123\")")))
     (is (= "123" (eval* "(String/new \"123\")")))))

#?(:clj
   (when-not tu/native?
     (deftest clojure-1_12-array-test
       (let [byte-1 (class (make-array Byte/TYPE 0))
             byte-3 (class (make-array Byte/TYPE 0 0 0))
             String-1 (class (make-array String 0))]
         (is (= (class (make-array Long/TYPE 0)) (eval* "long/1")))
         (is (= (class (make-array Long/TYPE 0 0)) (eval* "long/2") ))
         (is (= (class (make-array Integer/TYPE 0)) (eval* "int/1")))
         (is (= (class (make-array Double/TYPE 0)) (eval* "double/1") ))
         (is (= (class (make-array Short/TYPE 0)) (eval* "short/1") ))
         (is (= (class (make-array Boolean/TYPE 0)) (eval* "boolean/1")))
         (is (= byte-1 (eval* "byte/1")))
         (is (= (class (make-array Float/TYPE 0)) (eval* "float/1")))
         (is (= (class (make-array String 0)) (eval* "String/1")))
         (is (= String-1 (eval* "java.lang.String/1")))
         (is (= (symbol (pr-str byte-1)) (eval* "`byte/1")))
         (is (= (symbol (pr-str byte-3)) (eval* "`byte/3")))
         (is (= (symbol "java.util.UUID/1") (eval* "`java.util.UUID/1")))
         (is (= (symbol (pr-str String-1)) (eval* "`String/1")))
         (is (= (symbol (pr-str String-1)) (eval* "`java.lang.String/1")))
         (is (= [(symbol "long/2")] (eval* "['long/2]") (eval* "`[~'long/2]")))))))

(when-not tu/native?
  (deftest exception-data
    (testing "top-level interop forms have line and column data"
      (let [form-ex-data
            (fn [form]
              (try
                (tu/eval* (str form) {:classes {:allow :all
                                                #?@(:clj ['Long Long])}})
                (is (= nil "shouldn't reach here") (str form))
                (catch #?(:clj Exception :cljs :default) e
                  (ex-data e))))]
        (testing "instance members"
          (are [form]
            (let [actual (form-ex-data form)]
              (and (tu/submap? {:type   :sci/error
                                :line   1
                                :column 1}
                actual)
                (str/includes? (:message actual) "missingMem")))
            '(. 3 missingMem) '(. 3 missingMem 1 2)
            '(.missingMem 3)  '(.missingMem 3 1 2)
            ; these return nil in cljs
            #?@(:clj ['(.-missingMem 3) '(. 3 -missingMem)])))
        #?(:clj
           (testing "static members"
             (are [form]
               (let [actual (form-ex-data form)]
                 (and (tu/submap? {:type   :sci/error
                                   :line   1
                                   :column 1}
                        actual)
                   (str/includes? (:message actual) "missingMem")))
               '(. Long missingMem)  '(. Long missingMem 1 2)
               '(.missingMem Long)   '(.missingMem Long 1 2)
               '(Long/missingMem)    '(Long/missingMem 1 2)
               '(. Long -missingMem) '(.-missingMem Long)
               #_#_'Long/missingMem      '(Long/-missingMem))))))))

(deftest syntax-test
  (when-not tu/native?
    (doseq [expr ["(.)" "(. {})" "(.foo)"]]
      (is (thrown-with-msg? #?(:clj IllegalArgumentException :cljs js/Error)
                            #"Malformed"
                            (try (eval* expr)
                                 (catch #?(:clj Exception :cljs :default) e
                                   (throw #?(:clj (.getCause e))
                                          #?(:cljs (.-cause e))))))))))

;;;; CLJS

#?(:cljs
   (deftest methods-test
     (is (= \C (tu/eval* "(String/fromCharCode 67)" {:classes {'String js/String}})))
     (is (= 42 (tu/eval* "(js/parseInt \"42\")"     {:classes {'js js/global}})))
     (is (= "42"
            (tu/eval* "(.log js/console \"42\")"
                      {:classes {:allow :all
                                 'js #js {:console #js {:log identity}}}})))
     (is (= "42"
            (tu/eval* "(js/console.log \"42\")"
                      {:classes {:allow :all
                                 'js #js {:console #js {:log identity}}}})))
     (is (str/starts-with? (tu/eval* "(.toString js/Math.PI)"
                                     {:classes {:allow :all
                                                'js goog.global}})
                           "3.14"))
     (let [func-without-apply (let [f (fn [_ _ _] 0)]
                                (js/Object.setPrototypeOf f nil)
                                f)]
       (is (= 0 (tu/eval* "(.f foo)"
                          {:classes {:allow :all
                                     'foo #js {:f func-without-apply}}})))
       (is (= 0 (tu/eval* "(foo/f)"
                          {:classes {:allow :all
                                     'foo #js {:f func-without-apply}}}))))))

#?(:cljs
   (deftest field-access-test
     (is (= "NL" (tu/eval* "goog/LOCALE" {:classes {:allow :all
                                                    'goog #js {:LOCALE "NL"}}})))
     (is (= "NL" (tu/eval* "(. js/goog -LOCALE)" {:classes {:allow :all
                                                            'js #js {:goog #js {:LOCALE "NL"}}}})))
     (is (true? (tu/eval* "(some? js/goog.LOCALE)" {:classes {:allow :all
                                                              'js #js {:goog #js {:LOCALE "NL"}}}})))
     (is (true? (tu/eval* "(some? js/Number.POSITIVE_INFINITY)" {:classes {:allow :all
                                                                           'js goog.global}})))))

#?(:cljs
   (deftest constructor-test
     (is (= 42 (tu/eval* "(js/parseInt (.-message (js/Error. \"42\")))"
                         {:classes {:allow :all
                                    'js js/global}})))
     (is (= 42 (tu/eval* "(let [err js/Error] (js/parseInt (.-message (err. \"42\"))))"
                         {:classes {:allow :all
                                    'js js/global}})))
     (is (= 42 (tu/eval* "(def err js/Error) (js/parseInt (.-message (err. \"42\")))"
                         {:classes {:allow :all
                                    'js js/global}})))
     (is (=  [{"msg" "hello"}]
             (tu/eval* "(def pkg #js {:SomeConstructor js/Array}) (js->clj (new (.. pkg -SomeConstructor) #js {:msg \"hello\"}))"
                       {:classes {'js goog.global :allow :all}})))
     (is (= "foo" (sci/eval-string "(str (foo/String. \"foo\"))" {:namespaces {'foo {'String js/String}}})))))

#?(:cljs
   (when-not (tu/planck-env?)
     #?(:cljs
        (def fs (let [m (js->clj (js/require "fs"))]
                  (zipmap (map symbol (keys m)) (vals m)))))
     #?(:cljs
        (deftest object-as-namespace-test
          (is (str/includes?
               (tu/eval* "(str (fs/readFileSync \"README.md\"))" {:namespaces {'fs fs}})
               "EPL"))))))

#?(:cljs
   (deftest js-reader-test
     (is (= (js->clj #js [1 2 3]) (js->clj (tu/eval* "#js [1 2 3]" {}))))))

#?(:cljs
   (deftest preserve-clojure-vals-in-args-test
     (testing "interop doesn't convert values to Clojure values automatically"
       (async done
              (.then (tu/eval* "(.then (.resolve js/Promise {:a 1}) (clojure.core/fn [{:keys [:a] :as m}] a))"
                               {:classes
                                {'js goog/global :allow :all}})
                     (fn [a]
                       (is (= 1 a))
                       (done)))))))

#?(:cljs
   (deftest dot-in-js-invocation-test
     (when-not (tu/planck-env?)
       (is (str/includes?
            (tu/eval* "(first (js/process.argv.slice 0))"
                      {:classes
                       {'js goog/global :allow :all}})
            "node")))
     (is (tu/eval* "(js/Promise.all [])"
                   {:classes
                    {'js goog/global :allow :all}}))))


#?(:cljs (def persistent-queue (let [x PersistentQueue]
                                 (gobj/set x "EMPTY" PersistentQueue.EMPTY)
                                 x)))

#?(:cljs
   (deftest dotted-reference-test
     (is (= 1 (sci/eval-string "(def x #js {:a 1 :b #js {:c 2}}) x.a" {:classes {'js goog/global}})))
     (is (= 2 (sci/eval-string "(def x #js {:a 1 :b #js {:c 2}}) x.b.c" {:classes {'js goog/global}})))
     (is (= 1 (sci/eval-string "(ns foo.bar) (def x #js {:a #js {:b 1}}) (ns bar) foo.bar.x.a.b")))
     (is (= 1 (sci/eval-string "(ns foo.bar) (def x #js {:a #js {:b 1}}) (ns bar) foo.bar/x.a.b")))
     (testing "var ref"
       (is (= PersistentQueue.EMPTY (sci/eval-string "(def x PersistentQueue.EMPTY) x" {:namespaces {'clojure.core {'PersistentQueue (sci/new-var 'x persistent-queue)}}})))
       (is (= 2 (sci/eval-string "(def x #js {:inc inc}) (x.inc 1)"))))
     (testing "non-var ref"
       (is (= PersistentQueue.EMPTY
              (sci/eval-string "(def x PersistentQueue.EMPTY) x"
                               {:namespaces {'clojure.core {'PersistentQueue persistent-queue}}}))))
     (testing "with cljs.core prefix"
       (is (= PersistentQueue.EMPTY
              (sci/eval-string "(def x cljs.core/PersistentQueue.EMPTY) x"
                               {:namespaces {'clojure.core {'PersistentQueue persistent-queue}}})
              (sci/eval-string "(def x cljs.core.PersistentQueue.EMPTY) x"
                               {:namespaces {'clojure.core {'PersistentQueue persistent-queue}}}))))
     (testing "local ref"
       (is (= 1 (sci/eval-string "(let [x #js {:a 1}] x.a)")))
       (is (= 3 (sci/eval-string "(let [a #js {:b #js {:c 3}}] a.b.c)"))))))

#?(:cljs
   (deftest delayed-lookup-test
     (is (true? (tu/eval* "(def f (fn [] (when js/document (js/document.getElementById \"foo\")) true)) (f)"
                          {:classes
                           {'js goog/global :allow :all}})))

     (is (true? (tu/eval* "(when js/document js/document.getElementById) true"
                          {:classes
                           {'js goog/global :allow :all}})))))

#?(:cljs (deftest local-interop-test
           (is (= 1 (tu/eval* "(let [j #js {:a (fn [] 1)}] (j.a))" nil)))))

#?(:clj (def type-hint-config {:classes {'java.util.concurrent.Executors java.util.concurrent.Executors
                                         'java.util.concurrent.ThreadPoolExecutor java.util.concurrent.ThreadPoolExecutor
                                         'java.util.concurrent.Callable java.util.concurrent.Callable
                                         'java.util.concurrent.FutureTask java.util.concurrent.FutureTask
                                         'java.lang.Runnable java.lang.Runnable
                                         'java.util.concurrent.ExecutorService java.util.concurrent.ExecutorService}
                               :imports {'Runnable 'java.lang.Runnable
                                         'Callable 'java.util.concurrent.Callable}}))

#?(:clj
   (deftest type-hint-test
     (testing "string type hints"
       (is (string? (sci/eval-string "(defn read-string [^\"[B\" v] (String. v)) (read-string \"1\")"))))
     (testing "runnable"
       (testing "type hinting local with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(def fut (let [^java.lang.Runnable f (fn [] 3)] (.submit (java.util.concurrent.Executors/newCachedThreadPool) f))) (.get fut)" type-hint-config))))
       (testing "type hinting local on value with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(def fut (let [f ^java.lang.Runnable (fn [] 3)] (.submit (java.util.concurrent.Executors/newCachedThreadPool) f))) (.get fut)" type-hint-config))))
       (testing "type hinting local on value with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(def fut (let [f ^java.lang.Runnable (identity (fn [] 3))] (.submit (java.util.concurrent.Executors/newCachedThreadPool) f))) (.get fut)" type-hint-config))))
       (testing "type hinting on expression with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(def fut (let [f (fn [] 3)] (.submit (java.util.concurrent.Executors/newCachedThreadPool) ^java.lang.Runnable f))) (.get fut)" type-hint-config))))
       (testing "type hinting on fn expression as argument with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(def fut (.submit (java.util.concurrent.Executors/newCachedThreadPool) ^java.lang.Runnable (fn [] 3))) (.get fut)" type-hint-config))))
       (testing "type hinting on fn call expression as argument with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(def fut (.submit (java.util.concurrent.Executors/newCachedThreadPool) ^java.lang.Runnable (identity (fn [] 3)))) (.get fut)" type-hint-config))))
       (testing "type hinting fn argument with runnable returns nil on futuretask get"
         (is (nil? (sci/eval-string "(defn fut [^Runnable f] (.submit (java.util.concurrent.Executors/newCachedThreadPool) ^java.lang.Runnable f)) (.get (fut (fn [] 3)))" type-hint-config)))))
     (testing "callable"
       (testing "type hinting with callable returns nil on futuretask get"
         (is (= 3 (sci/eval-string "(def fut (let [^java.util.concurrent.Callable f (fn [] 3)] (.submit (java.util.concurrent.Executors/newCachedThreadPool) f))) (.get fut)" type-hint-config))))
       (testing "type hinting fn argument with callable returns nil on futuretask get"
         (is (= 3 (sci/eval-string "(defn fut [^java.util.concurrent.Callable f] (.submit (java.util.concurrent.Executors/newCachedThreadPool) f)) (.get (fut (fn [] 3)))" type-hint-config))))
       (testing "type hinting on fn expression as argument with Callable returns nil on futuretask get"
         (is (= 3 (sci/eval-string "(def fut (.submit (java.util.concurrent.Executors/newCachedThreadPool) ^java.util.concurrent.Callable (fn [] 3))) (.get fut)" type-hint-config))))
       (testing "similar cases but with qualified interop"
         (is (= [nil nil nil 3 3 3] (sci/eval-string "(def ^java.util.concurrent.ExecutorService thread-pool (java.util.concurrent.Executors/newCachedThreadPool))
[
@(java.util.concurrent.ExecutorService/.submit thread-pool ^Runnable (fn [] 3))
@(^[Runnable] java.util.concurrent.ExecutorService/.submit thread-pool (fn [] 3))
@((identity ^[Runnable] java.util.concurrent.ExecutorService/.submit) thread-pool (fn [] 3))
@(java.util.concurrent.ExecutorService/.submit thread-pool ^Callable (fn [] 3))
@(^[Callable] java.util.concurrent.ExecutorService/.submit thread-pool (fn [] 3))
@((identity ^[Callable] java.util.concurrent.ExecutorService/.submit) thread-pool (fn [] 3))
]"
                                    type-hint-config)))))
     (testing "type hint on interop argument"
       ;; this test assumes clojure/core.clj comes from a jar file
       ;; the test will fail when not processing the type hint on the interop argument
       (sci/eval-string "(.getJarEntry ^java.net.JarURLConnection (.openConnection resource))"
                        {:bindings {'resource (io/resource "clojure/core.clj")}
                         :classes {'java.net.URL java.net.URL
                                   'java.net.JarURLConnection java.net.JarURLConnection}}))
))

#?(:cljs
   (deftest issue-987-munged-method-or-property-name-test
     (is (= 1 (sci/eval-string "(def x #js {:foo_bar 1}) (.-foo-bar x)" {:classes {:allow :all}})))
     (is (= 1 (sci/eval-string "(def x #js {:foo_bar (fn [] 1)}) (.foo-bar x)" {:classes {:allow :all}})))
     (testing "reserved keyword munging is bypassed"
       (is (= 1 (sci/eval-string "(def x #js {:catch (fn [] 1)}) (.catch x)" {:classes {:allow :all}})))
       (is (= 1 (sci/eval-string "d/foo-bar" {:imports {'d 'dude}
                                              :classes {:allow :all 'dude #js {:foo_bar 1}}})))
       (is (= 1 (sci/eval-string "(d/foo-bar)" {:imports {'d 'dude}
                                                :classes {:allow :all 'dude #js {:foo_bar (fn [] 1)}}}))))
     (is (= {:foo_bar 1} (sci/eval-string "(js->clj (doto #js {} (set! -foo-bar 1)) :keywordize-keys true)" {:classes {:allow :all}})))
     (testing "dotted access"
       (is (= 2 (sci/eval-string "(def x #js {:a 1 :foo_bar #js {:catch 2}}) x.foo-bar.catch" {:classes {'js goog/global}})))
       (is (= 3 (sci/eval-string "(let [a #js {:foo_bar #js {:catch 3}}] a.foo-bar.catch)"))))))

#?(:clj
   (deftest issue-987-deftype-munged-fields-test
     ;; these cases don't work in CLJS yet because {:classes {:allow :all}} takes the fast path
     ;; perhaps we can fix this by exposing the deftype as an Object in CLJS with mutated fields
     (is (= 1 (sci/eval-string "(deftype Foo [foo-bar]) (.-foo-bar (->Foo 1))" {:classes {:allow :all}})))
     ;; this doesn't work yet either, but not common in Clojure
     #_(is (= 1 (sci/eval-string "(deftype Foo [foo-bar]) (.-foo_bar (->Foo 1))" {:classes {:allow :all}})))))
