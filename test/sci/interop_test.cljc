(ns sci.interop-test
  (:require
   [clojure.test :as test :refer [deftest is testing #?(:cljs async)]]
   [sci.test-utils :as tu]
   #?(:clj [sci.core :as sci])
   #?(:cljs [clojure.string :as str]))
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
   (deftest static-fields
     (is (= 32 (eval* "Integer/SIZE")))
     (is (= 32 (eval* "(Integer/SIZE)")))
     (is (= 32 (eval* "(. Integer -SIZE)")))
     (is (= 32 (eval* "(. Integer SIZE)")))
     (testing "calling static field on unconfigured classes is not allowed"
       (is (thrown-with-msg? Exception #"not"
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
       (is (thrown-with-msg? Exception #"not"
                             (eval* "(clojure.lang.Var/find 'clojure.core/int)"))))))

(deftest syntax-test
  (when-not tu/native?
    (doseq [expr ["(.)" "(. {})" "(.foo)"]]
      (is (thrown-with-msg? #?(:clj IllegalArgumentException :cljs js/Error)
                            #"Malformed"
                            (eval* expr))))))

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
                          "3.14"))))

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
                                    'js js/global}})))))

#?(:cljs
   (def fs (let [m (js->clj (js/require "fs"))]
             (zipmap (map symbol (keys m)) (vals m)))))
#?(:cljs
   (deftest object-as-namespace-test
     (is (str/includes?
          (tu/eval* "(str (fs/readFileSync \"README.md\"))" {:namespaces {'fs fs}})
          "EPL"))))

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
   (deftest dot-in-js-invocation
     (is (str/includes?
          (tu/eval* "(first (js/process.argv.slice 0))"
                    {:classes
                     {'js goog/global :allow :all}})
          "node"))
     (is (tu/eval* "(js/Promise.all [])"
                   {:classes
                    {'js goog/global :allow :all}}))))
