(ns sci.interop-test
  (:require
   [clojure.test :as test :refer [deftest is #?(:clj testing)]]
   [sci.test-utils :as tu]
   #?(:cljs [clojure.string :as str])))

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
(foo/foo (ex-info \"message\" {}))"))))))
#?(:cljs
   (deftest instance-methods
     (is (= 102 (tu/eval* "(.charCodeAt \"foo\" 0)" {:classes {'String js/String}})))))

#?(:cljs
   (deftest instance-fields
     (is (= 1 (tu/eval* "(.-x (js-obj \"x\" 1))" {:classes {:allow :all}})))))

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
                             (eval* "(System/exit 0)"))))))

#?(:cljs
   (deftest static-methods
     (is (= \C (tu/eval* "(String/fromCharCode 67)" {:classes {'String js/String}})))
     (is (= 42 (tu/eval* "(js/parseInt \"42\")"     {:classes {'js js/global}})))))

#?(:cljs
   (deftest methods-on-static-fields
     (is (= 42 (tu/eval* "(do (.log js/console \"42\") 42)" {:classes {:allow :all
                                                                       'js js/global}})))
     (is (= 42 (tu/eval* "(do (js/console.log \"42\") 42)" {:classes {:allow :all
                                                                      'js js/global}})))))

#?(:cljs
   (deftest static-field-constructors
     (is (= 42 (tu/eval* "(js/parseInt (.-message (js/Error. \"42\")))" {:classes {:allow :all
                                                                                   'js js/global}})))))

;; TODO add test for special Objects (need browser for this)
#_(let [sse (js/EventSource. "/sse")]
        (.addEventListener sse "message" (fn [e] (.log js/console "Data" e))))


#?(:cljs
   (do (def fs (let [m (js->clj (js/require "fs"))]
                 (zipmap (map symbol (keys m)) (vals m))))
       (deftest add-object-as-namespace
         (is (str/includes?
              (tu/eval* "(str (fs/readFileSync \"README.md\"))" {:namespaces {'fs fs}})
              "EPL")))))

#?(:clj
   (deftest static-fields
     (is (some? (eval* "Integer/SIZE")))
     (testing "calling static field on unconfigured classes is not allowed"
       (is (thrown-with-msg? Exception #"not"
                             (eval* "System/PrintStream"))))))

#?(:clj
   (deftest constructor-test
     (is (= "dude" (eval* "(String. (str \"dude\"))")))
     (is (= "dude" (eval* "(new String (str \"dude\"))")))))

#?(:clj
   (deftest import-test
     (is (some? (eval* "(import clojure.lang.ExceptionInfo) ExceptionInfo")))
     (is (thrown-with-msg? Exception #"resolve.*at.*1"
                           (eval* "(import foo.bar.Baz)")))))

(deftest syntax-test
  (when-not tu/native?
    (doseq [expr ["(.)" "(. {})" "(.foo)"]]
      (is (thrown-with-msg? #?(:clj IllegalArgumentException :cljs js/Error)
                            #"Malformed"
                            (eval* expr))))))
