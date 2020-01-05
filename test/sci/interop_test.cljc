(ns sci.interop-test
  (:require
   #?(:clj [clojure.test :as test :refer [deftest is testing]])
   [sci.test-utils :as tu]))

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
                                                    {:classes {'java.lang.Class Class}})))))))

#?(:clj
   (deftest static-methods
     (is (= 123 (eval* "(Integer/parseInt \"123\")")))
     (is (= 123 (eval* "(Integer/parseInt (str \"12\" \"3\") (inc 9))")))
     (testing "calling static methods on unconfigured classes is not allowed"
       (is (thrown-with-msg? Exception #"not"
                             (eval* "(System/exit 0)"))))))

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
