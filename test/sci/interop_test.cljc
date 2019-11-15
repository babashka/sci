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
     (is (= "hii" (eval* "(def x \"foo\") (-> x (.replace \\o \\i) (.replace \"f\" \"h\"))")))))

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
   (deftest import-test
     (is (some? (eval* "(import clojure.lang.ExceptionInfo) (type ExceptionInfo)")))))
