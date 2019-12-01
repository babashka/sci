(ns sci.vars-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest dynamic-var-test
  (testing "set var root binding"
    #?(:clj (is (thrown-with-msg? Exception #"root binding"
                                  (eval* "(def ^:dynamic x 1) (set! x 2) x")))
       :cljs (= 2 (eval* "(def ^:dynamic x 1) (set! x 2) x"))))
  (testing "set var thread-local binding"
    (is (= [0 1 2 0] (eval*
                      "(def a (atom []))
                     (defn add! [v] (swap! a conj v))
                     (def ^:dynamic x 0)
                     (add! x)
                     (binding [x 1]
                       (add! x)
                       (set! x (inc x))
                       (add! x))
                       (add! x)
                     @a"))))
  (testing "usage of var name evals to var value, but using it as var prints var name"
    (is (= "[1 #'x]" (eval* "(def ^:dynamic x 1) (str [x (var x)])"))))
  (testing "with-redefs"
    (is (= [10 0] (eval* "(def ^:redef x 0)
                          (def a (atom []))
                          (defn add! [v] (swap! a conj v))
                          (with-redefs [x 10] (add! x)) (add! x)
                          @a")))))

(deftest with-redefs-test
  (is (= [10 11 10]
         (eval* "(def a (atom []))
                 (defn add! [v] (swap! a conj v))
                 (def ^:dynamic x 10)
                 (add! x)
                 (with-redefs [x 11] (add! x)) (add! x)"))))

(deftest println-test
  (is (= "hello\n" (sci/with-out-str (eval* "(println \"hello\")")))))
