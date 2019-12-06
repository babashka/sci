(ns sci.vars-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.test-utils :as tu]
   [sci.core :as sci]
   [sci.addons :as addons]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest dynamic-var-test
  (testing "set var root binding"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"root binding"
                          (eval* "(def ^:dynamic x 1) (set! x 2) x"))))
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
    (is (= "[1 #'user/x]" (eval* "(def ^:dynamic x 1) (str [x (var x)])")))))

#_(deftest with-redefs-test
    (is (= [10 11 10]
           (eval* "(def a (atom []))
                 (defn add! [v] (swap! a conj v))
                 (def ^:dynamic x 10)
                 (add! x)
                 (with-redefs [x 11] (add! x)) (add! x)"))))

(deftest redefine-var-test
  (is (= 11 (eval* "
(def x 10)
(defn foo [] x)
(def x 11)
(foo)")))
  (is (= 10 (eval* "
(defmacro foo [] `(+ 1 2 3 4))
(defn bar [] (foo))
(defmacro foo [] `(+ 1 2 3))
(bar)
")))
  (is (= 6 (eval* "
(defmacro foo [] `(+ 1 2 3 4))
(defn bar [] (foo))
(defmacro foo [] `(+ 1 2 3))
(defn bar [] (foo))
(bar)
")))
  (is (= 2 (eval* "
(defn foo [] 1)
(defn bar [] (foo))
(defn foo [] 2)
(bar)
"))))

(deftest const-test
  (is (= 10 (eval* "
(def ^:const x 10)
(defn foo [] x)
(def x 11)
(foo)"))))

(deftest var-call-test
  (is (= 1 (eval* "(defn foo [] 1) (#'foo)")))
  (is (= 11 (eval* "(defn foo [x] (inc x)) (#'foo 10)")))
  (is (= 10 (eval* "(defn foo [& xs] (apply + xs)) (apply #'foo 1 2 3 [4])"))))

(deftest macro-val-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"value of a macro"
                        (eval* "(defmacro foo []) foo")))
  (is (some? (eval* "(defmacro foo []) #'foo"))))

(deftest unbound-call-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"unbound fn: #'user/x"
                        (eval* "(def x) (x 1)"))))

#?(:clj
   (when-not tu/native?
     (deftest binding-conveyor-test
       (is (= 1 (tu/eval* "(def ^:dynamic x 0) (binding [x 1] @(future x))"
                          (addons/future {}))))
       (is (= 13 (tu/eval* "(def ^:dynamic x 10)
                              (binding [x (inc x)]
                                @(future (binding [x (inc x)] @(future (binding [x (inc x)] x)))))"
                           (addons/future {})))))))

(deftest with-bindings-api-test
  (when-not tu/native?
    (let [x (sci/new-dynamic-var 'x)]
      (is (= 1 (sci/with-bindings {x 1}
                 (sci/eval-string "*x*" {:bindings {'*x* x}})))))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"bind non-dynamic"
                          (sci/with-bindings {1 1}
                            (sci/eval-string "*x*" {:bindings {'*x* 1}}))))))

(deftest binding-api-test
  (when-not tu/native?
    (let [x (sci/new-dynamic-var 'x)]
      (is (= 1 (sci/binding [x 1]
                 (sci/eval-string "*x*" {:bindings {'*x* x}})))))))

#_(deftest with-redefs-api-test
    (when-not tu/native?
      (let [x (sci/new-dynamic-var 'x)]
        (is (= 1 (sci/with-redefs [x 1]
                   (sci/eval-string "x" {:bindings {'x x}}))))
        (is (str/includes? (str/lower-case (str @x)) "unbound")))
      (is (thrown-with-msg? #?(:clj Throwable :cljs js/Error) #"1 is not a var"
                            (sci/with-redefs [1 1])))))
