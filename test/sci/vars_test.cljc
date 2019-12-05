(ns sci.vars-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.test-utils :as tu]))

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
    (is (= "[1 #'user/x]" (eval* "(def ^:dynamic x 1) (str [x (var x)])"))))
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
   (defn future*
     [_ _ & body]
     `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
        (~'future-call f#))))

#?(:clj
   (when-not tu/native?
     (deftest binding-conveyor-test
       (is (= 1 (tu/eval* "(def ^:dynamic x 0) (binding [x 1] @(future x))"
                          {:bindings {'future (with-meta future* {:sci/macro true})
                                      'future-call future-call}}))))))
