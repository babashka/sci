(ns sci.vars-test
  (:require
   #?(:clj [sci.addons :as addons])
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci]
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
  (testing "dynamic vars are never directly linked, not even built in ones"
    (let [x (sci/new-dynamic-var '*x* (fn [] 10)
                                 {:ns (sci/create-ns 'user)
                                  #_#_:sci.impl/built-in true})]
      (is (= [11 10] (sci/eval-string
                      "[(binding [*x* (fn [] 11)] (*x*)) (*x*)]"
                      {:bindings {'*x* x}}))))))

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
#?(:clj
   (deftest with-bindings-test
     (is (= 6 (eval* "
(let [sw (java.io.StringWriter.)]
  (with-bindings {#'*out* sw}
    (println \"hello\"))
  (let [res (str sw)]
    (count res)))")))))

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


#?(:clj
   (deftest pmap-test
     (when-not tu/native?
       (is (= '(11 11 11)
              (tu/eval* "(def ^:dynamic x 10) (binding [x 11] (pmap #(+ x %) [0 0 0]))"
                        (addons/future {})))))))

(def ^:dynamic *x* 10)

#?(:clj
   (deftest pmap-api-test
     (when-not tu/native?
       (let [x (sci/new-dynamic-var 'x 10)]
         (testing "sci future sees clojure bindings, futures in pmap see sci bindings"
           (is (= '(11 11 11)
                  @(binding [*x* 11] (sci/future (sci/binding [x *x*] (sci/pmap identity [@x @x @x])))))))))))

#?(:clj
   (deftest promise-test
     (when-not tu/native?
       (is (= :delivered (tu/eval* "(let [x (promise)]
                                      (future (deliver x :delivered))
                                      (deref x 1 :failed))"
                                   (addons/future {}))))
       (is (= :failed (tu/eval* "(let [x (promise)]
                                   (deref x 1 :failed))"
                                (addons/future {})))))))

(deftest def-returns-var-test
  (is (= "#'user/x" (eval* "(str (def x 1))")))
  (is (= "#'user/foo" (eval* "(str (defmacro foo []))"))))

(deftest def-within-binding-test
  (testing "emulation of clojure def within binding behavior"
    (is (= "#'bar/x" (eval* "(str (ns foo) (ns bar) (binding [*ns* (the-ns 'foo)] (def x 1)))")))))

(deftest alter-var-root-test
  (is (= 2 (eval* "(def x 1) (alter-var-root #'x (fn foo [v] (inc x))) x")))
  #?(:clj (testing "it is atomic"
            (is (= 1000 (sci/eval-string "(def x 0) (do (doall (pmap #(alter-var-root #'x (fn foo [v] (+ v %))) (take 1000 (repeat 1)))) x)"
                                         {:namespaces {'clojure.core {'pmap clojure.core/pmap}}})))))
  (testing "alter-var-root uses root binding to update"
    (is (= 2 (eval* "(def ^:dynamic *x* 1) (binding [*x* 2] (alter-var-root #'*x* inc)) *x*")))))

(deftest with-redefs-test
  (is (= [2 1]  (eval* "(def x 1) [(with-redefs [x 2] x) x]")))
  (let [x (sci/new-dynamic-var '*x* (fn [] 10) {:ns (sci/create-ns 'user)
                                                :sci.impl/built-in true})]
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error)
         #"Built-in var"
         (sci/eval-string
          "[(with-redefs [*x* (fn [] 11)] (*x*)) (*x*)]" {:bindings {'*x* x}})))))
