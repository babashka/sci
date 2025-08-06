(ns sci.vars-test
  (:require
   #?(:clj [sci.addons :as addons])
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.impl.unrestrict :refer [*unrestricted*]]
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
                      {:bindings {'*x* x}})))))
  (testing "dynamic binding of false works"
    (is (false? (sci/eval-string
                 "(def ^:dynamic x nil) (binding [x false] x)"))))
  (testing
      (let [foo (sci/new-dynamic-var 'foo 1)]
        (sci/with-bindings {foo @foo}
          (is (= 1 (sci/eval-string "*foo*" {:bindings {'*foo* foo}})))
          (sci/set! foo 2)
          (is (= 2 (sci/eval-string "*foo*" {:bindings {'*foo* foo}})))))))

(deftest binding-syntax-test
  (testing "no vector binding"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"vector"
                          (eval* "(def ^:dynamic x 1) (binding #{x 1})"))))
  (testing "not even bindings"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"even"
                          (eval* "(def ^:dynamic x 1) (binding [x])")))))

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
   (when-not tu/native?
     (deftest bound-fn-test
       (is (= :hello (tu/eval* "
(def ^:dynamic *some-var* nil)
(def state (promise))
(defn f [] (deliver state *some-var*))

(binding [*some-var* :hello]
  (.start (java.lang.Thread. (bound-fn* f))))
@state"
                               {:classes {'java.lang.Thread java.lang.Thread}})))
       (is (= :hello (tu/eval* "
(def ^:dynamic *some-var* nil)
(def state (promise))
(defn f [] (deliver state *some-var*))

(binding [*some-var* :hello]
  (.start (java.lang.Thread. (bound-fn [] (f)))))
@state"
                               {:classes {'java.lang.Thread java.lang.Thread}}))))))

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
                                      (deref x))"
                                   (-> (addons/future {})
                                       (assoc-in [:classes 'java.lang.Thread] Thread)))))
       (is (= :failed (tu/eval* "(let [x (promise)]
                                   (deref x 1 :failed))"
                                (addons/future {})))))))

(deftest def-returns-var-test
  (is (= "#'user/x" (eval* "(str (def x 1))")))
  (is (= "#'user/foo" (eval* "(str (defmacro foo []))"))))

(deftest def-within-binding-test
  (testing "emulation of clojure def within binding behavior"
    (is (= "#'bar/x" (eval* "(ns foo) (ns bar) (str (binding [*ns* (the-ns 'foo)] (def x 1)))")))))

(deftest alter-var-root-test
  (is (= 2 (eval* "(def x 1) (alter-var-root #'x (fn foo [v] (inc x))) x")))
  #?(:clj (testing "it is atomic"
            (is (= 1000 (sci/eval-string "(def x 0) (do (doall (pmap #(alter-var-root #'x (fn foo [v] (+ v %))) (take 1000 (repeat 1)))) x)"
                                         {:namespaces {'clojure.core {'pmap clojure.core/pmap}}})))))
  (testing "alter-var-root uses root binding to update"
    (is (= 2 (eval* "(def ^:dynamic *x* 1) (binding [*x* 2] (alter-var-root #'*x* inc)) *x*"))))
  (testing "alter-var-root returns new value"
    (is (= 2 (eval* "(def x 1) (alter-var-root #'x inc)")))))

(deftest with-redefs-test
  (is (= [2 1]  (eval* "(def x 1) [(with-redefs [x 2] x) x]")))
  (let [x (sci/new-dynamic-var '*x* (fn [] 10) {:ns (sci/create-ns 'user)
                                                :sci/built-in true})]
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error)
         #"Built-in var"
         (sci/eval-string
          "[(with-redefs [*x* (fn [] 11)] (*x*)) (*x*)]" {:bindings {'*x* x}}))))
  (binding [*unrestricted* true]
    (is (= {} (sci/eval-string "(with-redefs [assoc dissoc] (assoc {:a :b} :a :b))")))))

(deftest var-get-set-test
  (is (= "10\n11\n"
         (sci/with-out-str
           (sci/eval-string "
(def ^:dynamic x)
(binding [x 10]
  (prn (var-get #'x))
  (var-set #'x 11)
  (prn (var-get #'x)))")))))

(deftest with-local-vars-test
  (is (= 2 (eval* "(with-local-vars [x 1] (+ 1 (var-get x)))")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"even"
       (sci/eval-string
        "(with-local-vars [x] (+ 1 (var-get x)))")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"vector"
       (sci/eval-string
        "(with-local-vars #{x 1} (+ 1 (var-get x)))"))))

(deftest thread-bound?-test
  (is (false? (eval* "(def ^:dynamic *x*) (def ^:dynamic *y*) (thread-bound? #'*x* #'*x*)")))
  (is (true? (eval* "(def ^:dynamic *x*) (def ^:dynamic *y*)
    (binding [*x* *x* *y* *y*] (thread-bound? #'*x* #'*x*))"))))

(deftest add-watch-test
  (is (str/starts-with?
       (sci/with-out-str (sci/eval-string "(def x 1) (add-watch #'x :foo (fn [k r o n] (prn :o o :n n))) (alter-var-root #'x (constantly 5))"))
       ":o 1 :n 5")))

#?(:clj
   (deftest thread-binds
     (is (true?
          (sci/eval-string*
           (sci/init
            (addons/future {}))
           "@(future (load-string \"(set! *warn-on-reflection* true)\"))")))))
