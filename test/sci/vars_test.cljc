(ns sci.vars-test
  (:require
   #?@(:cljd [] :clj [[sci.addons :as addons]])
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest dynamic-var-test
  (testing "set var root binding"
    (is (thrown-with-msg? #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error) #"root binding"
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
  (testing "set! on sci var from api"
    (let [foo (sci/new-dynamic-var 'foo 1)]
        (sci/with-bindings {foo @foo}
          (is (= 1 (sci/eval-string "*foo*" {:bindings {'*foo* foo}})))
          (sci/set! foo 2)
          (is (= 2 (sci/eval-string "*foo*" {:bindings {'*foo* foo}})))))))

(deftest binding-syntax-test
  (testing "no vector binding"
    (is (thrown-with-msg? #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error) #"vector"
                          (eval* "(def ^:dynamic x 1) (binding #{x 1})"))))
  (testing "not even bindings"
    (is (thrown-with-msg? #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error) #"even"
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
  (is (thrown-with-msg? #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
                        #"value of a macro"
                        (eval* "(defmacro foo []) foo")))
  (is (some? (eval* "(defmacro foo []) #'foo"))))

(deftest unbound-call-test
  (is (thrown-with-msg? #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
                        #"unbound fn: #'user/x"
                        (eval* "(def x) (x 1)"))))

#?(:cljd nil
   :clj
   (when-not tu/native?
     (deftest binding-conveyor-test
       (is (= 1 (tu/eval* "(def ^:dynamic x 0) (binding [x 1] @(future x))"
                          (addons/future {}))))
       (is (= 13 (tu/eval* "(def ^:dynamic x 10)
                              (binding [x (inc x)]
                                @(future (binding [x (inc x)] @(future (binding [x (inc x)] x)))))"
                           (addons/future {})))))

     (deftest forked-future-uses-child-world-test
       (let [parent (sci/init (addons/future {}))]
         (sci/eval-string*
          parent
          "(def state (atom 0))
           (def ^:dynamic *scope* :root)")
         (let [child (sci/fork parent)]
           (is (= [:bound 1]
                  (sci/eval-string*
                   child
                   "(binding [*scope* :bound]
                      @(future [*scope* (swap! state inc)]))")))
           (is (= [:root 0]
                  (sci/eval-string* parent "[*scope* @state]")))
           (is (= [:root 1]
                  (sci/eval-string* child "[*scope* @state]"))))))

     (deftest forked-future-participates-in-world-quiescence-test
       (let [entered (promise)
             release (promise)
             parent (sci/init
                     (addons/future
                      {:bindings {'block! (fn []
                                            (deliver entered true)
                                            @release)}}))
             child (sci/fork parent)
             task (sci/eval-string* child "(future (block!))")]
         (is (= true (deref entered 1000 ::timeout)))
         (let [forking (future (sci/fork child))]
           (try
             (is (= ::waiting (deref forking 50 ::waiting)))
             (finally
               (deliver release true)))
           (is (= true (deref task 1000 ::timeout)))
           (is (map? (deref forking 1000 ::timeout))))))))

#?(:cljd nil
   :clj
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
                               {:classes {'java.lang.Thread java.lang.Thread}}))))

     (deftest forked-bound-fn-uses-child-world-test
       (let [parent (sci/init {})]
         (sci/eval-string*
          parent
          "(def state (atom 0))
           (def ^:dynamic *scope* :root)")
         (let [child (sci/fork parent)
               f (sci/eval-string*
                  child
                  "(binding [*scope* :bound]
                     (bound-fn [] [*scope* (swap! state inc)]))")
               result (promise)]
           (.start (Thread. #(deliver result (f))))
           (is (= [:bound 1] (deref result 1000 ::timeout)))
           (is (= 0 (sci/eval-string* parent "@state")))
           (is (= 1 (sci/eval-string* child "@state"))))))))

#?(:cljd nil
   :clj
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
    (is (thrown-with-msg? #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error) #"bind non-dynamic"
                          (sci/with-bindings {1 1}
                            (sci/eval-string "*x*" {:bindings {'*x* 1}}))))))

(deftest binding-api-test
  (when-not tu/native?
    (let [x (sci/new-dynamic-var 'x)]
      (is (= 1 (sci/binding [x 1]
                 (sci/eval-string "*x*" {:bindings {'*x* x}})))))))

(deftest world-scoped-dynamic-binding-test
  (let [parent-holder (atom nil)
        parent (sci/init
                {:bindings
                 {'read-parent #(sci/eval-string* @parent-holder "late")}})]
    (reset! parent-holder parent)
    (sci/eval-string* parent
                      "(def late 0) (def ^:dynamic shared 0)")
    (let [child (sci/fork parent)]
      (sci/eval-string* child
                        "(alter-meta! #'late assoc :dynamic true)")
      (testing "a child binding does not override a nested parent evaluation"
        (is (= [9 0 9]
               (sci/eval-string*
                child
                "(binding [late 9] [late (read-parent) late])"))))
      (testing "host API bindings deliberately enter either world"
        (let [shared (sci/eval-string* parent "#'shared")]
          (is (= [7 7]
                 (sci/with-bindings
                   {shared 7}
                   [(sci/eval-string* parent "shared")
                    (sci/eval-string* child "shared")])))))
      (is (= [0 0]
             [(sci/eval-string* parent "late")
              (sci/eval-string* child "late")])))))

#_(deftest with-redefs-api-test
    (when-not tu/native?
      (let [x (sci/new-dynamic-var 'x)]
        (is (= 1 (sci/with-redefs [x 1]
                   (sci/eval-string "x" {:bindings {'x x}}))))
        (is (str/includes? (str/lower-case (str @x)) "unbound")))
      (is (thrown-with-msg? #?(:clj Throwable :cljs js/Error) #"1 is not a var"
                            (sci/with-redefs [1 1])))))

#?(:cljd nil
   :clj
   (deftest pmap-test
     (when-not tu/native?
       (is (= '(11 11 11)
              (tu/eval* "(def ^:dynamic x 10) (binding [x 11] (pmap #(+ x %) [0 0 0]))"
                        (addons/future {})))))))

(def ^:dynamic *x* 10)

#?(:cljd nil
   :clj
   (deftest pmap-api-test
     (when-not tu/native?
       (let [x (sci/new-dynamic-var 'x 10)]
         (testing "sci future sees clojure bindings, futures in pmap see sci bindings"
           (is (= '(11 11 11)
                  @(binding [*x* 11] (sci/future (sci/binding [x *x*] (sci/pmap identity [@x @x @x])))))))))))

#?(:cljd nil
   :clj
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

#?(:cljd nil
   :clj
   (deftest forked-promise-state-test
     (when-not tu/native?
       (let [entered (promise)
             parent (sci/init
                     (-> (addons/future {})
                         (assoc :bindings
                                {'mark-entered
                                 #(deliver entered true)})))]
         (sci/eval-string*
          parent
          "(def pending (promise))
           (def blocked (promise))
           (def completed (promise))
           (deliver completed :done)")
         (let [child (sci/fork parent)]
           (is (= [false true :done :child :child]
                  (sci/eval-string*
                   child
                   "[(realized? pending)
                     (realized? completed)
                     @completed
                     @(deliver pending :child)
                     @(deliver pending :ignored)]")))
           (is (= [false :timeout]
                  (sci/eval-string*
                   parent
                   "[(realized? pending) (deref pending 1 :timeout)]")))
           (is (= [:parent :child]
                  [(sci/eval-string* parent "@(deliver pending :parent)")
                   (sci/eval-string* child "@pending")]))
           (let [waiting (sci/eval-string*
                          child
                          "(future (mark-entered) @blocked)")]
             (is (= true (deref entered 1000 ::timeout)))
             (sci/eval-string* child "(deliver blocked :released)")
             (is (= :released (deref waiting 1000 ::timeout)))))))))

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
         #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
         #"Built-in var"
         (sci/eval-string
          "[(with-redefs [*x* (fn [] 11)] (*x*)) (*x*)]" {:bindings {'*x* x}}))))
  (is (= {} (sci/eval-string "(with-redefs [assoc dissoc] (assoc {:a :b} :a :b))"
                             {:unrestricted true}))))

(deftest ctx-unrestricted-var-mutation-test
  (testing "ctx :unrestricted true allows built-in var mutation"
    (is (thrown-with-msg?
         #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
         #"Built-in var"
         (sci/eval-string "(with-redefs [assoc dissoc] (assoc {:a :b} :a :b))")))
    (is (= {} (sci/eval-string "(with-redefs [assoc dissoc] (assoc {:a :b} :a :b))"
                               {:unrestricted true}))))
  (testing "nested eval-string does not inherit the host eval's unrestrictedness"
    (is (thrown-with-msg?
         #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
         #"Built-in var"
         (sci/eval-string
          "(nested-eval \"(with-redefs [assoc dissoc] (assoc {:a :b} :a :b))\")"
          {:unrestricted true
           :namespaces {'user {'nested-eval (fn [s] (sci/eval-string s))}}})))
    (is (= {} (sci/eval-string
               "(nested-eval \"(with-redefs [assoc dissoc] (assoc {:a :b} :a :b))\")"
               {:unrestricted true
                :namespaces {'user {'nested-eval
                                    (fn [s] (sci/eval-string s {:unrestricted true}))}}})))))

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
       #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
       #"even"
       (sci/eval-string
        "(with-local-vars [x] (+ 1 (var-get x)))")))
  (is (thrown-with-msg?
       #?(:cljd cljd.core/ExceptionInfo :clj Exception :cljs js/Error)
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

#?(:cljd nil
   :clj
   (deftest thread-binds
     (is (true?
          (sci/eval-string*
           (sci/init
            (addons/future {}))
           "@(future (load-string \"(set! *warn-on-reflection* true)\"))")))))
