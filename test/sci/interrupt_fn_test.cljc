(ns sci.interrupt-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [sci.core :as sci]))

(defn limit-interrupt [n]
  (let [counter (atom 0)]
    (fn []
      (when (> (swap! counter inc) n)
        (throw (ex-info "interrupted" {:type :interrupt}))))))

(deftest loop-forms-test
  (testing "interrupt-fn fires in loop/recur and derived forms (dotimes, while)"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 500)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* ctx "(loop [] (recur))")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* (sci/init {:interrupt-fn (limit-interrupt 500)})
                              "(dotimes [_ 1000000] nil)"))))))

(deftest mutual-recursion-test
  (testing "interrupt-fn fires on every fn entry, catching mutual recursion"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 200)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* ctx "(declare b) (defn a [] (b)) (defn b [] (a)) (a)"))))))

(deftest direct-recursion-no-recur-test
  (testing "interrupt-fn fires on fn entry for non-recur self-calls"
    ;; low limit to fire well before JVM stack overflow
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 50)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* ctx "(defn f [] (f)) (f)"))))))

(deftest no-interrupt-fn-test
  (testing "absent interrupt-fn does not affect execution"
    (let [ctx (sci/init {})]
      (is (= 10 (sci/eval-string* ctx "(loop [i 0] (if (= i 10) i (recur (inc i))))")))
      (is (= 99 (sci/eval-string* ctx "(dotimes [i 100] i) 99"))))))

(deftest normal-completion-under-budget-test
  (testing "execution completes normally when budget is not exceeded"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 10000)})]
      (is (= 100 (sci/eval-string* ctx "(loop [i 0] (if (= i 100) i (recur (inc i))))")))
      (is (= 45 (sci/eval-string* ctx "(reduce + (range 10))"))))))

(deftest fork-preserves-interrupt-fn-test
  (testing "forked context inherits interrupt-fn"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 1000)})
          forked (sci/fork ctx)]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* forked "(loop [] (recur))"))))))

(deftest merge-opts-preserves-interrupt-fn-test
  (testing "merge-opts carries interrupt-fn forward when not overridden"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 1000)})
          ctx2 (sci/merge-opts ctx {:namespaces {'user {'x 1}}})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* ctx2 "(loop [] (recur))"))))))
