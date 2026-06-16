(ns sci.interrupt-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.interrupt :as interrupt]))

(defn limit-interrupt [n]
  (let [counter (atom 0)]
    (fn []
      (when (> (swap! counter inc) n)
        (throw (ex-info "interrupted" {:type :interrupt}))))))

(defn interrupt-init
  "Context with the opt-in sci.interrupt core overrides merged in."
  [n]
  (sci/init {:interrupt-fn (limit-interrupt n)
             :namespaces {'clojure.core interrupt/overrides}}))

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

(deftest host-seq-producers-test
  (testing "opt-in interruptible range/repeat/cycle/iterate fire interrupt-fn"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (range))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (repeat :x))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (cycle [1 2 3]))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (iterate inc 0))")))))

(deftest host-materializers-test
  (testing "opt-in interruptible doall/dorun/count/into/reduce fire interrupt-fn on host sequences"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(reduce + (range))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(count (range))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
          (sci/eval-string* (interrupt-init 500) "(into [] (range))")))))

(deftest overrides-without-interrupt-fn-test
  (testing "merging overrides without :interrupt-fn falls back to native behavior"
    (let [ctx (sci/init {:namespaces {'clojure.core interrupt/overrides}})]
      (is (= [0 1 2] (sci/eval-string* ctx "(vec (range 3))")))
      (is (= 3       (sci/eval-string* ctx "(count [1 2 3])")))
      (is (= 6       (sci/eval-string* ctx "(reduce + [1 2 3])")))
      (is (= [1 1 1] (sci/eval-string* ctx "(vec (take 3 (repeat 1)))")))
      (is (= [0 1 2] (sci/eval-string* ctx "(vec (take 3 (iterate inc 0)))"))))))

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
