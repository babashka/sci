(ns sci.interrupt-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.interrupt :as interrupt]))

(defn limit-interrupt
  "Signals via a plain ex-info. Sandboxed code can catch this. Used to test the
  generic :interrupt-fn mechanism (interrupt-fn may throw any exception)."
  [n]
  (let [counter (atom 0)]
    (fn []
      (when (> (swap! counter inc) n)
        (throw (ex-info "interrupted" {:type :interrupt}))))))

(defn limit-interrupt!
  "Signals via sci.interrupt/interrupt!, the canonical way: the interrupt is
  uncatchable from within sandboxed code."
  [n]
  (let [counter (atom 0)]
    (fn []
      (when (> (swap! counter inc) n)
        (interrupt/interrupt!)))))

(defn interrupt-init
  "Context with the opt-in sci.interrupt core overrides merged in."
  [n]
  (sci/init {:interrupt-fn (limit-interrupt! n)
             :namespaces {'clojure.core   interrupt/clojure-core
                          'clojure.string interrupt/clojure-string}}))

(def ^:private catch-all-loop
  #?(:clj  "(try (loop [] (recur)) (catch Exception _ :swallowed))"
     :cljs "(try (loop [] (recur)) (catch :default _ :swallowed))"))

(deftest interrupt-uncatchable-test
  (testing "interrupt! cannot be caught by sandboxed try/catch"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 100)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* ctx catch-all-loop)))))
  (testing "finally still runs while the interrupt propagates"
    (let [a (atom nil)
          ctx (sci/init {:interrupt-fn (limit-interrupt! 100)
                         :namespaces {'user {'mark (fn [] (reset! a :ran))}}})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* ctx "(try (loop [] (recur)) (finally (mark)))")))
      (is (= :ran @a)))))

(deftest interrupt-not-maskable-by-finally-test
  (testing "a finally that throws cannot mask the interrupt and let an outer catch swallow it"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 100)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string*
             ctx
             #?(:clj  "(try (try (loop [] (recur)) (finally (throw (ex-info \"mine\" {})))) (catch Exception _ :swallowed))"
                :cljs "(try (try (loop [] (recur)) (finally (throw (ex-info \"mine\" {})))) (catch :default _ :swallowed))"))))))
  (testing "a non-interrupt exception is still masked by a throwing finally (Clojure semantics)"
    (let [ctx (sci/init {})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"b"
            (sci/eval-string*
             ctx
             "(try (throw (ex-info \"a\" {})) (finally (throw (ex-info \"b\" {}))))"))))))

(deftest interrupt-forge-resistant-test
  (testing "sandboxed code cannot forge an uncatchable interrupt by guessing the key"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 100000)})]
      (is (= :caught
             (sci/eval-string*
              ctx
              #?(:clj  "(try (throw (ex-info \"x\" {:sci.impl/interrupt :sci.impl/interrupt})) (catch Exception _ :caught))"
                 :cljs "(try (throw (ex-info \"x\" {:sci.impl/interrupt :sci.impl/interrupt})) (catch :default _ :caught))")))))))

(deftest arbitrary-exception-test
  (testing "interrupt-fn may throw an arbitrary exception; it propagates to the host when uncaught"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 500)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"interrupted"
            (sci/eval-string* ctx "(loop [] (recur))")))))
  (testing "unlike interrupt!, a plain exception can be caught by sandboxed code"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt 100)})]
      (is (= :swallowed (sci/eval-string* ctx catch-all-loop))))))

(deftest loop-forms-test
  (testing "interrupt-fn fires in loop/recur and derived forms (dotimes, while)"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 500)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* ctx "(loop [] (recur))")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* (sci/init {:interrupt-fn (limit-interrupt! 500)})
                              "(dotimes [_ 1000000] nil)"))))))

(deftest mutual-recursion-test
  (testing "interrupt-fn fires on every fn entry, catching mutual recursion"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 200)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* ctx "(declare b) (defn a [] (b)) (defn b [] (a)) (a)"))))))

(deftest direct-recursion-no-recur-test
  (testing "interrupt-fn fires on fn entry for non-recur self-calls"
    ;; low limit to fire well before JVM stack overflow
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 50)})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* ctx "(defn f [] (f)) (f)"))))))

(deftest no-interrupt-fn-test
  (testing "absent interrupt-fn does not affect execution"
    (let [ctx (sci/init {})]
      (is (= 10 (sci/eval-string* ctx "(loop [i 0] (if (= i 10) i (recur (inc i))))")))
      (is (= 99 (sci/eval-string* ctx "(dotimes [i 100] i) 99"))))))

(deftest normal-completion-under-budget-test
  (testing "execution completes normally when budget is not exceeded"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 10000)})]
      (is (= 100 (sci/eval-string* ctx "(loop [i 0] (if (= i 100) i (recur (inc i))))")))
      (is (= 45 (sci/eval-string* ctx "(reduce + (range 10))"))))))

(deftest host-seq-producers-test
  (testing "opt-in interruptible range/repeat/cycle/iterate fire interrupt-fn"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (range))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (repeat :x))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (cycle [1 2 3]))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(doall (iterate inc 0))")))))

(deftest host-materializers-test
  (testing "opt-in interruptible doall/dorun/count/into/reduce fire interrupt-fn on host sequences"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(reduce + (range))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(count (range))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
          (sci/eval-string* (interrupt-init 500) "(into [] (range))")))))

#?(:clj
   (deftest regex-redos-test
     (testing "interrupt-fn fires during regex backtracking (ReDoS), aborting the match"

       (testing "re-matches"         
         ;; ^(.*a){20}$ backtracks catastrophically on all-'a' input with a non-matching tail       
         (let [evil "(re-matches #\"^(.*a){20}$\" (apply str (conj (vec (repeat 28 \\a)) \\!)))"]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (sci/eval-string* (interrupt-init 100000) evil)))))

       (testing "re-matcher"
         (let [evil "(re-matcher #\"^(.*a){20}$\" (apply str (conj (vec (repeat 28 \\a)) \\!)))"]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (re-find (sci/eval-string* (interrupt-init 100000) evil)))))
         (let [evil "(re-find (re-matcher #\"^(.*a){20}$\" (apply str (conj (vec (repeat 28 \\a)) \\!))))"]
           (is (thrown-with-msg? Exception #"Interrupted"
                               (sci/eval-string* (interrupt-init 100000) evil))))))
     
     (testing "clojure.string/split"         
         ;; ^(.*a){20}$ backtracks catastrophically on all-'a' input with a non-matching tail       
         (let [evil "(clojure.string/split (apply str (conj (vec (repeat 28 \\a)) \\!)) #\"^(.*a){20}$\")"]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (sci/eval-string* (interrupt-init 100000) evil)))))

     (testing "clojure.string/replace"         
         ;; ^(.*a){20}$ backtracks catastrophically on all-'a' input with a non-matching tail       
         (let [evil "(clojure.string/replace (apply str (conj (vec (repeat 28 \\a)) \\!)) #\"^(.*a){20}$\" \"X\")"]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (sci/eval-string* (interrupt-init 100000) evil))))

         (let [evil "(clojure.string/replace (apply str (conj (vec (repeat 28 \\a)) \\!)) #\"^(.*a){20}$\" (constantly \"X\"))("]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (sci/eval-string* (interrupt-init 100000) evil)))))

     (testing "clojure.string/replace-first"         
         ;; ^(.*a){20}$ backtracks catastrophically on all-'a' input with a non-matching tail       
         (let [evil "(clojure.string/replace-first (apply str (conj (vec (repeat 28 \\a)) \\!)) #\"^(.*a){20}$\" \"X\")"]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (sci/eval-string* (interrupt-init 100000) evil))))

         (let [evil "(clojure.string/replace-first (apply str (conj (vec (repeat 28 \\a)) \\!)) #\"^(.*a){20}$\" (constantly \"X\"))"]
           (is (thrown-with-msg? Exception #"Interrupted"
                                 (sci/eval-string* (interrupt-init 100000) evil)))))

     (testing "re-matcher/re-matches/re-find/re-seq stay correct with interrupt-fn active"
       (let [ctx (interrupt-init 1000000)]
         (is (= "12345" (sci/eval-string* ctx "(re-find (re-matcher #\"\\d+\" \"abc12345def\"))")))
         (is (= ["12-34" "12" "34"] (sci/eval-string* ctx "(re-matches #\"(\\d+)-(\\d+)\" \"12-34\")")))
         (is (= "123" (sci/eval-string* ctx "(re-find #\"\\d+\" \"ab123\")")))
         (is (= ["1" "22" "333"] (sci/eval-string* ctx "(re-seq #\"\\d+\" \"a1b22c333\")")))))

     (testing "clojure.string/split stays correct with interrupt-fn active"
       (let [ctx (interrupt-init 1000000)]
         (is (= ["Sci" "is" "awesome!"] (sci/eval-string* ctx "(clojure.string/split \"Sci is awesome!\" #\" \")")))
))

     (testing "clojure.string/replace stays correct with interrupt-fn active"
       (let [ctx (interrupt-init 1000000)]
         (is (= "The color is blue, the sky is blue" (sci/eval-string* ctx "(clojure.string/replace \"The color is red, the sky is red\" #\"red\" \"blue\")")))
         (is (= "Thee cooloor iis reed." (sci/eval-string* ctx "(clojure.string/replace \"The color is red.\" #\"[aeiou]\"  #(str %1 %1))")))))

     (testing "clojure.string/replace-first stays correct with interrupt-fn active"
       (let [ctx (interrupt-init 1000000)]
         (is (= "The color is blue" (sci/eval-string* ctx "(clojure.string/replace-first \"The color is red\" #\"red\" \"blue\")")))
         (is (= "Thee color is red." (sci/eval-string* ctx "(clojure.string/replace-first \"The color is red.\" #\"[aeiou]\"  #(str %1 %1))")))))))



(deftest clojure-core-without-interrupt-fn-test
  (testing "merging clojure-core overrides without :interrupt-fn falls back to native behavior"
    (let [ctx (sci/init {:namespaces {'clojure.core interrupt/clojure-core}})]
      (is (= [0 1 2] (sci/eval-string* ctx "(vec (range 3))")))
      (is (= 3       (sci/eval-string* ctx "(count [1 2 3])")))
      (is (= 6       (sci/eval-string* ctx "(reduce + [1 2 3])")))
      (is (= [1 1 1] (sci/eval-string* ctx "(vec (take 3 (repeat 1)))")))
      (is (= [0 1 2] (sci/eval-string* ctx "(vec (take 3 (iterate inc 0)))"))))))

(deftest clojure-core-arity-parity-test
  (testing "overrides preserve clojure.core arities and empty-coll behavior"
    (let [ctx (sci/init {:namespaces {'clojure.core interrupt/clojure-core}})]
      ;; into 0/1-arity must not break (regression: only 2/3-arity were defined)
      (is (= []      (sci/eval-string* ctx "(into)")))
      (is (= [1 2]   (sci/eval-string* ctx "(into [1 2])")))
      (is (= [1 2 3] (sci/eval-string* ctx "(into [1] [2 3])")))
      (is (= {:a 1}  (sci/eval-string* ctx "(into {} [[:a 1]])")))
      ;; cycle on empty coll matches clojure.core (() not nil)
      (is (= ()      (sci/eval-string* ctx "(doall (cycle []))"))))
    ;; same with interrupt-fn active
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 500)
                         :namespaces {'clojure.core interrupt/clojure-core}})]
      (is (= [1 2 3] (sci/eval-string* ctx "(into [1] [2 3])")))
      (is (= ()      (sci/eval-string* ctx "(doall (cycle []))"))))))

(deftest fork-preserves-interrupt-fn-test
  (testing "forked context inherits interrupt-fn"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 1000)})
          forked (sci/fork ctx)]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* forked "(loop [] (recur))"))))))

(deftest merge-opts-preserves-interrupt-fn-test
  (testing "merge-opts carries interrupt-fn forward when not overridden"
    (let [ctx (sci/init {:interrupt-fn (limit-interrupt! 1000)})
          ctx2 (sci/merge-opts ctx {:namespaces {'user {'x 1}}})]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Interrupted"
            (sci/eval-string* ctx2 "(loop [] (recur))"))))))
