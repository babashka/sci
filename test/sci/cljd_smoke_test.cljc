(ns sci.cljd-smoke-test
  "First namespaces that work on ClojureDart. Runs on all platforms.
  Also serves as a compilation canary: requires namespaces that have no
  cljd tests yet."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.impl.analyzer]
            [sci.impl.callstack]
            [sci.impl.copy-vars]
            [sci.impl.evaluator]
            [sci.impl.faster]
            [sci.impl.fns]
            [sci.impl.interpreter]
            [sci.impl.records]
            [sci.impl.resolve]
            [sci.impl.types :as types]
            [sci.lang]))

(deftest eval-form-test
  (is (= '(+ 1 2) (types/getVal (types/->EvalForm '(+ 1 2))))))

(deftest eval-string-test
  (is (= 6 (sci.impl.interpreter/eval-string "(+ 1 2 3)")))
  (is (= 3 (sci.impl.interpreter/eval-string "((fn [x] (+ x 1)) 2)")))
  (is (= [2 3 4] (sci.impl.interpreter/eval-string "(mapv inc [1 2 3])")))
  (is (= 10 (sci.impl.interpreter/eval-string "(let [x 4 y 6] (+ x y))")))
  (is (= "hello" (sci.impl.interpreter/eval-string "(str \"he\" \"llo\")")))
  (is (= 1 (sci.impl.interpreter/eval-string "(def x 1) x")))
  (is (= 2 (sci.impl.interpreter/eval-string "(defn f [] 2) (f)"))))

(defn- evals [s] (sci.impl.interpreter/eval-string s))

(defmacro twice-macro [x] `(do ~x ~x))

#?(:cljd
   (deftest host-macro-copy-test
     (let [v (sci.impl.copy-vars/copy-var twice-macro (sci.lang/->Namespace 'foo nil) {})]
       (is (= 2 (sci.impl.interpreter/eval-string "(foo/twice-macro 2)"
                                                  {:namespaces {'foo {'twice-macro v}}}))))))

#?(:cljd
   (deftest host-macro-copy-public-test
     (let [v (sci.impl.copy-vars/copy-var twice-macro (sci.lang/->Namespace 'foo nil)
                                          {:sci.impl/public true})]
       (is (= 2 (sci.impl.interpreter/eval-string "(foo/twice-macro 2)"
                                                  {:namespaces {'foo {'twice-macro v}}})))
       (is (= 2 (sci.impl.interpreter/eval-string "(foo/twice-macro (prn 1)) 2"
                                                  {:namespaces {'foo {'twice-macro v}}}))))))

(deftest eval-macros-test
  (testing "threading"
    (is (= 6 (evals "(-> 1 inc (+ 4))")))
    (is (= [2 3] (evals "(->> [1 2] (map inc) vec)")))
    (is (= 3 (evals "(as-> 1 x (inc x) (inc x))")))
    (is (= 2 (evals "(some-> {:a 1} :a inc)")))
    (is (nil? (evals "(some-> {:a 1} :b inc)")))
    (is (= 2 (evals "(some->> {:a 1} :a inc)")))
    (is (= 42 (evals "(doto (atom 41) (swap! inc)) 42"))))
  (testing "conditionals"
    (is (= :a (evals "(if-let [x true] :a :b)")))
    (is (= :b (evals "(if-not true :a :b)")))
    (is (= :a (evals "(if-some [x 1] :a :b)")))
    (is (= :a (evals "(when true :a)")))
    (is (= :a (evals "(when-not false :a)")))
    (is (= 1 (evals "(when-let [x 1] x)")))
    (is (= 1 (evals "(when-some [x 1] x)")))
    (is (= 1 (evals "(when-first [x [1 2]] x)")))
    (is (= :two (evals "(case 2 1 :one 2 :two :other)")))
    (is (= :small (evals "(cond (< 1 2) :small :else :big)")))
    (is (= 4 (evals "(cond-> 1 true inc true (+ 2))")))
    (is (= 3 (evals "(cond->> 1 true (+ 2))")))
    (is (= :two (evals "(condp = 2 1 :one 2 :two)"))))
  (testing "binding and iteration"
    (is (= 3 (evals "(let [x 1 y 2] (+ x y))")))
    (is (= 10 (evals "(loop [i 0] (if (< i 10) (recur (inc i)) i))")))
    (is (= [0 1 2] (evals "(vec (for [i (range 3)] i))")))
    (is (= [1 3] (evals "(vec (for [i (range 4) :when (odd? i)] i))")))
    (is (= 6 (evals "(let [a (atom 0)] (doseq [i [1 2 3]] (swap! a + i)) @a)")))
    (is (= 3 (evals "(let [a (atom 0)] (dotimes [_ 3] (swap! a inc)) @a)")))
    (is (= 5 (evals "(let [a (atom 0)] (while (< @a 5) (swap! a inc)) @a)")))
    (is (= 3 (evals "(letfn [(f [x] (g x)) (g [x] (inc x))] (f 2))"))))
  (testing "fns and vars"
    (is (= 1 (evals "((fn [x] x) 1)")))
    (is (= 2 (evals "(defn- private-f [] 2) (private-f)")))
    (is (= 1 (evals "(defonce once-x 1) (defonce once-x 2) once-x")))
    (is (= [3 4] (evals "(declare h2) (defn h1 [] [3 (h2)]) (defn h2 [] 4) (h1)")))
    (is (= 1 (evals "(defmacro m [] 1) (m)")))
    (is (= 4 (evals "(def ^:dynamic *d* 1) (binding [*d* 4] *d*)")))
    (is (= 2 (evals "(def vs (volatile! 1)) (vswap! vs inc)"))))
  (testing "misc"
    (is (nil? (evals "(comment (this is not evaluated))")))
    (is (= 1 (evals "(delay 2) (force (delay 1))")))
    (is (= [1 2 3 4] (evals "(vec (lazy-cat [1 2] [3 4]))")))
    (is (= [1 1 1] (evals "(vec (take 3 ((fn ones [] (lazy-seq (cons 1 (ones)))))))")))
    (is (= :ok (evals "(locking :dummy :ok)")))
    (is (= "1" (evals "(with-out-str (pr 1))")))
    (is (= 3 (evals "(and 1 2 3)")))
    (is (= 1 (evals "(or nil 1)")))
    (is (true? (evals "(assert true) true")))
    (is (= 6 (evals "(reduce + (map inc [0 1 2]))")))))

(deftest constant-test
  (testing "constants evaluate to themselves"
    (is (= 42 (types/eval (types/->constant 42) nil nil)))))

(deftest eval-node-test
  (is (false? (types/eval-node? 42))))

(deftest stack-test
  (testing "default stack is nil"
    (is (nil? (types/stack 42)))))

(deftest type-impl2-test
  (is (= :foo (types/type-impl2 (with-meta {} {:type :foo})))))
