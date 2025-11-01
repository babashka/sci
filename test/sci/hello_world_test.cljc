(ns sci.hello-world-test
  "Basic non-interop tests for SCI evaluation on ClojureCLR.
  
  These tests verify that SCI works correctly on ClojureCLR
  without relying on Java/CLR interop features.
  
  Note: Tests involving defn, loop/recur, destructuring, and complex
  features that require CLR reflection have been excluded, as the
  custom reflector has not yet been ported to C#."
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci]))

(deftest hello-world-test
  (testing "Simple string evaluation"
    (is (= "Hello, World!" (sci/eval-string "\"Hello, World!\""))))
  
  (testing "Simple arithmetic"
    (is (= 3 (sci/eval-string "(+ 1 2)")))
    (is (= 10 (sci/eval-string "(* 2 5)")))
    (is (= 4 (sci/eval-string "(- 7 3)")))
    (is (= 2 (sci/eval-string "(/ 8 4)")))))

(deftest literal-test
  (testing "Number literals"
    (is (= 42 (sci/eval-string "42")))
    (is (= 3.14 (sci/eval-string "3.14"))))
  
  (testing "Boolean literals"
    (is (= true (sci/eval-string "true")))
    (is (= false (sci/eval-string "false"))))
  
  (testing "Nil literal"
    (is (= nil (sci/eval-string "nil"))))
  
  (testing "String literals"
    (is (= "foo" (sci/eval-string "\"foo\""))))
  
  (testing "Keyword literals"
    (is (= :foo (sci/eval-string ":foo")))
    (is (= :foo/bar (sci/eval-string ":foo/bar"))))
  
  (testing "Symbol literals"
    (is (= 'foo (sci/eval-string "'foo")))))

(deftest collection-test
  (testing "Vector literals"
    (is (= [1 2 3] (sci/eval-string "[1 2 3]")))
    (is (= [] (sci/eval-string "[]"))))
  
  (testing "List literals"
    (is (= '(1 2 3) (sci/eval-string "'(1 2 3)")))
    (is (= '() (sci/eval-string "'()"))))
  
  (testing "Map literals"
    (is (= {:a 1 :b 2} (sci/eval-string "{:a 1 :b 2}")))
    (is (= {} (sci/eval-string "{}"))))
  
  (testing "Set literals"
    (is (= #{1 2 3} (sci/eval-string "#{1 2 3}")))
    (is (= #{} (sci/eval-string "#{}")))))

(deftest function-test
  (testing "Anonymous functions"
    (is (= 10 (sci/eval-string "((fn [x] (* x 2)) 5)")))
    (is (= 7 (sci/eval-string "((fn [a b] (+ a b)) 3 4)"))))
  
  (testing "Function literals"
    (is (= 8 (sci/eval-string "(#(* % 2) 4)")))
    (is (= 15 (sci/eval-string "(#(+ %1 %2) 7 8)")))))

(deftest conditional-test
  (testing "if expressions"
    (is (= :yes (sci/eval-string "(if true :yes :no)")))
    (is (= :no (sci/eval-string "(if false :yes :no)")))
    (is (= nil (sci/eval-string "(if false :yes)"))))
  
  (testing "when expressions"
    (is (= 5 (sci/eval-string "(when true 5)")))
    (is (= nil (sci/eval-string "(when false 5)")))))

(deftest sequence-operations-test
  (testing "map function"
    (is (= [2 4 6] (sci/eval-string "(map #(* % 2) [1 2 3])")))
    (is (= [2 3 4] (sci/eval-string "(map inc [1 2 3])"))))
  
  (testing "filter function"
    (is (= [2 4] (sci/eval-string "(filter even? [1 2 3 4])")))
    (is (= [1 3] (sci/eval-string "(filter odd? [1 2 3 4])"))))
  
  (testing "reduce function"
    (is (= 10 (sci/eval-string "(reduce + [1 2 3 4])")))
    (is (= 24 (sci/eval-string "(reduce * [1 2 3 4])")))))

(deftest do-test
  (testing "do expressions"
    (is (= 3 (sci/eval-string "(do 1 2 3)")))
    (is (= 5 (sci/eval-string "(do (def x 5) x)")))))

(deftest def-test
  (testing "def creates var"
    (is (= 42 (sci/eval-string "(def answer 42) answer")))
    (is (= "hello" (sci/eval-string "(def greeting \"hello\") greeting")))))

(deftest macro-test
  (testing "Built-in macros work"
    (is (= 4 (sci/eval-string "(-> 1 inc inc inc)")))
    (is (= 6 (sci/eval-string "(->> [1 2 3] (reduce +))"))))
  
  (testing "and/or macros"
    (is (= true (sci/eval-string "(and true true)")))
    (is (= false (sci/eval-string "(and true false)")))
    (is (= true (sci/eval-string "(or false true)")))
    (is (= false (sci/eval-string "(or false false)")))))

(deftest namespace-test
  (testing "Namespace operations"
    (is (some? (sci/eval-string "(ns foo.bar) *ns*")))
    (is (= 'foo.bar (sci/eval-string "(ns foo.bar) (ns-name *ns*)")))))

(deftest apply-test
  (testing "apply function"
    (is (= 10 (sci/eval-string "(apply + [1 2 3 4])")))
    (is (= 120 (sci/eval-string "(apply * [2 3 4 5])")))))

(deftest comp-test
  (testing "comp function"
    (is (= [2 3 4] (sci/eval-string "((comp vec (partial map inc)) [1 2 3])")))))

(deftest partial-test
  (testing "partial function"
    (is (= [2 3 4] (sci/eval-string "(map (partial + 1) [1 2 3])")))))
