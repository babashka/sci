(ns sci.jit-test
  "Differential tests for the JS codegen tier: the same program evaluated
  jitted and interpreted must agree on values, error messages and error
  locations."
  (:require [clojure.test :as t :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.jit :as jit]))

;; strict compile: an emitter exception must fail the test, not hide behind
;; a silent interpreter fallback (which the differential checks can't see,
;; since both sides then run the interpreter and agree). other test
;; namespaces expect the default (jit on): restore after each test.
(vreset! jit/strict-compile? true)
(t/use-fixtures :each (fn [f] (try (f) (finally (jit/enable!)))))

(defn- eval-both
  ([src] (eval-both src nil))
  ([src opts]
   (jit/disable!)
   (let [interp (try {:val (pr-str (sci/eval-string* (sci/init opts) src))}
                     (catch :default e
                       {:err (.-message e)
                        :loc ((juxt :line :column) (ex-data e))}))]
     (jit/enable!)
     (let [jitted (try {:val (pr-str (sci/eval-string* (sci/init opts) src))}
                       (catch :default e
                         {:err (.-message e)
                          :loc ((juxt :line :column) (ex-data e))}))]
       (jit/disable!)
       [interp jitted]))))

(defn- agree?
  ([src] (agree? src nil))
  ([src opts]
   (let [[interp jitted] (eval-both src opts)]
     (is (= interp jitted) (str src " " (pr-str opts)))
     interp)))

(deftest jit-differential-test
  (testing "values"
    (doseq [src ["(loop [i 0 j 1000] (if (zero? j) i (recur (inc i) (dec j))))"
                 "((fn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) 15)"
                 "(do (defn f [x] (inc x)) (defn g [] (f 1)) (def r1 (g)) (defn f [x] (dec x)) [r1 (g)])"
                 "(let [f :a] (f {:a 1}))"
                 "(loop [i 0] (case i 5 i (recur (inc i))))"
                 "(loop [i 0] (if (< i 3) (recur (inc i)) (try (inc i) (catch :default e -1))))"
                 "(apply (fn [& xs] (count xs)) (range 5))"
                 "((fn ([x] x) ([x y] (+ x y))) 1 2)"
                 "(do (def ^:dynamic *d* 1) (defn h [] *d*) (binding [*d* 2] (h)))"
                 "(do (def mf (with-meta (fn [] 1) {:a 1})) [(mf) (:a (meta mf))])"
                 "((fn fact [n] (if (zero? n) 1 (* n (fact (dec n))))) 10)"
                 "(do (defn compose [f g] (fn [x] (f (g x)))) ((compose inc dec) 5))"
                 "((fn [a b & more] [a b (vec more)]) 1 2 3 4)"
                 "(mapv (fn [[k v]] [v k]) {:a 1 :b 2})"
                 "((fn [x] [x {:a x :b [x 1]} #{x}]) 3)"
                 "((fn [a b] (or (and a b) [a b])) nil 1)"
                 "(loop [i 0] (or (when (< i 3) (recur (inc i))) i))"
                 "(mapv (fn [k] (case k :a 1 :b 2 [1 2] :vec 0)) [:a :b [1 2] :c])"
                 "((fn [x] (case x \"s\" :str 5 :five nil :nil :other)) \"s\")"
                 "(try ((fn [x] (case x 1 :one)) 42) (catch :default e (ex-message e)))"
                 "((fn [n] (let [f (fn [x] (+ x n))] (f 10))) 5)"
                 "((fn [n] (let [add (fn [x] (fn [y] (+ x y n)))] ((add 1) 2))) 10)"
                 "((fn [n] (let [fib (fn fib [i] (if (< i 2) i (+ (fib (- i 1)) (fib (- i 2)))))] (fib n))) 10)"
                 "(let [mk (fn [n] (fn [] n)) fs (mapv mk [1 2 3])] (mapv (fn [f] (f)) fs))"
                 "((fn [n] ((comp inc (fn [x] (* x n))) 3)) 5)"
                 "(let [counter (fn [] (let [state (atom 0)] (fn [] (swap! state inc)))) c (counter)] [(c) (c)])"
                 "((fn [x] (js/Math.abs (js/Math.sin x))) -1)"
                 "((fn [x] (vec (js/Array. x 2))) 1)"
                 "(try ((fn [x] (js/Math.nope x)) 1) (catch :default e (ex-message e)))"]
            ;; interop asts only attach under ctx :unrestricted, so run both
            opts [nil {:unrestricted true}]]
      (agree? src opts)))
  (testing "errors and locations"
    (doseq [src ["(defn bar [x] (subs x 0)) (defn foo [] (bar nil)) (foo)"
                 "((fn [x] (pos? x)) (take 1 1))"
                 "((fn [v] (nth v 9)) [1 2 3])"
                 "(do (defn g [m] (assoc m :k 1)) (g 3))"]]
      (let [res (agree? src)]
        (is (:err res) (str "expected an error: " src))))))

(deftest jit-operator-parity-test
  ;; the operator-inlining table claims fn-body == JS operator; the cljs ==
  ;; FN (structural -equiv) vs == MACRO (===) mismatch proved hand
  ;; verification insufficient, so pin every candidate over a hostile matrix
  (let [vals-src ["0" "1" "-1" "0.5" "##NaN" "##Inf" "1000000" "\"a\"" "\"\""
                  "nil" "true" "false" "[1 2]" "(vector 1 2)" "{:a 1}" ":k"]]
    (doseq [op '[+ - * < > <= >= == = inc dec zero? pos? neg? nil? not
                 min max rem mod quot unchecked-add unchecked-subtract]
            a vals-src
            b (case op
                (inc dec zero? pos? neg? nil? not) [nil]
                vals-src)]
      (let [src (if b
                  (str "((fn [x y] (" op " x y)) " a " " b ")")
                  (str "((fn [x] (" op " x)) " a ")"))
            [interp jitted] (eval-both src)]
        (is (= interp jitted) src)))))

(deftest jit-var-mutation-visibility-test
  ;; BOTH modes cache var derefs keyed on sci.impl.vars/var-epoch, so
  ;; interp/jit agreement alone can't catch a missed bump — each case
  ;; asserts the literal expected value. Every public mutation path must
  ;; be immediately visible through a warm call site.
  (doseq [[expected src]
          [["[1 2]" "(defn f [] 1) (defn call [] (f)) (def before (call)) (defn f [] 2) [before (call)]"]
           ["[1 2]" "(defn f [] 1) (defn call [] (f)) (def before (call)) (alter-var-root #'f (constantly (fn [] 2))) [before (call)]"]
           ["[1 2 1]" "(defn f [] 1) (defn call [] (f)) (def before (call)) [before (with-redefs [f (fn [] 2)] (call)) (call)]"]
           ["[1 2 1]" "(def ^:dynamic *f* (fn [] 1)) (defn call [] (*f*)) (def before (call)) [before (binding [*f* (fn [] 2)] (call)) (call)]"]
           ["[1 [2 3] 1]" "(def ^:dynamic *f* (fn [] 1)) (defn call [] (*f*)) [(call) (binding [*f* (fn [] 2)] [(call) (do (set! *f* (fn [] 3)) (call))]) (call)]"]]]
    (let [[interp jitted] (eval-both src)]
      (is (= {:val expected} interp) src)
      (is (= {:val expected} jitted) src))))

(deftest jit-interop-member-name-emission-test
  ;; interop member names are interpolated into generated JS source as a
  ;; JSON.stringify'd string literal, not a bare identifier, so a name that
  ;; is not a legal JS identifier (dashes, dollar) still reads the right key
  ;; instead of emitting broken source. (Quote/bracket injection is not
  ;; reachable from user source: member names come from reader symbols; the
  ;; JSON.stringify is defense in depth.)
  (doseq [src ["(let [o (js-obj \"we-ird\" 1)] (.-we-ird o))"
               "(let [o (js-obj \"a$b\" 2)] (.-a$b o))"
               "(let [o (js-obj \"toUpperCase\" 3)] (.-toUpperCase o))"
               "(.toUpperCase \"ab\")"
               "(.indexOf \"abcabc\" \"b\")"]]
    (let [[interp jitted] (eval-both src {:unrestricted true})]
      (is (= interp jitted) src))))

(deftest jit-registered-class-ctor-test
  ;; a class registered under a plain symbol takes analyze-new's :else arm
  ;; (distinct from the js/X. static-access ctor), incl. required JS libs
  (let [opts {:unrestricted true :classes {'MyDate js/Date 'MyArr js/Array}}]
    (doseq [src ["(.getFullYear (MyDate. 2020 0 1))"
                 "(vec (MyArr. 1 2 3))"
                 "(.-length (MyArr. 5))"
                 "((fn [y] (.getFullYear (MyDate. y 0 1))) 1999)"]]
      (let [[interp jitted] (eval-both src opts)]
        (is (= interp jitted) src)))))

(deftest jit-error-location-in-loop-test
  ;; loop* carries the loop form's meta on its synthesized call, and only
  ;; wrap-guaranteed stacks become jit sites (intern-stack!): both modes
  ;; must agree AND report the pinned location (a differential check alone
  ;; cannot catch an analyzer-level location shift, both modes move
  ;; together)
  (doseq [[src expected-loc]
          [["(defn g [] (loop [i 0] (.toUpperCase (pos? i))))\n(g)" [1 12]]
           ["(defmacro m [] (list (list 'fn [] (list '.toUpperCase nil))))\n(defn f [] (pos? (m)))\n(f)" [2 18]]
           ["(defn f0 [x] (pos? (loop [i 0 acc x] (if (< i 2) (recur (inc i) (.toUpperCase (> :k acc))) acc))))\n(f0 42)" [1 20]]]]
    (let [[interp jitted] (eval-both src {:unrestricted true})]
      (is (= interp jitted) src)
      (is (:err interp) src)
      (is (= expected-loc (:loc interp)) src))))

(deftest jit-fallback-test
  (testing "disabled jit still evaluates"
    (jit/disable!)
    (is (= 10 (sci/eval-string* (sci/init {}) "(loop [i 0] (if (< i 10) (recur (inc i)) i))")))))
