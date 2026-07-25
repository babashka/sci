(ns sci.destructure-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [sci.core :as sci]))

(defn eval* [form]
  (sci/eval-string (pr-str form)))

;; sci wraps both analysis and runtime errors in ex-info, which is not an
;; Exception on ClojureDart
(defn throws? [form]
  (try (eval* form)
       false
       (catch #?(:cljd cljd.core/ExceptionInfo
                 :clj Exception
                 :cljs js/Error) _ true)))

(deftest req!-test
  (let [m {:a 1, :b 2, :f nil, :g false, nil "nil"}]
    (is (throws? (list 'req! m :e)))
    (is (= 1 (eval* (list 'req! m :a))))
    (is (= "nil" (eval* (list 'req! m nil))))
    (is (= 2 (eval* (list 'req! m :b))))
    (is (nil? (eval* (list 'req! m :f))))
    (is (= false (eval* (list 'req! m :g)))))
  (testing "lookup follows get, not just maps"
    (is (throws? '(req! nil :a)))
    (is (= 2 (eval* '(req! [1 2] 1))))
    (is (throws? '(req! [1 2] 5)))
    (is (= :a (eval* '(req! #{:a} :a))))
    (is (throws? '(req! #{:a} :b)))))

(deftest some-vals-test
  (is (= {:a 1} (eval* '(some-vals {:a 1 :b nil}))))
  (is (nil? (eval* '(some-vals {:a nil}))))
  (is (nil? (eval* '(some-vals nil))))
  (is (= {:a false} (eval* '(some-vals {:a false}))))
  (is (= '{a 1} (eval* '(some-vals '{a 1 b nil})))))

(deftest keys-bang-test
  (testing ":keys! binds and throws when key missing"
    (is (= 1 (eval* '(let [{:keys! [a b]} {:a 1 :b 2}] a))))
    (is (throws? '(let [{:keys! [a b]} {:a 1}] a))))
  (testing ":keys! with & requires keys after & without binding them"
    (is (= 1 (eval* '(let [{:keys! [a & :b]} {:a 1 :b 2}] a))))
    (is (throws? '(let [{:keys! [a & :b]} {:a 1}] a))))
  (testing "nested maps with :keys! &"
    (let [m {:a 1 :b {:a 2 :b 3 :c 4 :d 42}}]
      (is (= [(:b m) 1 2 3 4]
             (eval* (list 'let ['{a :a {aa :a :as m :keys! [b c & :d]} :b} m]
                          '[m a aa b c]))))
      (is (throws? (list 'let ['{a :a {:keys! [b c & :d :e]} :b} m] 'a)))
      (is (throws? (list 'let ['{a :a {:keys! [b c & :d]} :b} (update m :b dissoc :c)] 'a)))))
  (testing "qualified names and declarators with :keys! &"
    (let [m {:foo/a 1 :b 2 :foo/c 3}]
      (is (= 1 (eval* (list 'let ['{:keys! [foo/a & :b]} m] 'a))))
      (is (= 2 (eval* (list 'let ['{:keys! [b & :foo/c]} m] 'b))))
      (is (throws? (list 'let ['{:keys! [b & :foo/c]} (dissoc m :b)] 'b)))
      (is (throws? (list 'let ['{:keys! [b & :foo/c]} (dissoc m :foo/c)] 'b)))
      (is (= 1 (eval* (list 'let ['{:foo/keys! [aa & :bb]} {:foo/aa 1 :bb 2}] 'aa))))))
  (testing "keys after & are not bound"
    (is (throws? '(let [{:keys! [a & :b]} {:a 1 :b 2}] b)))
    (is (throws? '(let [{:keys! [foo/a & :foo/c]} {:foo/a 1 :foo/c 2}] c))))
  (testing "& may appear only once"
    (is (throws? '(let [{:keys [a & :b & :c]} {:a 1}] a)))))

(deftest syms-bang-test
  (testing ":syms! binds and throws when key missing"
    (is (= 1 (eval* '(let [{:syms! [a b]} '{a 1 b 2}] a))))
    (is (throws? '(let [{:syms! [a b]} {:a 1}] a))))
  (testing ":syms! with & requires keys after & without binding them"
    (is (= 1 (eval* '(let [{:syms! [a & 'b]} '{a 1 b 2}] a))))
    (is (throws? '(let [{:syms! [a & 'b]} {:a 1}] a))))
  (testing "nested maps with :syms! &"
    (let [m '{a 1 b {a 2 b 3 c 4 d 42}}]
      (is (= [(get m 'b) 1 2 3 4]
             (eval* (list 'let ['{a 'a {aa 'a :as m :syms! [b c & 'd]} 'b} (list 'quote m)]
                          '[m a aa b c]))))
      (is (throws? (list 'let ['{:syms! [b c & 'd 'e]} (list 'quote (get m 'b))] 'b)))))
  (testing "qualified names with :syms! &"
    (let [m '{foo/a 1 b 2 foo/c 3}]
      (is (= 1 (eval* (list 'let ['{:syms! [foo/a & 'b]} (list 'quote m)] 'a))))
      (is (= 2 (eval* (list 'let ['{:syms! [b & 'foo/c]} (list 'quote m)] 'b))))
      (is (throws? (list 'let ['{:syms! [b & 'foo/c]} (list 'quote (dissoc m 'b))] 'b)))
      (is (= 1 (eval* (list 'let ['{:foo/syms! [aa & 'bb]} (list 'quote '{foo/aa 1 bb 2})] 'aa))))))
  (testing "keys after & are not bound"
    (is (throws? '(let [{:syms! [a & 'b]} '{a 1 b 2}] b)))))

(deftest strs-bang-test
  (testing ":strs! binds and throws when key missing"
    (is (= 1 (eval* '(let [{:strs! [a b]} {"a" 1 "b" 2}] a))))
    (is (throws? '(let [{:strs! [a b]} {:a 1}] a))))
  (testing ":strs! with & requires keys after & without binding them"
    (is (= 1 (eval* '(let [{:strs! [a & "b"]} {"a" 1 "b" 2}] a))))
    (is (throws? '(let [{:strs! [a & "b"]} {:a 1}] a))))
  (testing "nested maps with :strs! &"
    (let [m {"a" 1 "b" {"a" 2 "b" 3 "c" 4 "d" 42}}]
      (is (= [(get m "b") 1 2 3 4]
             (eval* (list 'let ['{a "a" {aa "a" :as m :strs! [b c & "d"]} "b"} m]
                          '[m a aa b c]))))
      (is (throws? (list 'let ['{a "a" {:strs! [b c & "d" "e"]} "b"} m] 'a)))
      (is (throws? (list 'let ['{a "a" {:strs! [b c & "d"]} "b"} (update m "b" dissoc "c")] 'a)))))
  (testing "keys after & are not bound"
    (is (throws? '(let [{:strs! [a & "b"]} {"a" 1 "b" 2}] b)))))

(deftest mixed-keys-after-amp-test
  (is (= 1 (eval* '(let [{:keys [a & 'b]} {:a 1}] a))))
  (is (= 1 (eval* '(let [{:keys! [a & 'b "c"]} {:a 1 (quote b) 2 "c" 3}] a))))
  (is (throws? '(let [{:keys! [a & 'b "c"]} {:a 1 (quote b) 2}] a))))

(deftest select-test
  (let [m {:a 1 :b 2 :c 3 :d 4
           'sa 10 'sb 20 'sc 30 'sd 40
           "stra" 100 "strb" 200 "strc" 300 "strd" 400
           :foo/x 1000 :foo/y 2000 :foo/z 3000
           :nested {:aa 1 'saa 10 "straa" 100}}
        sel (fn [binding] (eval* (list 'let [binding (list 'quote m)] 'sel)))]
    (testing "select picks up keys mentioned anywhere in the binding form"
      (is (= {:a 1 :b 2 :c 3 :d 4}
             (sel '{:keys [a b & :c] :keys! [d] :select sel})))
      (is (= '{sa 10 sb 20 sc 30 sd 40}
             (sel '{:syms [sa sb & 'sc] :syms! [sd] :select sel})))
      (is (= {"stra" 100 "strb" 200 "strc" 300 "strd" 400}
             (sel '{:strs [stra strb & "strc"] :strs! [strd] :select sel})))
      (is (= {:foo/x 1000 :foo/z 3000}
             (sel '{:foo/keys [x & :zz] :foo/keys! [z] :select sel}))))
    (testing "select descends into nested maps"
      (is (= '{:aa 1 saa 10}
             (sel '{{aa :aa saa 'saa :select sel} :nested})))
      (is (= '{:nested {:aa 1 saa 10}}
             (sel '{{aa :aa saa 'saa} :nested :select sel}))))
    (testing "select of everything equals :as"
      (is (true? (eval* (list 'let ['{:keys [a b c d]
                                     :syms [sa sb sc sd]
                                     :strs [stra strb strc strd]
                                     :foo/keys! [x y z]
                                     nest :nested
                                     :as mm
                                     :select sel} (list 'quote m)]
                              '(= sel mm))))))
    (testing "select doesn't fabricate maps"
      (is (nil? (eval* '(let [{{a :a} :n :select s} nil] s))))
      (is (= {} (eval* '(let [{{a :a} :n :select s} {}] s))))
      (is (= {:n nil} (eval* '(let [{{a :a} :n :select s} {:n nil}] s))))
      (is (= {:n {}} (eval* '(let [{{a :a} :n :select s} {:n {}}] s)))))
    (testing "defaults fill in missing keys"
      (is (= {:n {:a 42}} (eval* '(let [{{a :a :or {a 42}} :n :select s} nil] s))))
      (is (= {:n {:a 42}} (eval* '(let [{{a :a :or {a 42}} :n :select s} {:n nil}] s)))))))

(deftest all-test
  (is (= {:a 1 :b 2} (eval* '(let [{:keys [a] :all m} {:a 1 :b 2}] m))))
  (testing ":all keeps keys not mentioned in the binding form"
    (is (= {:n {:aa 1 :bb 2} :c 3}
           (eval* '(let [{{aa :aa} :n :all m} {:n {:aa 1 :bb 2} :c 3}] m)))))
  (testing ":all is augmented by defaults"
    (is (= {:a 42 :b 2} (eval* '(let [{:keys [a] :or {a 42} :all m} {:b 2}] m)))))
  (testing ":select and :all in the same binding form"
    (is (= [{:a 1} {:a 1 :b 2}]
           (eval* '(let [{:keys [a] :select s :all m} {:a 1 :b 2}] [s m]))))))

(deftest defaults-test
  (testing ":defaults binds a map of key to default value"
    (is (= {} (eval* '(let [{:defaults d :or {}} {}] d))))
    (is (= {:a 1} (eval* '(let [{:keys [a] :defaults d :or {:a 1}} {}] d))))
    (is (= {:a 1} (eval* '(let [{:keys [a] :defaults d :or {a 1}} {}] d)))))
  (testing ":defaults without :or is an error"
    (is (throws? '(let [{:defaults d} {}] d))))
  (testing "the same key can't have both a binding and a key default"
    (is (throws? '(let [{:keys [a] :defaults d :or {:a 1 a 1}} {}] d)))))

(deftest or-by-key-test
  (testing ":or accepts key -> val in addition to binding -> val"
    (is (= [1 42] (eval* '(let [{:keys [a b] :or {:b 42}} {:a 1}] [a b]))))
    (is (= [1 42] (eval* '(let [{:syms [a b] :or {'b 42}} '{a 1}] [a b]))))
    (is (= [1 42] (eval* '(let [{:strs [a b] :or {"b" 42}} {"a" 1}] [a b]))))))

(deftest or-strictness-test
  (testing "with :select, :all or :defaults every :or entry must be a bound key"
    (is (throws? '(let [{:keys [a] :or {z 42} :select s} {:a 1}] s)))
    (is (throws? '(let [{:keys [a & :b] :or {b 42} :select s} {:a 1}] s)))
    (is (throws? '(let [{:keys [a] :or {:z 42} :all m} {:a 1}] m)))
    (is (throws? '(let [{:defaults d :or {:a 1}} {}] d)))
    (is (throws? '(let [{:keys [a] :or {:a 1 :z 2} :select s} {:a 1}] s))))
  (testing "without the new directives :or is unchecked"
    (is (= 1 (eval* '(let [{:keys [a] :or {z 42}} {:a 1}] a))))))

(deftest required-key-default-test
  (is (throws? '(let [{:keys! [a] :or {a 1}} {}] a))))

(deftest unsupported-map-directive-test
  (is (throws? '(let [{:vals [a]} {:a 1}] a))))

;; PATCH: drop with the destructure patch if clojure/clojure keeps 208443ae
(deftest patch-or-default-refers-to-sibling-binding-test
  (is (= "Does not conform to :int"
         (eval* '(let [{:keys [pred] :exo/keys [message]
                        :or {message (str "Does not conform to " pred)}}
                       {:pred :int}]
                   message))))
  (testing ":defaults, :select and :all still evaluate a default once"
    (is (= [42 {:a 42} 1]
           (eval* '(let [n (atom 0) f (fn [] (swap! n inc) 42)
                         {:keys [a] :or {a (f)} :defaults d} {}]
                     [a d @n]))))))

(deftest binding-contexts-test
  (testing "fn params"
    (is (= [1 2] (eval* '((fn [{:keys! [a b]}] [a b]) {:a 1 :b 2}))))
    (is (throws? '((fn [{:keys! [a b]}] [a b]) {:a 1})))
    (is (= {:a 1} (eval* '((fn [{:keys [a] :select s}] s) {:a 1 :b 2})))))
  (testing "kwargs"
    (is (= 1 (eval* '((fn [& {:keys! [a]}] a) :a 1))))
    (is (throws? '((fn [& {:keys! [a]}] a) :b 1))))
  (testing "for"
    (is (= [1 2] (eval* '(vec (for [{:keys! [a]} [{:a 1} {:a 2}]] a)))))
    (is (throws? '(vec (for [{:keys! [a]} [{:a 1} {:b 2}]] a))))
    (is (= [{:a 1}] (eval* '(vec (for [{:keys [a] :select s} [{:a 1 :b 2}]] s))))))
  (testing "loop"
    (is (= {:a 1 :b 2} (eval* '(loop [{:keys [a] :all m} {:a 1 :b 2}] m))))))
