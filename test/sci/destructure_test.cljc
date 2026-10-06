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

(defn unresolved? [form sym]
  (try (eval* form)
       false
       (catch #?(:cljd cljd.core/ExceptionInfo
                 :clj Exception
                 :cljs js/Error) e
         (= (str "Unable to resolve symbol: " sym) (ex-message e)))))

(deftest unbound-after-amp-test
  (testing "a key after & binds no local"
    (is (unresolved? '(let [{:keys! [a & :b]} {:a 1 :b 2}] b) 'b))
    (is (unresolved? '(let [{a :a {aa :a :keys [b c & :e]} :b} {:a 1 :b {:a 2 :e 3}}] e) 'e))
    (is (unresolved? '(let [{:keys! [foo/a & :foo/c]} {:foo/a 1 :foo/c 3}] c) 'c))
    (is (unresolved? '(let [{:foo/keys! [foo/aa & :foo/cc]} #:foo{:aa 1 :cc 3}] cc) 'cc))
    (is (unresolved? '(let [{:syms! [a & 'b]} '{a 1 b 2}] b) 'b))
    (is (unresolved? '(let [{:strs! [a & "b"]} {"a" 1 "b" 2}] b) 'b))))

(deftest selector-arglists-test
  (is (= '([m]) (eval* '(:arglists (meta (var selector)))))))

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

(deftest excess-test
  (testing ":excess binds the input minus every selected key"
    (is (= {:b 2 :c 3}
           (eval* '(let [{:keys [a] :excess ex} {:a 1 :b 2 :c 3}] ex)))))
  (testing ":excess retains nil values"
    (is (= {:b 2 :z nil}
           (eval* '(let [{:keys [a] :excess ex} {:a 1 :b 2 :z nil}] ex))))
    (is (= {:bb nil}
           (eval* '(let [{{:keys [aa] :excess ex} :c} {:c {:aa 10 :bb nil}}] ex)))))
  (testing "nested maps contribute their own excess under the parent key"
    (is (= {:c 3 :n {:bb 20}}
           (eval* '(let [{:keys [a] {aa :aa} :n :excess ex}
                         {:a 1 :c 3 :n {:aa 10 :bb 20}}]
                     ex)))))
  (testing "a nested map without excess is left out"
    (is (= {:c 3}
           (eval* '(let [{:keys [a] {aa :aa} :n :excess ex}
                         {:a 1 :c 3 :n {:aa 10}}]
                     ex)))))
  (testing "nil when nothing is left"
    (is (nil? (eval* '(let [{:keys [a] {aa :aa} :n :excess ex}
                            {:a 1 :n {:aa 10}}]
                        ex))))
    (is (nil? (eval* '(let [{:excess ex} {}] ex))))
    (is (nil? (eval* '(let [{:keys [a] :excess ex} nil] ex)))))
  (testing "keys named after & count as selected"
    (is (= {:c 3}
           (eval* '(let [{:keys [a & :b] :excess ex} {:a 1 :b 2 :c 3}] ex)))))
  (testing ":or defaults stay out of :excess"
    (is (= [{:b 2} {:a 1 :z 99}]
           (eval* '(let [{:keys [a z] :or {z 99} :excess ex :select s} {:a 1 :b 2}]
                     [ex s])))))
  (testing ":excess applies to :syms, :strs and qualified keys"
    (is (= '{e 5} (eval* '(let [{:syms [d] :excess ex} '{d 4 e 5}] ex))))
    (is (= {"h" 7} (eval* '(let [{:strs [g] :excess ex} {"g" 6 "h" 7}] ex))))
    (is (= {:bar/y 2} (eval* '(let [{:foo/keys [x] :excess ex} {:foo/x 1 :bar/y 2}] ex)))))
  (testing ":select merged with :excess equals :all"
    (is (eval* '(let [{:keys [a] :select s :excess ex :all m} {:a 1 :b 2 :c 3}]
                  (= m (merge s ex)))))))

(deftest missing-test
  (testing ":missing collects absent required keys instead of throwing"
    (is (nil? (eval* '(let [{:keys! [a b] :missing m} {:a 1 :b 2}] m))))
    (is (= {:c nil} (eval* '(let [{:keys! [a b c] :missing m} {:a 1 :b 2}] m))))
    (is (= {:c nil} (eval* '(let [{:keys! [a b & :c] :missing m} {:a 1 :b 2}] m))))
    (is (= '{c nil} (eval* '(let [{:syms! [a c] :missing m} '{a 1}] m))))
    (is (= {"c" nil} (eval* '(let [{:strs! [a c] :missing m} {"a" 1}] m))))
    (is (= #:foo{:d nil} (eval* '(let [{:foo/keys! [a d] :missing m} {:foo/a 1}] m)))))
  (testing "a missing required key binds nil"
    (is (= [1 nil] (eval* '(let [{:keys! [a c] :missing m} {:a 1}] [a c])))))
  (testing "a present required key with a nil value is not missing"
    (is (nil? (eval* '(let [{:keys! [a] :missing m} {:a nil}] m)))))
  (testing "nested required keys are collected under the parent key"
    (is (= {:nest {:x nil :y nil}}
           (eval* '(let [{:keys! [a & :nest] {:keys! [x y]} :nest :missing m} {:a 0}] m))))
    (is (= {:nest {:x nil :y nil}}
           (eval* '(let [{:keys! [a & :nest] {:keys! [x y]} :nest :missing m} {:a 0 :nest nil}] m))))
    (is (= {:nest {:y nil}}
           (eval* '(let [{:keys! [a & :nest] {:keys! [x y]} :nest :missing m} {:a 0 :nest {:x 1}}] m))))
    (is (nil? (eval* '(let [{:keys! [a & :nest] {:keys! [x y]} :nest :missing m}
                            {:a 0 :nest {:x 1 :y 2}}]
                        m))))
    (is (= {:nest {:bb nil}}
           (eval* '(let [{:keys! [a] {:keys! [bb]} :nest :missing m} {:a 1 :nest {:aa 10}}] m))))))

(deftest selector-test
  (testing "selector requires a map with at least one directive"
    (is (throws? '(selector {:keys [a b]})))
    (is (throws? '(let [m {}] (selector m))))
    (is (throws? '(selector nil)))
    (is (throws? '(selector {}))))
  (testing "a single directive returns its value"
    (is (= {:a 1 :b 2 :c 3 :d 4}
           (eval* '((selector {:keys [a b & :c :z] :keys! [d] :select s}) {:a 1 :b 2 :c 3 :d 4 :e 5}))))
    (is (= {:a 1 :z 42}
           (eval* '((selector {:keys [a & :z] :select s :or {:z 42}}) {:a 1 :e 5}))))
    (is (= {:e 5} (eval* '((selector {:keys [a] :excess ex}) {:a 1 :e 5}))))
    (is (= {:a 1 :e 5} (eval* '((selector {:keys [a] :all m}) {:a 1 :e 5}))))
    (is (= {:d nil} (eval* '((selector {:keys! [d] :missing m}) {:a 1})))))
  (testing "a required key without :missing throws"
    (is (throws? '((selector {:keys! [d] :select s}) {:a 1}))))
  (testing "several directives return a map of directive to non-nil value"
    (is (= {:select {:a 1}}
           (eval* '((selector {:keys! [a] :select s :missing m}) {:a 1 :b 2}))))
    (is (= {:select {:a 1} :excess {:b 2}}
           (eval* '((selector {:keys [a] :select s :excess ex}) {:a 1 :b 2}))))))

(deftest or-by-key-test
  (testing ":or accepts key -> val in addition to binding -> val"
    (is (= [1 42] (eval* '(let [{:keys [a b] :or {:b 42}} {:a 1}] [a b]))))
    (is (= [1 42] (eval* '(let [{:syms [a b] :or {'b 42}} '{a 1}] [a b]))))
    (is (= [1 42] (eval* '(let [{:strs [a b] :or {"b" 42}} {"a" 1}] [a b]))))))

(deftest or-strictness-test
  (testing "with :select or :all every :or entry must be a bound key"
    (is (throws? '(let [{:keys [a] :or {z 42} :select s} {:a 1}] s)))
    (is (throws? '(let [{:keys [a & :b] :or {b 42} :select s} {:a 1}] s)))
    (is (throws? '(let [{:keys [a] :or {:z 42} :all m} {:a 1}] m)))
    (is (throws? '(let [{:keys [a] :or {:a 1 :z 2} :select s} {:a 1}] s))))
  (testing "without :select or :all :or is unchecked"
    (is (= 1 (eval* '(let [{:keys [a] :or {z 42}} {:a 1}] a))))))

(deftest required-key-default-test
  (is (throws? '(let [{:keys! [a] :or {a 1}} {}] a))))

(deftest unsupported-map-directive-test
  (is (throws? '(let [{:vals [a]} {:a 1}] a))))

;; PATCH: drop with the destructure patch if clojure/clojure keeps 208443ae
(deftest patch-or-default-refers-to-sibling-binding-test
  (is (= "Does not conform to :int"
         (eval* '(let [{:keys [pred message]
                        :or {message (str "Does not conform to " pred)}}
                       {:pred :int}]
                   message))))
  (testing ":select and :all evaluate a default once"
    (is (= [42 {:a 42} {:a 42} 1]
           (eval* '(let [n (atom 0) f (fn [] (swap! n inc) 42)
                         {:keys [a] :or {a (f)} :select s :all m} {}]
                     [a s m @n]))))))

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

(deftest merge-nil-argument-test
  (testing "merge with one nil argument returns the other argument"
    (is (eval* '(let [m (sorted-map :b 1 :a 2)] (identical? m (merge nil m)))))
    (is (eval* '(let [m {:a 1}] (identical? m (merge m nil)))))
    (is (= {:m 1} (eval* '(meta (merge nil (with-meta {:a 1} {:m 1}))))))))

(deftest all-select-keep-input-test
  (testing ":all without :or returns the input map"
    (is (eval* '(let [in (sorted-map :b 1 :a 2) {:keys [a] :all m} in] (identical? in m))))
    (is (= {:m 1} (eval* '(let [{:keys [a] :all m} (with-meta {:a 1} {:m 1})] (meta m))))))
  (testing ":select keeps the metadata of the input map"
    (is (= {:m 1} (eval* '(let [{:keys [a] :select s} (with-meta {:a 1 :b 2} {:m 1})] (meta s)))))))
