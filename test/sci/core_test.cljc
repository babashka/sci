(ns sci.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest core-test
  (testing "do can have multiple expressions"
    (is (= 2 (if tu/native?
               (eval* '(do 0 1 2))
               (let [a (atom 0)]
                 (tu/eval*
                  '(do (f) (f)) {:bindings {'f #(swap! a inc)}})
                 @a)))))
  (testing "if and when"
    (is (= 1 (eval* 0 '(if (zero? *in*) 1 2))))
    (is (= 2 (eval* 1 '(if (zero? *in*) 1 2))))
    (is (= 1 (eval* 0 '(when (zero? *in*) 1))))
    (is (nil? (eval* 1 '(when (zero? *in*) 1))))
    (testing "when can have multiple body expressions"
      (is (= 2 (if tu/native?
                 (eval* '(when true 0 1 2))
                 (let [a (atom 0)]
                   (tu/eval*
                    '(when true (f) (f)) {:bindings {'f #(swap! a inc)}})
                   @a))))))
  (testing "and and or"
    (is (= false (eval* 0 '(and false true *in*))))
    (is (= 0 (eval* 0 '(and true true *in*))))
    (is (= 1 (eval* 1 '(or false false *in*))))
    (is (= false (eval* false '(or false false *in*))))
    (is (= 3 (eval* false '(or false false *in* 3)))))
  (testing "fn literals"
    (is (= 2 (eval* 1 "(#(+ 1 %) *in*)")))
    (is (= [1 2 3] (eval* 1 "(map #(+ 1 %) [0 1 2])")))
    (is (eval* 1 "(#(when (odd? *in*) *in*))"))
    (is (eval* 1 "(#(when (odd? *in*) *in*))"))
    (is (= 1 (eval* "(do (defn foo [] 1) (#(foo)))"))))
  (testing "map"
    (is (= [1 2 3] (eval* 1 '(map inc [0 1 2])))))
  (testing "keep"
    (is (= [false true false] (eval* 1 '(keep odd? [0 1 2])))))
  (testing "->"
    (is (= 4 (eval* 1 '(-> *in* inc inc (inc)))))
    (is (= '([0 1] [1 2] [2 3]) (eval* '(map-indexed #(-> [%1 %2]) [1 2 3]))))
    (is (= '(1 2 3) (eval* '(-> '(1 2 3))))))
  (testing "->>"
    (is (= 7 (eval* ["foo" "baaar" "baaaaaz"] "(->> *in* (map count) (apply max))"))))
  (testing "as->"
    (is (= "4444444444"
           (eval* '(as-> 1 x (inc x) (inc x) (inc x) (apply str (repeat 10 (str x))))))))
  (testing "literals"
    (is (= {:a 4
            :b {:a 2}
            :c [1 1]
            :d #{1 2}}
           (eval* 1 '{:a (+ 1 2 *in*)
                      :b {:a (inc *in*)}
                      :c [*in* *in*]
                      :d #{*in* (inc *in*)}}))))
  (testing "quoting"
    (is (= {:a '*in*} (eval* 1 (str "'{:a *in*}"))))
    (is (= '#{1 2 3 *in*} (eval* 4 "'#{1 2 3 *in*}")))
    (is (= '[1 2 3 *in*] (eval* 4 "'[1 2 3 *in*]")))
    (is (= '(1 2 3 *in*) (eval* 4 "'(1 2 3 *in*)"))))
  (testing "calling ifns"
    (is (= 3 (eval* nil '({:a 1} 2 3))))
    (is (= 1 (eval* nil '({:a 1} :a 3))))
    (is (= 3 (eval* nil '((hash-map :a 1) 2 3))))
    (is (= 1 (eval* nil '((hash-map :a 1) :a 3))))
    (is (= :a (eval* nil '(#{:a :b :c} :a)))))
  (testing "cannot call x as a function"
    (doseq [example ['(1 2 3) '("foo" 2 3) '(nil 1 2 3)]]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"call.*function"
                            (eval* nil example))))))

(deftest destructure-test
  (is (= 1 (eval* nil "(let [{:keys [a]} {:a 1}] a)")))
  (is (= 1 (eval* nil "(let [{:keys [:a]} {:a 1}] a)")))
  (is (= 1 (eval* nil "((fn [{:keys [a]}] a) {:a 1})")))
  (is (= 1 (eval* nil "((fn [{:keys [:a]}] a) {:a 1})")))
  (is (= 1 (eval* nil "((fn [{:person/keys [id]}] id) {:person/id 1})")))
  (is (= 1 (eval* nil "((fn [{:syms [a]}] a) '{a 1})")))
  (is (= 1 (eval* nil "((fn [{:strs [a]}] a) '{\"a\" 1})"))))

(defn test-difference
  ([var-name expr-string max-attempts]
   (test-difference var-name expr-string 0 max-attempts))
  ([var-name expr-string attempt max-attempts]
   (if (> attempt max-attempts)
     (is false (str var-name " did not give random results."))
     (let [[x y] [(eval* expr-string) (eval* expr-string)]]
       ;; (prn "X>" x "Y>" y)
       (if (not= x y)
         (is true (str var-name " did not give random results."))
         (recur var-name expr-string (inc attempt) max-attempts))))))

(deftest rand-test
  (testing "patch for oracle/graal 1610 works"
    (test-difference "rand" "(rand)" 10)
    (test-difference "rand-int" "(rand-int 10)" 10)
    (test-difference "rand-nth" "(rand-nth (range 10))" 10)
    (test-difference "random-sample" "(random-sample 0.1 (range 100))" 10)))

(deftest let-test
  (is (= [1 2] (eval* '(let [x 1 y (+ x x)] [x y]))))
  (is (= [1 2] (eval* '(let [{:keys [:x :y]} {:x 1 :y 2}] [x y]))))
  (testing "let can have multiple body expressions"
    (is (= 2 (if tu/native?
               (eval* '(let [x 2] 1 2 3 x))
               (let [a (atom 0)]
                 (tu/eval*
                  '(let [x 3] (f) (f) x) {:bindings {'f #(swap! a inc)}})
                 @a))))))

(deftest delay-test
  (when-not tu/native?
    ;; cannot test this natively due to metadata serialization in EDN
    (is (= 6 (tu/eval* '(+ 1 2 3) {:bindings {(with-meta 'x {:sci/deref! true})
                                              (delay (throw (new #?(:clj Exception :cljs js/Error)
                                                                 "o n000s")))}})))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"o n000s"
                          (tu/eval* '(+ 1 2 3 x) {:bindings {(with-meta 'x {:sci/deref! true})
                                                             (delay (throw (new #?(:clj Exception :cljs js/Error)
                                                                                "o n000s")))}})))))
(deftest fn-literal-test
  (is (= '(1 2 3)
         (eval* "(map #(do %) [1 2 3])")))
  (is (= '([0 1] [1 2] [2 3])
         (eval* "(map-indexed #(do [%1 %2]) [1 2 3])")))
  (is (= '(1 2 3)
         (eval* "(apply #(do %&) [1 2 3])"))))

(deftest fn-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"arg"
       (eval* '((fn foo [x] (if (< x 3) (foo 1 (inc x)) x)) 0))))
  (is (= 3 (eval* '((fn foo [x] (if (< x 3) (foo (inc x)) x)) 0))))
  (is (= [2 3] (eval* '((fn foo [[x & xs]] xs) [1 2 3]))))
  (is (= [2 3] (eval* '((fn foo [x & xs] xs) 1 2 3))))
  (is (= 2 (eval* '((fn foo [x & [y]] y) 1 2 3))))
  (is (= 1 (eval* '((fn ([x] x) ([x y] y)) 1))))
  (is (= 2 (eval* '((fn ([x] x) ([x y] y)) 1 2))))
  (is (= '(2 3 4) (eval* '(apply (fn [x & xs] xs) 1 2 [3 4])))))

(deftest def-test
  (is (= "nice val" (eval* '(do (def foo "nice val") foo))))
  (is (nil? (eval* '(do (def foo) foo))))
  (is (= 2 (eval* '(do (def foo) (def foo "docstring" 2) foo)))))

(deftest defn-test
  (is (= 2 (eval* '(do (defn foo "increment c" [x] (inc x)) (foo 1)))))
  (is (= 3 (eval* '(do (defn foo ([x] (inc x)) ([x y] (+ x y)))
                       (foo 1)
                       (foo 1 2)))))
  (is (= 0 (eval* '(do (defn foo [x] (inc x))
                       (defn foo "decrement c" [x] (dec x))
                       (foo 1))))))

(deftest resolve-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"x"
       (eval* "#(inc x)")))
  (testing "as->"
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error) #"y"
         (eval* "(defn foo [] (as-> y x (inc y)))")))
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error) #"y"
         (eval* "(defn foo [] (as-> 10 x (inc y)))")))))

(deftest do-test
  (testing "expressions with do are evaluated in order and have side effects,
  even when one of the following expressions have an unresolved symbol"
    (when-not tu/native?
      (is
       (str/includes?
        (with-out-str (try (tu/eval* "(do (defn foo []) (foo) (println \"hello\") (defn bar [] x))"
                                     {:bindings {'println println}})
                           (catch #?(:clj Exception :cljs js/Error) _ nil)))
        "hello")))))

(deftest macroexpand-test
  (is (= [6] (eval* "[(-> 3 inc inc inc)]")))
  (is (= [{3 6}] (eval* "[{(->> 2 inc) (-> 3 inc inc inc)}]")))
  (is (eval* (str `(#(< 10 % 18) 15))))
  (is (eval* (str `(#(and (int? %) (< 10 % 18)))) 15)))

(deftest permission-test
  (is (tu/eval* "(int? 1)" {:allow '[int?]}))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(int? 1)" {:allow '[boolean?]})))
  (is (= 3 (tu/eval* "(do (defn foo []) 3)" {})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(defn foo [])" {:allow '[fn]})))
  (if tu/native?
    (is (tu/eval* "(#(pos-int? %) 10)" {:allow '[pos-int?]}))
    (is ((tu/eval* "#(pos-int? %)" {:allow '[pos-int?]}) 10)))
  (if tu/native?
    (is (= 3 (tu/eval* "((fn [x] (if (> x 1) (inc x))) 2)" {:allow '[fn if > inc]})))
    (is (= 3 ((tu/eval* "(fn [x] (if (> x 1) (inc x)))" {:allow '[fn if > inc]}) 2))))
  (is (tu/eval* (str (list `#(inc %) 10)) {:allow '[fn* inc]}))
  (is (tu/eval* (str (list `#(let [x %] x) 10)) {:allow '[fn* let]}))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(loop [] (recur))" {:deny '[loop]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/loop [] (recur))" {:deny '[loop]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/loop [] (recur))" {:deny '[recur]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/loop [] (recur))" {:preset :termination-safe}))))

(deftest realize-max-test
  (when-not tu/native?
    (let [d (try (tu/eval* "(reduce (fn [_ _]) (range 1000))" {:realize-max 100})
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                   (ex-data e)))]
      (is (= :sci.error/realized-beyond-max (:type d)))
      (is (= "(reduce (fn [_ _]) (range 1000))" (:start-expression d)))))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"realized"
                        (tu/eval* "(reduce (fn [_ _]) (range 1000))" {:realize-max 100})))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"realized"
                        (doall (tu/eval* "(repeat 1)" {:realize-max 100}))))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"realized"
                        (doall (tu/eval* "(repeat 1)" {:preset :termination-safe}))))
  (is (= (range 10) (tu/eval* "(range 10)" {:realize-max 100}))))

(deftest idempotent-eval-test
  (is (= '(foo/f1 foo/f2)
         (eval* "(map #(let [[ns v] %] (symbol (str ns) (str v))) '[[foo f1] [foo f2]])")))
  (is (= '(foo/f1)
         (eval* "(map #(let [[ns v] %] (symbol (str ns) (str v)))
                   (vector (vector (symbol \"foo\") (symbol \"f1\"))))")))
  (is (= '[["foo"] ["bar"]] (eval* "(map (fn [x] x) (list (list \"foo\") (list \"bar\")))"))))

(deftest error-location-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"\[at line 1, column 11\]"
                        (with-out-str (eval* nil "(+ 1 2 3) (conj 1 0)"))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"\[at line 1, column 13\]"
                        (tu/eval* "(+ 1 2 3 4) (vec (range))" {:realize-max 100})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"\[at line 1, column 19\]"
                        (tu/eval* "(+ 1 2 3 4 5) (do x)" {}))))

(deftest macro-test
  (when-not tu/native?
    (is (= [1 1]
           (tu/eval*
            '(do-twice 1)
            {:bindings {'do-twice (with-meta (fn [& body]
                                               `(vector (do ~@body) (do ~@body)))
                                    {:sci/macro true})}})))))

(deftest comment-test
  (is (nil? (eval* '(comment "anything"))))
  (is (nil? (eval* '(comment anything))))
  (is (nil? (eval* '(comment 1))))
  (is (nil? (eval* '(comment (+ 1 2 (* 3 4)))))))

(deftest GH-54-recursive-function-test
  (when-not tu/native?
    (is (= 5 (tu/eval* "(do (def c (atom 0))
                            (defn hello []
                              (swap! c inc)
                              (if (< @c 5) (hello) @c))
                            (hello))"
                       {:bindings {'atom atom
                                   'swap! swap!
                                   'deref deref}})))))

(deftest trampoline-test
  (is (= 10000 (tu/eval* "(defn hello [x] (if (< x 10000) #(hello (inc x)) x))
                         (trampoline hello 0)" {}))))

(deftest recur-test
  (is (= 10000 (tu/eval* "(defn hello [x] (if (< x 10000) (recur (inc x)) x)) (hello 0)"
                         {}))))

(deftest loop-test
  (is (= 2 (tu/eval* "(loop [[x y] [1 2]] (if (= x 3) y (recur [(inc x) y])))"
                     {}))))

(deftest for-test
  (is (= '([1 4] [1 6])
         (eval* "(for [i [1 2 3] :while (< i 2) j [4 5 6] :when (even? j)] [i j])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"vector"
                        (eval* "(for 1 [i j])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"even"
                        (eval* "(for [:dude] [i j])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"keyword"
                        (eval* "(for [x [1 2 3] :dude []] [i j])"))))

(deftest doseq-test
  (when-not tu/native?
    (is (= "1\n1\n3\n9\n"
           (with-out-str
             (tu/eval* "(doseq [i [1 2 3]
                              :when (odd? i)
                              :let [j (* i i)]]
                        (println i) (println j))"
                       {:bindings {'println println}}))))))

(deftest require-test
  (is (= "1-2-3" (eval* "(str/join \"-\" [1 2 3])")))
  (is (= "1-2-3" (eval* "(require '[clojure.string :as string]) (string/join \"-\" [1 2 3])")))
  (is (= "1-2-3" (eval* "(require '[clojure.string :refer [join]]) (join \"-\" [1 2 3])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"must be a sequential"
                        (eval* "(require '[clojure.string :refer :all]) (join \"-\" [1 2 3])")))
  (is (= #{1 4 6 3 2 5} (eval* "(set/union #{1 2 3} #{4 5 6})")))
  (is (= #{1 4 6 3 2 5} (eval* "(require '[clojure.set :as s]) (s/union #{1 2 3} #{4 5 6})")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"clojure.foo"
                        (eval* "(require '[clojure.foo :as s])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"quux does not exist"
                        (eval* "(require '[clojure.set :refer [quux]])"))))

(deftest cond-test
  (is (= 2 (eval* "(let [x 2]
                     (cond (string? x) 1 (int? x) 2))")))
  (is (= 2 (eval* "(let [x 2]
                     (cond (string? x) 1 :else 2))")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"even"
                        (eval* "(let [x 2]
                                  (cond (string? x) 1 :else))"))))

(deftest regex-test
  (is (= "1" (eval* "(re-find #\"\\d\" \"aaa1aaa\")"))))

(deftest case-test
  (is (= true (eval* "(case 1, 1 true, 2 (+ 1 2 3), 6)")))
  (is (= true (eval* "(case (inc 0), 1 true, 2 (+ 1 2 3), 6)")))
  (is (= 6 (eval* "(case (inc 1), 1 true, 2 (+ 1 2 3), 6)")))
  (is (= 7 (eval* "(case (inc 2), 1 true, 2 (+ 1 2 3), 7)")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"matching clause"
       (eval* "(case (inc 2), 1 true, 2 (+ 1 2 3))"))))

(deftest variable-can-have-macro-or-var-name
  (is (= true (eval* "(defn foo [merge] merge) (foo true)")))
  (is (= true (eval* "(defn foo [merge] merge) (defn bar [foo] foo) (bar true)")))
  (is (= true (eval* "(defn foo [comment] comment) (foo true)"))))

(deftest try-test
  (when-not tu/native?
    (let [state (atom nil)]
      (is (zero? (tu/eval* #?(:clj "(try (mapv 1 [1 2 3])
                                       (catch Exception _e 0)
                                       (finally (reset! state :finally)))"
                              :cljs "(try (mapv 1 [1 2 3])
                                       (catch js/Error _e 0)
                                       (finally (reset! state :finally)))")
                           {:bindings #?(:clj {'state state
                                               'reset! reset!
                                               'Exception Exception}
                                         :cljs {'state state
                                                'reset! reset!
                                                'js/Error js/Error})})))
      (is (= :finally @state)))))

;;;; Scratch

(comment
  (eval* 1 '(inc *in*))
  (test-difference "foo" "[10 10]" 0 10)
  (test-difference "rand" #(rand) 0 10)
  )
