(ns sci.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci :refer [eval-string]]
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
  (testing "do can return nil"
    (is (= [nil] (eval* "[(do 1 2 nil)]"))))
  (testing "if and when"
    (is (= 1 (eval* 0 '(if (zero? *in*) 1 2))))
    (is (= 2 (eval* 1 '(if (zero? *in*) 1 2))))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Too few arguments to if" 
                          (eval* '(if))))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Too few arguments to if" 
                          (eval* '(if 1))))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Too many arguments to if" 
                          (eval* '(if 1 2 3 4))))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Too many arguments to if" 
                          (eval* '(if 1 2 3 4 5))))
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
  (testing "some->"
    (is (= nil   (eval* '(some-> {:a {:a nil}}        :a :a :a (clojure.string/lower-case)))))
    (is (= "aaa" (eval* '(some-> {:a {:a {:a "AAA"}}} :a :a :a (clojure.string/lower-case))))))
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
    (is (= 6 (tu/eval* '(+ 1 2 3) {:bindings {(with-meta 'x {:sci.impl/deref! true})
                                              (delay (throw (new #?(:clj Exception :cljs js/Error)
                                                                 "o n000s")))}})))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"o n000s"
                          (tu/eval* '(+ 1 2 3 x) {:bindings {(with-meta 'x {:sci.impl/deref! true})
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
  (is (= '(2 3 4) (eval* '(apply (fn [x & xs] xs) 1 2 [3 4]))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Can't have fixed arity function with more params than variadic function \[at line 1, column 4\]"
                        (eval* "   (fn ([& args]) ([v ]))")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Can't have more than 1 variadic overload \[at line 1, column 4\]"
                        (eval* "   (fn ([& args]) ([v & args]))"))))

(deftest pre-post-conditions-test
  (is (thrown-with-msg? #?(:clj Throwable :cljs js/Error)
                        #"Assert failed: \(pos\? x\)"
                        (eval* "(def f (fn ([x] {:pre [(pos? x)]} x) ([x y] (+ x y)))) (f -1)")))
  (is (thrown-with-msg? #?(:clj Throwable :cljs js/Error)
                        #"Assert failed: \(< % 10\)"
                        (eval* "(def f (fn ([x] {:pre [(pos? x)]} x)
                                           ([x y] {:post [(< % 10)]} (+ x y)))) (f 5 10)"))))

(deftest def-test
  (is (= "nice val" (eval* '(do (def foo "nice val") foo))))
  (is (-> (eval* "(str (do (def foo) foo))")
          (str/lower-case)
          (str/includes? "unbound")))
  (is (= 2 (eval* '(do (def foo) (def foo "docstring" 2) foo))))
  (is (= 1 (eval* "(try (def x 1) x)")))
  (is (= 1 (eval* "(try (defn x [] 1) (x))")))
  (is (= 1 (eval* "(try (let [] (def x 1) x))"))))

(deftest defn-test
  (is (= 2 (eval* '(do (defn foo "increment c" [x] (inc x)) (foo 1)))))
  (is (= 3 (eval* '(do (defn foo ([x] (inc x)) ([x y] (+ x y)))
                       (foo 1)
                       (foo 1 2)))))
  (is (= 0 (eval* '(do (defn foo [x] (inc x))
                       (defn foo "decrement c" [x] (dec x))
                       (foo 1)))))
  (is (= 1337 (eval* '(do (defn foo "decrement c" {:cool-meta (inc 1336)}
                            [x] (dec x))
                          (:cool-meta (meta #'foo))))))
  (is (= 1337 (eval* '(do (defn foo {:cool-meta (inc 1336)}
                            [x] (dec x))
                          (:cool-meta (meta #'foo)))))))

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
         (eval* "(defn foo [] (as-> 10 x (inc y)))"))))
  (is (= 1 (eval* "((symbol \"do\") {'do 1})")))
  (is (= 1 (eval* "(let [x 'do] (x {'do 1}))")))
  (is (= 1 (eval* "(let [case 'case] (case {'case 1}))")))
  ;; in call position Clojure prioritizes special symbols over bindings
  (is (= '{do 1} (eval* "(let [do 'do] (do {'do 1}))")))
  (is (= 1 (eval* "((symbol \"recur\") {'recur 1})"))))

(deftest top-level-test
  (testing "top level expressions are evaluated in order and have side effects,
  even when one of the following expressions have an unresolved symbol"
    (when-not tu/native?
      (is
       (str/includes?
        (with-out-str (try (tu/eval* "(defn foo []) (foo) (println \"hello\") (defn bar [] x)"
                                     {:bindings {'println println}})
                           (catch #?(:clj Exception :cljs js/Error) _ nil)))
        "hello"))))
  (testing "nil as last expression returns nil as a whole"
    (is (nil? (eval* "1 2 nil")))))

(deftest macroexpand-test
  (is (= [6] (eval* "[(-> 3 inc inc inc)]")))
  (is (= [{3 6}] (eval* "[{(->> 2 inc) (-> 3 inc inc inc)}]")))
  (is (eval* (str `(#(< 10 % 18) 15))))
  (is (eval* (str `(#(and (int? %) (< 10 % 18)))) 15)))

(deftest permission-test
  (is (tu/eval* "(int? 1)" {:allow '[int?]}))
  (is (tu/eval* "(int? 1)" {:deny '[double?]}))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(int? 1)" {:allow '[boolean?]})))
  (is (= 3 (tu/eval* "(do (defn foo []) 3)" {:allow nil :deny []})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(defn foo [])" {:allow '[fn*]})))
  (if tu/native?
    (do (is (tu/eval* "(#(pos-int? %) 10)" {:allow '[fn* pos-int?]}))
        (is (tu/eval* "(#(clojure.core/pos-int? %) 10)" {:allow '[fn* pos-int?]}))
        (is (tu/eval* "(#(pos-int? %) 10)" {:allow '[fn* clojure.core/pos-int?]}))
        (is (tu/eval* "(#(clojure.core/pos-int? %) 10)" {:allow '[fn* clojure.core/pos-int?]})))
    (do (is ((tu/eval* "#(pos-int? %)" {:allow '[fn* pos-int?]}) 10))
        (is ((tu/eval* "#(clojure.core/pos-int? %)" {:allow '[fn* pos-int?]}) 10))
        (is ((tu/eval* "#(pos-int? %)" {:allow '[fn* clojure.core/pos-int?]}) 10))
        (is ((tu/eval* "#(clojure.core/pos-int? %)" {:allow '[fn* clojure.core/pos-int?]}) 10))))
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
                        (tu/eval* "(clojure.core/inc 1)" {:deny '[clojure.core/inc]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/loop [] (recur))" {:preset :termination-safe})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(eval '(loop [] (recur)))" {:preset :termination-safe})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(resolve 'trampoline)" {:preset :termination-safe})))

  (testing "for/doseq/dotimes use loop in a safe manner, so `{:deny '[loop recur]}` should not forbid it, see #141"
    (is '(1 2 3) (tu/eval* "(for [i [1 2 3] j [4 5 6]] [i j])" {:deny '[loop recur]}))
    (is (nil? (tu/eval* "(doseq [i [1 2 3]] i)" {:deny '[loop recur]})))
    (is (nil? (tu/eval* "(dotimes [i 3] i)" {:deny '[loop recur]})))
    (testing "users should not be able to hack around this by messing with metadata"
      (is (int? (:line (eval* "(def x (with-meta (symbol \"y\") {:line :allow})) (meta x)"))))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"allowed"
                            (tu/eval* "(def allowed-loop (with-meta (symbol \"loop\") {:line :allow}))
                                       (defmacro foo [] `(~allowed-loop [])) (foo)" {:deny '[loop recur]})))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"allowed"
                            (tu/eval* "(let [allowed-loop (with-meta (symbol \"loop\") {:line :allow})]
                                         (defmacro foo [] `(~allowed-loop [])))
                                       (foo)" {:deny '[loop recur]}))))
    (testing "but it should be forbidden in macros that are defined by a user"
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"allowed"
                            (tu/eval* "(defmacro foo [] `(loop [])) (foo)" {:deny '[loop recur]})))))
  (testing "users cannot hack around sci.impl/needs-ctx"
    (is (= [1 2] (eval* "(def f (with-meta (fn [ctx x] [ctx x]) {:sci.impl/needs-ctx true})) (f 1 2)")))))

(deftest realize-max-test
  (when-not tu/native?
    (let [d (try (tu/eval* "(reduce (fn [_ _]) (range 1000))" {:realize-max 100})
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                   (ex-data e)))]
      (is (= :sci.error/realized-beyond-max (:type d)))))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"realized"
                        (tu/eval* "(reduce (fn [_ _]) (range 1000))" {:realize-max 100})))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"realized"
                        (doall (tu/eval* "(repeat 1)" {:realize-max 100}))))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"realized"
                        (doall (tu/eval* "(repeat 1)" {:preset :termination-safe}))))
  (is (= (range 10) (tu/eval* "(range 10)" {:realize-max 100})))
  (is (= (map #(* % %) (range 99)) (tu/eval* "(map #(* % %) (range 99))" {:realize-max 100}))))

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
                        (eval* "(+ 1 2 3 4 5) (do x)")))
  (when-not tu/native?
    (testing "ex-data"
      (tu/assert-submap {:type :sci/error, :line 1, :column 15,
                         :message #"Cannot call foo with 1 arguments \[at line 1, column 15\]"}
                        (try (eval* "(defn foo []) (foo 1)")
                             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                               (let [d (ex-data ex)]
                                 d))))
      (tu/assert-submap {:type :sci/error, :line 1, :column 21,
                         :message #"Cannot call foo with 0 arguments"}
                        (try (eval* "(defn foo [x & xs]) (foo)")
                             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                               (let [d (ex-data ex)]
                                 d))))
      (tu/assert-submap {:type :sci/error, :line 1, :column 93,
                         :message #"Cannot call bindings"}
                        (try (eval* (str "(defmacro bindings [a] (zipmap (mapv #(list 'quote %) (keys &env)) (keys &env))) "
                                         "(let [x 1] (bindings))"))
                             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                               (let [d (ex-data ex)]
                                 d))))
      (tu/assert-submap {:type :sci/error, :line 1, :column 25,
                         :message #"Cannot call foo"}
                        (try (eval* (str "(defmacro foo [x & xs]) "
                                         "(foo)"))
                             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                               (let [d (ex-data ex)]
                                 d)))))))

(deftest macro-test
  (when-not tu/native?
    (is (= [1 1]
           (tu/eval*
            '(do-twice 1)
            {:bindings {'do-twice (with-meta (fn [_&form _&env & body]
                                               `(vector (do ~@body) (do ~@body)))
                                    {:sci/macro true})}}))))
  (testing "defn doesn't add binding to &env"
    (is (= '[x y] (eval* "
(defmacro lets []
  (let [res (mapv (fn [sym]
                           (list 'quote sym)) (keys &env))]
    res))

(defn foo [x] (let [y 1]
  (lets)))

(foo 10)"))))
  (testing "def with anon named fn adds binding to &env"
    (is (= '[foo x y] (eval* "
(defmacro lets []
  (let [res (mapv (fn [sym]
                           (list 'quote sym)) (keys &env))]
    res))

(def foo (fn foo [x] (let [y 1]
  (lets))))

(foo 10)"))))
  (is (= '(foo 1 2 3) (eval* "(defmacro foo [x y z] (list 'quote &form)) (foo 1 2 3)"))))

(deftest comment-test
  (is (nil? (eval* '(comment "anything"))))
  (is (nil? (eval* '(comment anything))))
  (is (nil? (eval* '(comment 1))))
  (is (nil? (eval* '(comment (+ 1 2 (* 3 4)))))))

(deftest GH-54-recursive-function-test
  (when-not tu/native?
    (is (= 5 (tu/eval* "(def c (atom 0))
                        (defn hello []
                        (swap! c inc)
                        (if (< @c 5) (hello) @c))
                        (hello)"
                       {:bindings {'atom atom
                                   'swap! swap!
                                   'deref deref}})))))

(deftest trampoline-test
  (is (= 10000 (tu/eval* "(defn hello [x] (if (< x 10000) #(hello (inc x)) x))
                         (trampoline hello 0)" {}))))

(deftest recur-test
  (is (= 10000 (tu/eval* "(defn hello [x] (if (< x 10000) (recur (inc x)) x)) (hello 0)"
                         {})))
  (testing "function with recur may be returned"
    (when-not tu/native?
      (let [f (eval* "(fn f [x] (if (< x 3) (recur (inc x)) x))")]
        (f 0)))))

(deftest loop-test
  (is (= 2 (tu/eval* "(loop [[x y] [1 2]] (if (= x 3) y (recur [(inc x) y])))"
                     {}))))

(deftest for-test
  (is (= '([1 4] [1 6])
         (eval* "(for [i [1 2 3] :while (< i 2) j [4 5 6] :when (even? j)] [i j])")))
  (is (= (for [[_ counts] [[1 [1 2 3]] [3 [1 2 3]]] c counts] c)
         (eval* "(for [[_ counts] [[1 [1 2 3]] [3 [1 2 3]]] c counts] c)")))
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
  (is (= "1-2-3" (eval* "(require '[clojure.string :refer :all]) (join \"-\" [1 2 3])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"must be a sequential"
                        (eval* "(require '[clojure.string :refer 1]) (join \"-\" [1 2 3])")))
  (is (= #{1 4 6 3 2 5} (eval* "(set/union #{1 2 3} #{4 5 6})")))
  (is (= #{1 4 6 3 2 5} (eval* "(require '[clojure.set :as s]) (s/union #{1 2 3} #{4 5 6})")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"clojure.foo"
                        (eval* "(require '[clojure.foo :as s])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"quux does not exist"
                        (eval* "(require '[clojure.set :refer [quux]])")))
  (is (= [1 2 3] (eval* "(ns foo) (def x 1) (ns bar) (def x 2) (in-ns 'baz) (def x 3) (require 'foo 'bar) [foo/x bar/x x]")))
  (testing
      "Require evaluates arguments"
    (is (= [1 2 3] (eval* "
(ns foo)
(def x 1)

(ns bar)
(def x 2)

(in-ns 'baz)
(def x 3)
(require (symbol \"foo\") (symbol \"bar\"))
[foo/x bar/x x]"))))
  (testing "require as function"
    (is (= 1 (eval* "(ns foo) (defn foo [] 1) (ns bar) (apply require ['[foo :as f]]) (f/foo)"))))
  (testing "rename"
    (is (= #{1 2} (eval* "(require '[clojure.set :refer [union] :rename {union union2}]) (union2 #{1} #{2})")))))

(deftest use-test
  (is (= #{1 2} (eval* "(ns foo (:use clojure.set)) (union #{1} #{2})")))
  (is (= #{1 2} (eval* "(use 'clojure.set) (union #{1} #{2})")))
  (is (= #{1 2} (eval* "(use '[clojure.set :only [union]]) (union #{1} #{2})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"not.*resolve.*union"
       (eval* "(use '[clojure.set :exclude [union]]) (union #{1} #{2})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"not.*resolve.*union"
       (eval* "(use '[clojure.set :only [difference]]) (union #{1} #{2})"))))

(deftest misc-namespace-test
  (is (= 1 (eval* "(alias (symbol \"c\") (symbol \"clojure.core\")) (c/and true 1)")))
  (is (= #{1 3 2} (eval* "(mapv alias ['set1 'set2] ['clojure.set 'clojure.set]) (set2/difference
(set1/union #{1 2 3} #{4 5 6}) #{4 5 6})")))
  (is (= 'clojure.set (eval* "(ns-name (find-ns 'clojure.set))")))
  (is (= 'clojure.set (eval* "(ns-name (the-ns (the-ns 'clojure.set)))")))
  (is (= 'clojure.core (eval* "(alias 'c 'clojure.core) (ns-name (get (ns-aliases *ns*) 'c))")))
  (is (str/includes? (eval* "(defn foo []) (str (ns-publics *ns*))")
                     "foo #'user/foo"))
  (is (contains? (set (eval* "(clojure.repl/dir-fn 'clojure.string)"))
                 'last-index-of)))

(deftest cond-test
  (is (= 2 (eval* "(let [x 2]
                     (cond (string? x) 1 (int? x) 2))")))
  (is (= 2 (eval* "(let [x 2]
                     (cond (string? x) 1 :else 2))")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"even"
                        (eval* "(let [x 2]
                                  (cond (string? x) 1 :else))"))))

(deftest condp-test
  (is (= "one" (eval* "(condp = 1 1 \"one\")")))
  (is (= 3 (eval* "
(condp some [1 2 3 4]
  #{0 6 7} :>> inc
  #{4 5 9} :>> dec
  #{1 2 3} :>> #(+ % 3))"))))

(deftest regex-test
  (is (= "1" (eval* "(re-find #\"\\d\" \"aaa1aaa\")"))))

(deftest case-test
  (is (= true (eval* "(case 1, 1 true, 2 (+ 1 2 3), 6)")))
  (is (= true (eval* "(case (inc 0), 1 true, 2 (+ 1 2 3), 6)")))
  (is (= 6 (eval* "(case (inc 1), 1 true, 2 (+ 1 2 3), 6)")))
  (is (= 7 (eval* "(case (inc 2), 1 true, 2 (+ 1 2 3), 7)")))
  (is (= 6 (eval* "(case (inc 2), 1 true, (2 3) (+ 1 2 3), 7)")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"(?i)duplicate case test constant"
       (eval* "(case (inc 2), 1 true, 1 false)")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"matching clause"
       (eval* "(case (inc 2), 1 true, 2 (+ 1 2 3))"))))

(deftest variable-can-have-macro-or-var-name
  (is (= true (eval* "(defn foo [merge] merge) (foo true)")))
  (is (= true (eval* "(defn foo [merge] merge) (defn bar [foo] foo) (bar true)")))
  (is (= true (eval* "(defn foo [comment] comment) (foo true)")))
  (is (= 2 (eval* "(defn foo [fn] (fn 1)) (foo inc)"))))

(deftest throw-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"foo"
                        #?(:clj (eval* "(throw (Exception. \"foo\"))")
                           :cljs (eval* "(throw (js/Error. \"foo\"))")))))

(deftest try-catch-finally-throw-test
  (when-not tu/native?
    (let [state (atom nil)]
      (is (zero? (tu/eval* #?(:clj "(try (mapv 1 [1 2 3])
                                       (catch Exception _e 0)
                                       (finally (reset! state :finally)))"
                              :cljs "(try (mapv 1 [1 2 3])
                                     (catch js/Error _e 0)
                                     (finally (reset! state :finally)))")
                           {:bindings {'state state
                                       'reset! reset!}})))
      (is (= :finally @state))))
  #?@(:clj
      [(is (nil? (eval* "(try (mapv 1 [1 2 3]) (catch Exception e nil))")))
       (when-not tu/native?
         (tu/assert-submap {:type :sci/error, :line 1, :column 4}
                           (try (eval* "   (/ 1 0)")
                                (catch Exception e (ex-data e)))))]
      :cljs
      [(is (= :foo (eval* "(try (mapv 1 [1 2 3]) (catch js/Error e :foo))")))
       (when-not tu/native?
         (tu/assert-submap {:type :sci/error, :line 1, :column 6, :a 1}
                           (eval* "(try (throw (ex-info \"\" {:a 1})) (catch js/Error e (ex-data e)))")))])
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Foo"
                        (eval* "(try 1 (catch Foo e e))")))
  (testing "try block can have multiple expressions"
    (is (= 3 (eval* "(try 1 2 3)"))))
  (testing "babashka GH-117"
    (is (= 'hello (eval* "(try 'hello)"))))
  (testing "babashka GH-220, try should accept nil in body"
    (is (nil? (eval* "(try 1 2 nil)")))
    (is (= 1 (eval* "(try 1 2 nil 1)")))))

(deftest syntax-quote-test
  (is (= '(clojure.core/list 10 10)
         (eval* "(let [x 10] `(list ~x ~x))")))
  (let [generated (str (eval* "(let [x 1] `(let [x# ~x] x#))"))]
    (is (not (str/includes? generated "x#")))
    (is (= 2 (count (re-seq #"__auto__" generated)))))
  (is (= 1 (eval* "`~(let [x 1] x)")))
  (is (= [1 2 3]
         (eval* "`[~@(for [x [1 2 3]] x)]")))
  (is (= 1
         (eval* "
(def x 1)
(defmacro foo [] `x)
(foo)
(ns bar)
(user/foo)")))
  (is (= 'user/x (eval* "`x")))
  (is (= '(try user/x (finally user/x)) (eval* "`(try x (finally x))")))
  (is (= {:a 1} (eval* "`{:a 1}")))
  (is (= '(quote user/x) (eval* "``x")))
  (is (= :smile (eval* "
(defn caller
  [a-map]
  ((get a-map :a-fn)))

(defmacro hash-test
  []
  `(let [a-fn# (fn [] :smile)]
     (caller {:a-fn a-fn#})))

(hash-test)
")))
  (is (= 2 (count (re-seq #"__auto__" (tu/eval* "(str `(let [x# 1] `~x#))" nil)))))
  #?(:clj (when-not tu/native?
            (is (= "foo" (tu/eval* "
(defmacro pat [s] `(java.util.regex.Pattern/compile ~s))
(def p (pat \"foo\")) (re-find p \"foo\")"
                                   {:classes {'java.util.regex.Pattern java.util.regex.Pattern}})))))
  #?(:clj (is (= 'java.lang.Exception (eval* "`Exception")))))

(deftest defmacro-test
  (is (= [":hello:hello" ":hello:hello"]
         (eval* "(defmacro foo [x] (let [y (str x x)] `[~y ~y])) (foo :hello)")))
  (comment(is (= ["hellohello" "hellohello"]
                 (eval* "(defmacro foo [x] (let [y (str x x)] `[~y ~y])) (foo hello)")))
          (is (= '(1 2 3)
                 (eval* "(defmacro foo [] `(list ~@[1 2 3])) (foo)")))
          (is (= '(bar)
                 (eval* "(defmacro foo [x] `(list (quote ~x))) (foo bar)")))
          (is (= 1 (eval* "(defmacro foo [x] `(let [x# ~x] x#)) (foo 1)")))
          (is (= "bar" (eval* "(defmacro foo [x] (str x)) (foo bar)")))
          (is (= 1 (eval* "(defmacro nested [x] `(let [x# 1337] ~`(let [x# ~x] x#))) (nested 1)")))
          (when-not tu/native?
            (is (= ":dude\n:dude\n"
                   (let [out (sci/with-out-str
                               (eval-string "(defmacro foo [x] (list 'do x x)) (foo (prn :dude))"))]
                     out))))))

(deftest declare-test
  (is (= [1 2] (eval* "(declare foo bar) (defn f [] [foo bar]) (def foo 1) (def bar 2) (f)")))
  (is (= 1 (eval* "(def x 1) (declare x) x")))
  (is (str/includes? (str/lower-case (eval* "(declare x) (str x)")) "unbound")))

(deftest reader-conditionals
  (is (= 6 (tu/eval* "(+ 1 2 #?(:bb 3 :clj 100))" {:features #{:bb}})))
  (is (= 103 (tu/eval* "(+ 1 2 #?(:bb 3 :clj 100))" {:features #{:clj}}))))

(deftest add-to-clojure-core-test
  (is (= 10 (tu/eval* "dude" {:namespaces '{clojure.core {dude 10}}}))))

(deftest try-catch-test
  (is (zero? (tu/eval* "(try #?(:clj (/ 1 0)
                                :cljs (1 1))
                          (catch #?(:clj ArithmeticException :cljs js/Error) _ 0))"
                       {:read-cond :allow
                        :features #?(:clj #{:clj}
                                     :cljs #{:cljs})})))
  (is (= 4 (eval* "(def x 1)
                   (try (pos? x)
                     (def y (+ 1 2 x))
                     y
                     (finally (pos? y)))"))))

(deftest recursion-test
  (testing "stack usage didn't get worse"
    (is (= 72
           (eval* "((fn foo [x] (if (= 72 x) x (foo (inc x)))) 0)")))))

(deftest syntax-errors
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"simple symbol.*at.*1"
                        (eval* "(def f/b 1)")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"simple symbol.*at.*1"
                        (eval* "(defn f/b [])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"missing.*at.*1"
                        (eval* "(defn foo)")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"missing.*at.*1"
                        (eval* "(defn foo ())"))))

(deftest ex-message-test
  (is (= "foo" #?(:clj (eval* "(ex-message (Exception. \"foo\"))")
                  :cljs (eval* "(ex-message (js/Error. \"foo\"))")))))

(deftest assert-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"should-be-true.*\[at line 1"
                        (eval* "(def should-be-true false) (assert should-be-true)"))))

(deftest dotimes-test
  (when-not tu/native?
    (let [state (atom 0)]
      (tu/eval* "(dotimes [i 10] (swap! state inc))" {:bindings {'state state}})
      (is (= 10 @state)))))

(deftest clojure-walk-test
  (is (= {"a" {"b" 1}} (eval* "(clojure.walk/stringify-keys {:a {:b 1}})"))))

(deftest letfn-test
  (is (= 2 (eval* "(letfn [(f ([x] (f x 1)) ([x y] (+ x y)))] (f 1))")))
  (is (= 3 (eval* "(letfn [(f ([x] (f x 1)) ([x y] (+ x y)))] (f 1 2))")))
  (is (= 11 (eval* "(letfn [(f [x] (g x)) (g [x] (inc x))] (f 10))")))
  (is (nil? (eval* "(letfn [(f [x] (g x)) (g [x] (inc x))])"))))

(deftest core-delay-test
  (is (= 1 (eval* "@(delay 1)"))))

(deftest defn--test
  (is (= 1 (eval* "(defn- foo [] 1) (foo)")))
  (is (true? (eval* "(defn- foo [] 1) (:private (meta #'foo))")))
  (is (= 1 (eval* "(defn get [url & [req]] url) (get 1)"))))

(deftest refer-clojure-exclude
  (is (thrown? #?(:clj Exception :cljs js/Error) (eval* "(ns foo (:refer-clojure :exclude [get])) (some? get)")))
  (is (true? (eval* "(ns foo (:refer-clojure :exclude [get])) (defn get []) (some? get)"))))

(deftest core-resolve-test
  (is (= 1 (eval* "((resolve 'clojure.core/inc) 0)")))
  (is (= 1 (eval* "((resolve 'inc) 0)")))
  (is (= true (eval* "(ns foo (:refer-clojure :exclude [inc])) (nil? (resolve 'inc))"))))

(deftest compatibility-test
  (is (true? (eval* "(def foo foo) (var? #'foo)")))
  (is (= 1 (eval*  "((resolve 'clojure.core/inc) 0)")))
  (is (= 1 (eval* "((resolve 'inc) 0)")))
  (is (true? (eval* "(ns foo (:refer-clojure :exclude [inc])) (nil? (resolve 'inc))"))))

(deftest defonce-test
  (is (= 1 (eval* "(defonce x 1) (defonce x 2) x"))))

(deftest metadata-on-var-test
  (is (= 'x (eval* "(def x) (:name (meta #'x))")))
  (is (= 'foo (eval* "(ns foo) (declare x) (def x 2) (ns-name (:ns (meta #'x)))"))))

(deftest eval-colls-once
  ;; #222: note: this only failed with clojure 1.10.1!
  (is (= [{}] (eval* "(defn foo [x] (for [x (sort-by identity x)] x)) (foo [{}])"))))

(deftest macroexpand-1-test
  (is (= [1 1] (eval* "(defmacro foo [x] `[~x ~x]) (macroexpand-1 '(foo 1))")))
  (is (= '(if 1 1 (clojure.core/cond)) (eval* "(macroexpand-1 '(cond 1 1))")))
  (is (= #?(:clj 'clojure.core/let
            :cljs 'cljs.core/let)
         (first (eval* "(macroexpand-1 '(for [x [1 2 3]] x))"))))
  (is (= '(user/bar 1) (eval* "(defmacro foo [x] `(bar ~x)) (defmacro bar [x] x) (macroexpand-1 '(foo 1))"))))

(deftest macroexpand-call-test
  (is (= [1 1] (eval* "(defmacro foo [x] `(bar ~x)) (defmacro bar [x] [x x]) (macroexpand '(foo 1))")))
  (is (= '(. (. System (getProperties)) (get  "os.name"))
         (eval* "(macroexpand '(.. System (getProperties) (get \"os.name\")))")))
  (is (= '[1 2 user/x] (eval* "(defmacro foo [x] `[1 2 x]) (macroexpand '(foo 1))"))))

(deftest load-fn-test
  (when-not tu/native?
    (is (= 1 (tu/eval* "
(let [ns 'foo]
  (require ns))
(foo/foo-fn)" {:load-fn (constantly
                         {:file "foo.clj"
                          :source "(ns foo) (defn foo-fn [] 1)"})})))))

(deftest reload-test
  (when-not tu/native?
    (is (= "hello\nhello\nhello\n"
           (sci/with-out-str
             (tu/eval* "
(require '[foo])
(require '[foo] :reload)
(require 'foo :reload)
1"
                       {:load-fn (fn [{:keys [:namespace]}]
                                   (case namespace
                                     'foo {:file "foo.clj"
                                           :source "(ns foo) (println \"hello\")"}))}))))
    (is (= "hello\nhello\n"
           (sci/with-out-str
             (tu/eval* "
(require '[foo])
(require '[foo] :reload)
(require 'foo)
1"
                       {:load-fn (fn [{:keys [:namespace]}]
                                   (case namespace
                                     'foo {:file "foo.clj"
                                           :source "(ns foo) (println \"hello\")"}))}))))))

(deftest alter-meta!-test
  (is (true? (eval* "(doto (def x) (alter-meta! assoc :private true)) (:private (meta #'x))")))
  (is (true? (eval* "(doto (def x) (reset-meta! {:private true})) (:private (meta #'x))"))))

(deftest could-not-resolve-symbol
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"resolve.*def"
                        (eval* "def"))))

(deftest function-results-dont-have-metadata
  (is (nil? (eval* "(meta (fn []))")))
  (is (nil? (eval* "(meta (fn ([]) ([_])))"))))

(deftest fn-on-meta-test
  (is (= "foo" (eval* "(def ^{:test (fn [] \"foo\")} x) ((:test (meta #'x)))"))))

(deftest readers-test
  (when-not tu/native?
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"No reader function" (tu/eval* "#x/str 5" {})))
    (is (string? (tu/eval* "#x/str 5" {:readers {'x/str str}})))))

(deftest built-in-vars-are-read-only-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"read-only"
       (tu/eval*  "(alter-var-root #'clojure.core/inc (constantly dec)) (inc 2)" {}))))

(deftest repl-doc-test
  (when-not tu/native?
    (is (= (str/trim "
-------------------------
user/f
([x] [x y])
Macro
  foodoc")
           (str/trim (sci/with-out-str (eval* "(defmacro f \"foodoc\" ([x]) ([x y])) (clojure.repl/doc f)")))))
    (is (= (str/trim  #?(:clj "
-------------------------
clojure.core/inc
([x])
  Returns a number one greater than num. Does not auto-promote
  longs, will throw on overflow. See also: inc'"
                         :cljs "
-------------------------
clojure.core/inc
([x])
  Returns a number one greater than num."))
           (str/trim (sci/with-out-str (eval* "(clojure.repl/doc inc)")))))
    (is (= (str/trim
            "-------------------------\nfoo\n  foodoc\n")
           (str/trim (sci/with-out-str (eval* "(ns foo \"foodoc\") (clojure.repl/doc foo)")))))))

(deftest repl-find-doc-test
  (when-not tu/native?
    (is (= (str/trim "
-------------------------
foo-ns/foo-fun
([x] [x y])
Macro
  foodoc
-------------------------
foo-ns/foo-macro
([x] [x y])
Macro
  foodoc
-------------------------
foo-ns
  foodoc")
           (str/trim (sci/with-out-str (eval* "
(ns foo-ns \"foodoc\")
(defmacro foo-fun \"foodoc\" ([x]) ([x y]))
(defmacro foo-macro \"foodoc\" ([x]) ([x y]))

(clojure.repl/find-doc #\"foodoc\")")))))))

(deftest tagged-literal-test
  (testing "EDN with custom reader tags can be read without exception"
    (is (= 1 (eval* "(require '[clojure.edn]) (clojure.edn/read-string {:default tagged-literal} \"#foo{:a 1}\") 1")))))

(deftest ifs-test
  (is (= 2 (eval* "(if-let [foo nil] 1 2)")))
  (is (= 2 (eval* "(if-let [foo false] 1 2)")))
  (is (= 2 (eval* "(if-some [foo nil] 1 2)")))
  (is (= 1 (eval* "(if-some [foo false] 1 2)"))))

(deftest whens-test
  (is (= nil (eval* "(when-let [foo nil] 1)")))
  (is (= nil (eval* "(when-let [foo false] 1)")))
  (is (= nil (eval* "(when-some [foo nil] 1)")))
  (is (= 1 (eval* "(when-some [foo false] 1)"))))

(deftest read-string-eval-test
  (is (= :foo (eval* "(def f (eval (read-string \"(with-meta (fn [ctx] :foo) {:sci.impl/op :needs-ctx})\"))) (f 1)")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"loop.*allowed"
                        (tu/eval* "(eval (read-string \"(loop [] (recur))\"))" {:deny '[loop]}))))

(deftest while-test
  (is (= 10 (eval* "(def a (atom 0)) (while (< @a 10) (swap! a inc)) @a"))))

(deftest meta-on-syntax-quote-test
  (is (:foo (eval* "(meta `^:foo (1 2 3))"))))

;;;; Scratch

(comment
  (eval* 1 '(inc *in*))
  (test-difference "foo" "[10 10]" 0 10)
  (test-difference "rand" #(rand) 0 10)
  )
