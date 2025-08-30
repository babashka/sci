(ns sci.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   #?(:clj [sci.ctx-store :as store])
   [sci.copy-ns-test-ns]
   [sci.core :as sci :refer [eval-string]]
   [sci.impl.unrestrict :as unrestrict]
   [sci.test-utils :as tu]))

#?(:cljs (def Exception js/Error))

#?(:cljs
   (defn testing-vars-str
     "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
     [m]
     (let [{:keys [file line column]} m]
       (str
        (reverse (map #(:name (meta %)) (:testing-vars (test/get-current-env))))
        " (" file ":" line (when column (str ":" column)) ")"))))

#?(:clj
   (defmethod clojure.test/report :begin-test-var [m]
     (println "===" (-> m :var meta :name))
     (println))
   :cljs (defmethod cljs.test/report [:cljs.test/default :begin-test-var] [m]
           (println "===" (-> m testing-vars-str))
           (println)))

#?(:clj
   (defmethod clojure.test/report :end-test-var [_m]
     (let [{:keys [:fail :error]} @test/*report-counters*]
       (when (and (= "true" (System/getenv "SCI_FAIL_FAST"))
                  (or (pos? fail) (pos? error)))
         (println "=== Failing fast")
         (System/exit 1)))))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}
                   :classes #?(:clj {'java.lang.IllegalArgumentException java.lang.IllegalArgumentException}
                               :cljs {})})))

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
    (is (= 10 (eval* "(if true 10 20)")))
    (is (= 20 (eval* "(if false 10 20)")))
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
    (is (= 3 (eval* false '(or false false *in* 3))))
    (is (true? (eval* false '(or nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil true)))))
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
  (testing "as->"
    (is (= "4444444444"
           (eval* '(as-> 1 x (inc x) (inc x) (inc x) (apply str (repeat 10 (str x))))))))
  (testing "some->"
    (is (= nil (eval* '(some-> {:a {:a nil}} :a :a :a (clojure.string/lower-case)))))
    (is (= "aaa" (eval* '(some-> {:a {:a {:a "AAA"}}} :a :a :a (clojure.string/lower-case))))))
  (testing "literals"
    (is (= {:a 4
            :b {:a 2}
            :c [1 1]
            :d #{1 2}
            :e {:a 1}}
           (eval* 1 '{:a (+ 1 2 *in*)
                      :b {:a (inc *in*)}
                      :c [*in* *in*]
                      :d #{*in* (inc *in*)}
                      :e {:a *in*}}))))
  (testing "duplicate keys"
    (is (thrown-with-msg? Exception #"Duplicate key" (sci/eval-string "(let [a 1 b 1] #{a b})")))
    (is (thrown-with-msg? Exception #"Duplicate key" (sci/eval-string "(let [a 1 b 1] {a 1 b 2})"))))
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
  (testing "EvalFn as function"
    (is (= 1 (eval* nil '((get {:foo identity} :foo) 1))))))

(deftest destructure-test
  (is (= 1 (eval* nil "(let [{:keys [a]} {:a 1}] a)")))
  (is (= 1 (eval* nil "(let [{:keys [:a]} {:a 1}] a)")))
  (is (= 42 (eval* nil "(let [k 'foo, a-map {k 42}, {foo-val k} a-map] foo-val)")))
  (is (= 1 (eval* nil "((fn [{:keys [a]}] a) {:a 1})")))
  (is (= 1 (eval* nil "((fn [{:keys [:a]}] a) {:a 1})")))
  (is (= 1 (eval* nil "((fn [{:person/keys [id]}] id) {:person/id 1})")))
  (is (= 1 (eval* nil "((fn [{:syms [a]}] a) '{a 1})")))
  (is (= 1 (eval* nil "((fn [{:strs [a]}] a) '{\"a\" 1})")))
  (testing "default destructuring with false"
    (is (false? (eval* '(let [{:keys [:a] :or {a false}} {:b 1}] a))))))

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
                 @a)))))
  (testing "nested lets"
    (is (= [2 1] (eval* "(let [x 1] [(let [x 2] x) x])"))))
  (testing "let*"
    (is (= [2 1] (eval* "(let* [x 1] [(let* [x 2] x) x])"))))
  (testing "n let bindings"
    (let [ctx (sci/init {})
          forms (map (fn [n]
                       `(let [~'x 0]
                          (let [~@(mapcat identity (repeat n ['x '(inc x)]))]
                            ~'x)))
                     (range 20))]
      (is (= (range 20) (map #(sci/eval-form ctx %) forms))))))

(deftest closure-test
  (testing "closure"
    (is (= 1 (eval* "(let [x 1] (defn foo [] x)) (foo)"))))
  (testing "nested closures"
    (is (= 3 (eval* "(let [x 1 y 2] ((fn [] (let [g (fn [] y)] (+ x (g))))))")))))

(deftest fn-literal-test
  (is (= '(1 2 3)
         (eval* "(map #(do %) [1 2 3])")))
  (is (= '([0 1] [1 2] [2 3])
         (eval* "(map-indexed #(do [%1 %2]) [1 2 3])")))
  (is (= '(1 2 3)
         (eval* "(apply #(do %&) [1 2 3])"))))

(deftest fn-test
  #_(is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error) #"arg"
         (eval* '((fn foo [x] (if (< x 3) (foo 1 (inc x)) x)) 0))))
  (is (= 3 (eval* '((fn foo [x] (if (< x 3) (foo (inc x)) x)) 0))))
  (is (= [2 3] (eval* '((fn foo [[x & xs]] xs) [1 2 3]))))
  (is (= [2 3] (eval* '((fn foo [x & xs] xs) 1 2 3))))
  (is (= 2 (eval* '((fn foo [x & [y]] y) 1 2 3))))
  (is (= 1 (eval* '((fn ([x] x) ([x y] y)) 1))))
  (is (= 2 (eval* '((fn ([x] x) ([x y] y)) 1 2))))
  (is (= "otherwise" (eval* '((fn ([x & xs] "variadic") ([x] "otherwise")) 1))))
  (is (= "otherwise" (eval* '((fn ([x] "otherwise") ([x & xs] "variadic")) 1))))
  (is (= "variadic" (eval* '((fn ([x] "otherwise") ([x & xs] "variadic")) 1 2))))
  (is (= '(2 3 4) (eval* '(apply (fn [x & xs] xs) 1 2 [3 4]))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Can't have fixed arity function with more params than variadic function"
                        (eval* "   (fn ([& args]) ([v ]))")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Can't have more than 1 variadic overload"
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
  (is (= 1 (eval* "(try (let [] (def x 1) x))")))
  (testing "conditionally defining vars doesn't add their metadata yet"
    (is (true? (eval* "(when true (def ^{:test (fn [])} y 1)) (fn? (:test (meta #'y)))")))
    (is (false? (eval* "(when false (def ^{:test (fn [])} y 1)) (fn? (:test (meta #'y)))"))))
  (is (= :a (eval* "(def ^{:foo :bar :a (fn [] :a)} x) ((:a (meta #'x)))")))
  (is (= "foo" (eval* "(def ^{:doc (str \"foo\")} x) (:doc (meta #'x))")))
  (is (= 1 (eval* "(ns foo) (def foo/foo 1) #'foo foo/foo"))))

(deftest def-location-test
  (is (= 5 (eval* "(defmacro foodef [sym & body]
  `(when (odd? 1)
     (def ~sym ~@body)))

(foodef x 1)

(:line (meta #'x))")))
  (is (= 4 (eval* "(defmacro foodef [sym & body]
  `(do (def ~sym ~@body)))

(foodef x 1)

(:line (meta #'x))"))))

(deftest defn-test
  (is (= 2 (eval* "(do (defn foo \"increment c\" [x] (inc x)) (foo 1))")))
  (is (= 3 (eval* "(do (defn foo ([x] (inc x)) ([x y] (+ x y)))
                       (foo 1)
                       (foo 1 2))")))
  (is (= 0 (eval* "(do (defn foo [x] (inc x))
                       (defn foo \"decrement c\" [x] (dec x))
                       (foo 1))")))
  (is (= 1337 (eval* "(do (defn foo \"decrement c\" {:cool-meta (inc 1336)}
                            [x] (dec x))
                          (:cool-meta (meta #'foo)))")))
  (is (= 1337 (eval* "(do (defn foo {:cool-meta (inc 1336)}
                            [x] (dec x))
                          (:cool-meta (meta #'foo)))")))
  (is (= 1337 (eval* "(do (defn ^{:cool-meta (inc 1336)} foo
                            [x] (dec x))
                          (:cool-meta (meta #'foo)))")))
  (is (= 1337 (eval* "(defn ^{:test (fn [] (g))} g [] 1337) ((:test (meta #'g)))")))
  (testing "var contains location information which can be used by
  clojure.repl/source to read relevant source lines (see babashka)"
    (is (true?
         (eval* "
(defn foo []
  (+ 1 2 3))

(defn submap? [m1 m2]
  (every? (fn [k]
            (= (get m1 k) (get m2 k)))
          (keys m1)))

(submap? {:line 2, :column 1} (meta #'foo))"))))
  (testing "trailing metadata"
    (is (= 1337 (eval* "(do (defn foo
                            ([x] (dec x)) {:cool-meta (+ 1336 1)})
                          (:cool-meta (meta #'foo)))")))
    (is (map? (eval* "(defn- accept [x] {::op ::accept :ret x}) (accept 1)"))))
  (testing "metadata isn't evaluated on defn expression"
    (eval* "^{:inverse-of foo} (defn bar [])")))

(deftest defn-kwargs-test
  (is (= {:a 1} (sci/eval-string "(defn foo [& {:keys [a]}] {:a a}) (foo :a 1)")))
  (is (= {:a 1} (sci/eval-string "(defn foo [& {:keys [a]}] {:a a}) (foo {:a 1})"))))

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
  (is (= 1 (eval* "((symbol \"recur\") {'recur 1})")))
  (is (= [true false] (eval* "(mapv (comp some? resolve) '[inc x])")))
  (is (= [1 nil] (eval* "(def a 1) [@(resolve 'a) (resolve '{a 1} 'a)]")))
  #?(:clj
     (testing "type hints"
       (sci/eval-string
        (binding [*print-meta* true]
          (pr-str '(let [http-url "https://www.clojure.org"
                         conn ^java.net.HttpURLConnection (.openConnection (java.net.URL. http-url))]
                     (.connect conn))))
        {:namespaces {'clojure.core {'slurp slurp}}
         :classes {'java.net.HttpURLConnection java.net.HttpURLConnection
                   'java.net.URL java.net.URL}})
       (sci/eval-string
        (binding [*print-meta* true]
          (pr-str '(let [http-url "https://www.clojure.org"]
                     (let [conn (.openConnection (java.net.URL. http-url))]
                       (.getHeaderFieldKey ^java.net.HttpURLConnection conn 0)))))
        {:namespaces {'clojure.core {'slurp slurp}}
         :classes {'java.net.HttpURLConnection java.net.HttpURLConnection
                   'java.net.URL java.net.URL}})))
  #?(:clj (is (nil? (sci/eval-string "(resolve 'java.lang.Exception/foo)"
                                     {:classes {'java.lang.Exception java.lang.Exception}})))
     :cljs (is (nil? (sci/eval-string "(resolve 'js/Error)" {:classes {'js #js {:Error js/Error}}}))))
  (is (= 1 (eval* "((binding [*ns* 'user] (resolve 'inc)) 0)")))
  (is (= 2 (eval* "(def x 2) (let [x 1 x #'x] @x)"))))

#?(:clj
   (deftest type-hint-let-test
     (is (= (BigDecimal. 42)
            (sci/eval-string "
(defn- nested-hint [^Number n]
  ;; .longValue is available on Number, should work
  (let [^BigDecimal n (.add (BigDecimal. (.longValue n)) (BigDecimal. 2))]
    ;; .abs is available on BigDecimal but not non Number, should work
    (.abs n)))

(nested-hint (BigDecimal. -44))"
                             {:classes {'BigDecimal BigDecimal :allow :all}})))))

#?(:clj
   (deftest type-hint-let-pops-after-nest-test
     (try
       (sci/eval-string "
(defn- nested-hint [^Number n]
  (let [^BigDecimal n (BigDecimal. -123)]
    ;; .abs is available on BigDecimal but not non Number, should work
    (.abs n))
  ;; longValue is available on Number, should work
  (.longValue n)
  ;; precision is not available on BigDecimal but not on Number, should throw
  (.precision n))

(nested-hint (BigDecimal. -42))"
                        {:classes {'BigDecimal BigDecimal :allow :all}})
       (is false "expected a throw")
       (catch Exception e
         (is (= "precision" (-> e ex-data :message)))))))

#?(:clj
   (deftest type-hint-shadowed-def-test
     (is (= (BigDecimal. 42)
            (sci/eval-string "
(def ^Number n (BigDecimal. 17))

(defn- foo [^BigDecimal n]
  ;; .abs is available on BigDecimal but not non Number, should work
  (.abs n))

(foo (BigDecimal. 42))"
                             {:classes {'BigDecimal BigDecimal :allow :all}})))))

#?(:clj
   (deftest type-hint-catch-test
     (is (= "message"
            (sci/eval-string "
(defn foo [^Number x]
  (try
    (throw (ex-info \"message\" {}))
    (catch Exception x
      ;; .getMessage is available on Exception, should work
      (.getMessage x))))

(foo (BigDecimal. 42))"
                             {:classes {'BigDecimal BigDecimal :allow :all}})))))

#?(:clj
   (deftest type-hint-letfn-test
     (is (= (BigDecimal. 33)
            (sci/eval-string "
(defn foo [^Number bar]
  (letfn [(myfn [^BigDecimal bar]
            ;; .abs is available on BigDecimal but not non Number, should work
            (.abs bar))]
    (myfn (BigDecimal. 33))))

(foo (BigDecimal. 42))"
                             {:classes {'BigDecimal BigDecimal :allow :all}})))))

#?(:clj
   (deftest type-hint-fn-test
     (is (= (BigDecimal. 99)
            (sci/eval-string "
(defn foo [^Number x]
  (fn [^BigDecimal x]
    ;; .abs is available on BigDecimal but not non Number, should work
    (.abs x)))

((foo (BigDecimal. -72)) (BigDecimal. -99))"
                             {:classes {'BigDecimal BigDecimal :allow :all}})))))

(deftest ns-resolve-test
  (is (= 'join (eval* "(ns foo (:require [clojure.string :refer [join]])) (ns bar) (-> (ns-resolve 'foo 'join) meta :name)"))))

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
    (is (= 3 (tu/eval* "((fn [x] (if (> x 1) (inc x))) 2)" {:allow '[fn fn* if > inc]})))
    (is (= 3 ((tu/eval* "(fn [x] (if (> x 1) (inc x)))" {:allow '[fn fn* if > inc]}) 2))))
  (is (tu/eval* (str (list `#(inc %) 10)) {:allow '[fn* inc]}))
  (is (tu/eval* (str (list `#(let [x %] x) 10)) {:allow '[fn* let let*]}))
  (is (= [2 3 4] (sci/eval-string "(impl/mapv inc [1 2 3])" {:allow '[impl/mapv inc]
                                                             :namespaces {'impl {'mapv mapv}}})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(loop [] (recur))" {:deny '[loop*]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/loop [] (recur))" {:deny '[loop*]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/loop [] (recur))" {:deny '[recur]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"allowed"
                        (tu/eval* "(clojure.core/inc 1)" {:deny '[clojure.core/inc]})))

  (testing "for/doseq are macroexpanded properly"
    (is (= 'loop* (first (tu/eval* "(macroexpand '(doseq [i [1 2 3]] nil))" {}))))
    (is (= 'let*
           (first (tu/eval* "(macroexpand '(for [i [1 2 3]] i))" {})))))
  (testing "for/doseq/dotimes use loop in a safe manner, so `{:deny '[loop recur]}` should not forbid it, see #141"
    (is '(1 2 3) (tu/eval* "(for [i [1 2 3] j [4 5 6]] [i j])" {:deny '[loop recur]}))
    (is (nil? (tu/eval* "(doseq [i [1 2 3]] i)" {:deny '[loop recur]})))
    (is (nil? (tu/eval* "(dotimes [i 3] i)" {:deny '[loop recur]})))
    (testing "users should not be able to hack around this by messing with metadata"
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"allowed"
                            (tu/eval* "(def allowed-loop (with-meta (symbol \"loop\") {:line :allow}))
                                       (defmacro foo [] `(~allowed-loop [])) (foo)" {:deny '[loop* recur]})))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"allowed"
                            (tu/eval* "(let [allowed-loop (with-meta (symbol \"loop\") {:line :allow})]
                                         (defmacro foo [] `(~allowed-loop [])))
                                       (foo)" {:deny '[loop* recur]}))))
    (testing "but it should be forbidden in macros that are defined by a user"
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"allowed"
                            (tu/eval* "(defmacro foo [] `(loop [])) (foo)" {:deny '[loop* recur]})))))
  (testing "users cannot hack around sci.impl/needs-ctx"
    (is (= 1 (eval* "(defn ^{:sci.impl/op 'needs-ctx} foo [& args] (count args)) (foo 1)"))))
  (testing "vars introduced by users are allowed"
    (is (= [2 3] (tu/eval* "(def x 2) (def y 3) [x y]" {:allow '[def]}))) 2))

(deftest idempotent-eval-test
  (is (= '(foo/f1 foo/f2)
         (eval* "(map #(let [[ns v] %] (symbol (str ns) (str v))) '[[foo f1] [foo f2]])")))
  (is (= '(foo/f1)
         (eval* "(map #(let [[ns v] %] (symbol (str ns) (str v)))
                   (vector (vector (symbol \"foo\") (symbol \"f1\"))))")))
  (is (= '[["foo"] ["bar"]] (eval* "(map (fn [x] x) (list (list \"foo\") (list \"bar\")))"))))

(deftest error-location-test
  (when-not tu/native?
    (is (thrown-with-data?
         {:line 1 :column 11}
         (with-out-str (eval* nil "(+ 1 2 3) (conj 1 0)"))))
    (is (thrown-with-data?
         {:line 1 :column 19}
         (eval* "(+ 1 2 3 4 5) (do x)")))
    (tu/assert-submap {:type :sci/error, :line 1, :column 15,
                       :message #"Wrong number of args \(1\) passed to: user/foo"}
                      (try (eval* "(defn foo []) (foo 1)")
                           (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                             (let [d (ex-data ex)]
                               d))))
    (tu/assert-submap {:type :sci/error, :line 1, :column 21,
                       :message #"Wrong number of args \(0\) passed to: user/foo"}
                      (try (eval* "(defn foo [x & xs]) (foo)")
                           (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                             (let [d (ex-data ex)]
                               d))))
    (tu/assert-submap {:type :sci/error, :line 3, :column 12,
                       :message #"Wrong number of args \(2\) passed to: user/bindings"}
                      (try (eval* "
(defmacro bindings [a] (zipmap (mapv #(list 'quote %) (keys &env)) (keys &env)))
(let [x 1] (bindings))")
                           (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                             (let [d (ex-data ex)]
                               d))))
    #_(tu/assert-submap {:type :sci/error, :line 1, :column 25,
                         :message #"Wrong number of args \(0\) passed to: user/foo"}
                        (try (eval* (str "(defmacro foo [x & xs]) "
                                         "(foo)"))
                             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                               (let [d (ex-data ex)]
                                 d))))))

(deftest disable-arity-checks-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Cannot call foo with 1 arguments"
                        (sci/eval-string "(defn foo ([]) ([x y])) (foo 1)"
                                         {:disable-arity-checks true}))))

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
  (is (= '(foo 1 2 3) (eval* "(defmacro foo [x y z] (list 'quote &form)) (foo 1 2 3)")))
  (testing "top level macro that emits do form should be analyzed an eval'ed interleaved"
    (is (= 'foo (eval* "(defmacro dude []
                     `(do (ns ~'foo) (def ~'x (ns-name *ns*)) (ns ~'user)))
                   (dude)
                   foo/x"))))
  (testing "nested macro call that results in top level do"
    (is (true? (eval* "
(defprotocol Proto
  (proto [_]))

(defmacro deftrecord [name]
  `(defrecord ~name []
     Proto
     (proto [_] (new ~name))))

(deftrecord Rec)

(map? (proto (->Rec)))")))))

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

(defn throws-tail-ex [expr]
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"Can only recur from tail position"
       (sci/eval-string (pr-str expr)))
      (str "FAIL: does not throw recur exception: " expr)))

(defn it-works [expr]
  (is (any? (sci/eval-string (pr-str expr)
                             #?(:clj {:classes {'String String}}
                                :cljs {:classes {:allow :all
                                                 'js js/global}})))
      (str "FAIL: " expr)))

(deftest recur-test
  (is (= 10000 (tu/eval* "(defn hello [x] (if (< x 10000) (recur (inc x)) x)) (hello 0)"
                         {})))
  (testing "variadic recur"
    (is (= '(4) (eval* "((fn [& args] (if-let [x (next args)] (recur x) args)) 1 2 3 4)")))
    (is (= '(4) (eval* "((fn [x & args] (if-let [x (next args)] (recur x x) x)) nil 2 3 4)")))
    (is (= '((3 4) (5 6)) (eval* "
((fn [& sqs]
  (if (= 3 (ffirst sqs))
    sqs
    (recur (map #(map inc %) sqs)))) [1 2] [3 4])")))
    (is (= '(10) (eval* "(defn foo [x & xs]
                           (if (pos? x) (recur (dec x) (rest xs)) xs))
                         (apply foo 10 (range 11))"))))
  (testing "function with recur may be returned"
    (when-not tu/native?
      (let [f (eval* "(fn f [x] (if (< x 3) (recur (inc x)) x))")]
        (f 0))))
  (testing "non-tail usage of recur"
    (throws-tail-ex '(recur))
    (it-works '(fn [] (recur)))
    (throws-tail-ex '(do (recur)))
    (throws-tail-ex '(fn [] (do (recur)) (do (recur))))
    (throws-tail-ex '(fn [] [(recur)]))
    (it-works '[(fn [x] (recur x))])
    (throws-tail-ex '(fn [] {:a (recur)}))
    (it-works '{:a (fn [] (recur))})
    (throws-tail-ex '(fn [] (recur) 1 2))
    (it-works '(fn [] (prn) (prn) (recur)))
    (throws-tail-ex '(fn [] (let [x (recur)])))
    (it-works '(fn [] (let [x (fn [] (recur))])))
    (throws-tail-ex '(loop [x (recur)]))
    (it-works '(loop [x (fn [] (recur))]))
    (throws-tail-ex '(letfn [(f ([x] (f x 1)) ([x y] (+ x y)))] (recur) (f 1)))
    (throws-tail-ex '(letfn [(f ([x] (f x 1)) ([x y] (+ x y)))] (f 1) (recur)))
    (throws-tail-ex '(letfn [(f [x] (recur 1) (f x 1))]))
    (it-works '(letfn [(f [x] (recur 1))]))
    (throws-tail-ex '(fn [] (new #?(:clj String :cljs js/Error) (recur))))
    (it-works '(fn [] (new #?(:clj String :cljs js/Error) (fn [] (recur)))))
    (throws-tail-ex '(fn [] (throw (recur))))
    #?(:cljs (it-works '(fn [] (throw (fn [] (recur))))))
    (throws-tail-ex '(fn [] (.length (recur))))
    (it-works '(fn [] (.length (fn [] (recur)))))
    (it-works '(fn [] (try (fn [] (recur)))))
    (it-works '(for [i [1 2 3]] i))
    (it-works '(loop [x 1]
                 (if (< x 10)
                   (let [y (+ x 1)]
                     (do :yolo
                         (recur (inc x)))))))
    (throws-tail-ex '(loop [] (def x (recur))))
    (it-works '(loop [] (defn f [] (recur))))
    (throws-tail-ex '(fn [] (case (recur) 1 2)))
    (it-works '(fn [] (case 1
                        1 ;; case return
                        (recur)
                        ;; case default
                        (recur))))
    (throws-tail-ex '(fn [] (or (recur) 1)))
    (it-works '(fn [] (or (recur))))
    (it-works '(fn [] (or 1 2 3 (recur))))
    (throws-tail-ex '(fn [] (and (recur) 1)))
    (dotimes [i 20]
      (let [body (list 'fn [] (list* 'do (concat (range i) [(list 'recur) :tail])))]
        (throws-tail-ex body)))
    (it-works '(fn [] (and (recur))))
    (it-works '(fn [] (and 1 2 3 (recur))))
    (is (thrown-with-msg?
         Exception #"Cannot recur across try"
         (sci/eval-string "(defn foo [] (try (recur)))")))
    #?(:clj (do (throws-tail-ex '(String/new (recur)))
                (throws-tail-ex '(String/.length (recur)))))))

(deftest loop-test
  (is (= 2 (tu/eval* "(loop [[x y] [1 2]] (if (= x 3) y (recur [(inc x) y])))" {})))
  (is (= '(5 4 3 2 1) (tu/eval* "
(loop [l (list 2 1)
       c (count l)]
  (if (> c 4)
    l
    (recur (conj l (inc c)) (inc c))))
" {})))
  (is (= 4 (tu/eval* "
(defmacro & [])
(loop [[x & xs] [1 2 3 4 5]
       y x]
  (if (> x 4)
    y
    (recur xs x)))
" {})))
  (is (= 2 (tu/eval* "
(let [x 1]
  (loop [x (inc x)]
    x))
" {})))
  (is (= 1 (tu/eval* "
(let [let 1] (loop [x 1] x))"
                     {})))
  (is (= [1 2 3] (tu/eval* "
(ns exclude-loop (:refer-clojure :exclude [loop]))
(def state (atom []))
(doseq [i [1 2 3]] (swap! state conj i))
@state"
                     {}))))

(deftest for-test
  (is (= '([1 4] [1 6])
         (eval* "(for [i [1 2 3] :while (< i 2) j [4 5 6] :when (even? j)] [i j])")))
  (is (= (for [[_ counts] [[1 [1 2 3]] [3 [1 2 3]]] c counts] c)
         (eval* "(for [[_ counts] [[1 [1 2 3]] [3 [1 2 3]]] c counts] c)")))
  (is (= (for [[_ counts] [[1 [1 2 3]] [3 [1 2 3]]] c counts] c)
         (eval* "
(defn when []) (defn nth [])
(for [[_ counts] [[1 [1 2 3]] [3 [1 2 3]]] c counts] c)")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"vector"
                        (eval* "(for 1 [i j])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"even"
                        (eval* "(for [:dude] [i j])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"keyword"
                        (eval* "(for [x [1 2 3] :dude []] [i j])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"args"
                        (eval* "(for 1 2 3)"))))

(deftest doseq-test
  (when-not tu/native?
    (is (= "1\n1\n3\n9\n"
           (with-out-str
             (tu/eval* "(doseq [i [1 2 3]
                              :when (odd? i)
                              :let [j (* i i)]]
                        (println i) (println j))"
                       {:bindings {'println println}}))))))

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
       #?(:clj (eval* "
(try (case (inc 2), 1 true, 2 (+ 1 2 3))
  (catch java.lang.IllegalArgumentException e
    (throw (Exception. (ex-message e)))))")
          :cljs (eval* "(case (inc 2), 1 true, 2 (+ 1 2 3))"))))
  #?(:clj
     (testing "case generated by macro"
       (is (= :yolo
              (eval*
               (pr-str '(do (defn- codepoint-clause [[test result]]
                              (cond (list? test)
                                    [(map int test) result]
                                    :else
                                    [(int test) result]))

                            (defmacro ^:private codepoint-case [e & clauses]
                              `(case ~e
                                 ~@(mapcat codepoint-clause (partition 2 clauses))
                                 ~@(when (odd? (count clauses))
                                     [(last clauses)])))

                            (codepoint-case
                             49
                             (\1) :yolo)))))))))

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
    (is (= 1 (eval* "(try 1 2 nil 1)"))))
  #?(:cljs
     (testing "allow all"
       (let [ctx (sci/init {:classes {'js goog/global :allow :all}})]
         (is (= 12 (sci/eval-string* ctx "(try (assoc 1 1 1) (catch :default e 12))")))
         (is (= 12 (sci/eval-string* ctx "(try (assoc 1 1 1) (catch js/Object e 12))")))
         (is (= 12 (sci/eval-string* ctx "(try (assoc 1 1 1) (catch (if (odd? 1) js/Object js/Error) e 12))"))))))
  #?(:cljs
     (testing "in JS you can throw anything"
       (is (thrown? js/Error
                    (sci/eval-string "(try (throw \"blah\") (catch js/Error e (str e e)))")))
       (is (= "blahblah" (sci/eval-string "(try (throw \"blah\") (catch :default e (str e e)))")))))
  (is (= [1](sci/eval-string "(defn catch [] 1) (try [(catch)])"))))

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
  #?(:clj (is (= 'java.lang.Exception (eval* "`Exception"))))
  (is (= 'foo/x (eval* "(ns foo) (def x) (ns bar (:require [foo :refer [x]])) `x")))
  (is (= 'foo/inc (eval* "(ns foo (:refer-clojure :exclude [inc])) `inc")))
  (is (= 'foo/inc (eval* "(ns foo) (defn inc []) `inc")))
  (is (true? (eval* "(require '[clojure.string :refer [join]]) (= 'clojure.string/join `join)")))
  (is (true? (eval* "(ns foo) (defn inc []) (ns bar (:require [foo :refer [inc]])) (= 'foo/inc `inc)")))
  (is (= '[foo.bar user/foo] (eval* "(ns user) [`foo.bar `foo]")))
  (is (= '(user.Foo.) (sci/eval-string "(defrecord Foo []) `(Foo.)")))
  (is (= '... (sci/eval-string "(defrecord Foo []) `..."))))

(deftest defmacro-test
  (is (= [":hello:hello" ":hello:hello"]
         (eval* "(defmacro foo [x] (let [y (str x x)] `[~y ~y])) (foo :hello)")))
  (comment (is (= ["hellohello" "hellohello"]
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
  (is (str/includes? (str/lower-case (eval* "(declare x) (str x)")) "unbound"))
  (testing "declare var with metadata"
    (is (= 2 (eval* "(declare ^:dynamic d) (binding [d 2] d)")))
    (is (= "x" (eval* "(declare ^{:doc \"x\"} e) (-> #' e meta :doc)")))
    (is (= :hello (edn/read-string (sci/with-out-str (sci/eval-string "(declare ^{:foo (prn :hello)} foo)")))))))

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
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"simple symbol"
                        (eval* "(def f/b 1)")))
  (when-not tu/native?
    (is (thrown-with-data? {:line 1}
                           (eval* "(def f/b 1)"))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Too many arguments to def"
                        (eval* "(def -main [] 1)")))
  (is (= 1 (eval* "(def x \"foo\" 1) x")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"simple symbol"
                        (eval* "(defn f/b [])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"missing"
                        (eval* "(defn foo)")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"vector"
                        (eval* "(defn foo ())")))
  (is (eval* "(def *clause* \"During formatting, *clause* is bound to :select, :from, :where, etc.\" nil)")))

(deftest ex-message-test
  (is (= "foo" #?(:clj (eval* "(ex-message (Exception. \"foo\"))")
                  :cljs (eval* "(ex-message (js/Error. \"foo\"))")))))

(deftest assert-test
  (when-not tu/native?
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"should-be-true"
         (eval* "(def should-be-true false) (assert should-be-true)")))
    (let [d (try (eval* "(def should-be-true false) (assert should-be-true)")
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e (ex-data e)))]
      (is (= 1 (:line d)))))
  (sci/binding [sci/assert true]
    (sci/eval-string "(set! *assert* false) (assert false)"))
  (sci/binding [sci/assert true]
    (sci/eval-string "(set! *assert* true)
                      (try (assert false)
                        (catch #?(:clj java.lang.AssertionError :cljs js/Error) e :dude))"
                     {:features #?(:clj #{:clj}
                                   :cljs #{:cljs})})))

(deftest dotimes-test
  (when-not tu/native?
    (let [state (atom 0)]
      (tu/eval* "(dotimes [i 10] (swap! state inc))" {:bindings {'state state}})
      (is (= 10 @state)))))

(deftest clojure-walk-test
  (is (= {"a" {"b" 1}} (eval* "(clojure.walk/stringify-keys {:a {:b 1}})")))
  (is (= '(inc (inc 1)) (eval* "(clojure.walk/macroexpand-all '(-> 1 (-> inc inc)))"))))

(deftest letfn-test
  (is (= 2 (eval* "(letfn [(f ([x] (f x 1)) ([x y] (+ x y)))] (f 1))")))
  (is (= 3 (eval* "(letfn [(f ([x] (f x 1)) ([x y] (+ x y)))] (f 1 2))")))
  (is (= 11 (eval* "(letfn [(f [x] (g x)) (g [x] (inc x))] (f 10))")))
  (is (nil? (eval* "(letfn [(f [x] (g x)) (g [x] (inc x))])")))
  (testing "letfn fn can be evaluated outside of body of letfn"
    (is (= 3 (eval* "
(let [f (letfn
        [(f [x] (g x))
         (g [x] (+ x 2))]
          f)]
   (f 1))")))))

(deftest core-delay-test
  (is (= 1 (eval* "@(delay 1)"))))

(deftest defn--test
  (is (= 1 (eval* "(defn- foo [] 1) (foo)")))
  (is (true? (eval* "(defn- foo [] 1) (:private (meta #'foo))")))
  (is (= 1 (eval* "(defn get [url & [req]] url) (get 1)"))))

(deftest core-resolve-test
  (is (= 1 (eval* "((resolve 'clojure.core/inc) 0)")))
  (is (= 1 (eval* "((resolve 'inc) 0)")))
  (is (= true (eval* "(ns foo (:refer-clojure :exclude [inc])) (nil? (resolve 'inc))"))))

(deftest compatibility-test
  (is (true? (eval* "(def foo foo) (var? #'foo)")))
  (is (= 1 (eval* "((resolve 'clojure.core/inc) 0)")))
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
  (is (= '(user/bar 1) (eval* "(defmacro foo [x] `(bar ~x)) (defmacro bar [x] x) (macroexpand-1 '(foo 1))")))
  (is (= '(foobar) (eval* "(defmacro foo [] '(foobar)) (macroexpand '(foo))")))
  (is (= '(clojure.core/defrecord Foo []) (eval* "(macroexpand '(defrecord Foo []))")))
  (is (= '(. nil log) (eval* "(macroexpand-1 '(.log))")))
  (is (= '(. js/console log) (eval* "(macroexpand-1 '(.log js/console))")))
  (is (= '(. js/console log 1 2 3) (eval* "(macroexpand-1 '(.log js/console 1 2 3))")))
  )

(deftest macroexpand-call-test
  (is (= [1 1] (eval* "(defmacro foo [x] `(bar ~x)) (defmacro bar [x] [x x]) (macroexpand '(foo 1))")))
  (is (= '(. (. System (getProperties)) (get "os.name"))
         (eval* "(macroexpand '(.. System (getProperties) (get \"os.name\")))")))
  (is (= '[1 2 user/x] (eval* "(defmacro foo [x] `[1 2 x]) (macroexpand '(foo 1))"))))

(deftest load-fn-test
  (when-not tu/native?
    (is (= 1 (tu/eval* "
(let [ns 'foo]
  (require ns))
(foo/foo-fn)" {:load-fn (constantly
                         {:file "foo.clj"
                          :source "(ns foo) (defn foo-fn [] 1)"})})))

    #?(:cljs
       (do (is (= 1 (tu/eval* "(ns foo (:require-macros [foo :refer [my-macro]]))
                           (defmacro my-macro [x] x) (my-macro 1)" {})))
           (is (= 1 (tu/eval* "(ns foo (:require-macros [bar :refer [my-macro]])) (my-macro 1)"
                              {:load-fn (constantly {:source "(ns bar) (defmacro my-macro [x] x)"})})))
           (is (= 1 (tu/eval* "(ns foo (:require [bar :refer-macros [my-macro]])) (my-macro 1)"
                              {:load-fn (constantly {:source "(ns bar) (defmacro my-macro [x] x)"})})))))))

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
                                     foo {:file "foo.clj"
                                          :source "(ns foo) (println \"hello\")"}))}))))
    (testing "no reload"
      (is (= "hello\nhello\n"
             (sci/with-out-str
               (tu/eval* "
(require '[foo])
(require '[foo] :reload)
(require 'foo)
1"
                         {:load-fn (fn [{:keys [:namespace]}]
                                     (case namespace
                                       foo {:file "foo.clj"
                                             :source "(ns foo) (println \"hello\")"}))})))))))

(deftest reload-all-test
  (when-not tu/native?
    (is (= ":bar\n:foo\n:foo\n:bar\n:foo\n"
           (sci/with-out-str
             (tu/eval* "
(require '[foo])
(require '[foo])
(require '[foo] :reload)
(require 'foo :reload-all)
1"
                       {:load-fn (fn [{:keys [:namespace]}]
                                   (case namespace
                                     bar {:file "bar.clj"
                                          :source "(ns bar) (println :bar)"}
                                     foo {:file "foo.clj"
                                          :source "(ns foo (:require bar)) (println :foo)"}))}))))))

(deftest alter-meta!-test
  (is (true? (eval* "(doto (def x) (alter-meta! assoc :private true)) (:private (meta #'x))")))
  (is (true? (eval* "(doto (def x) (reset-meta! {:private true})) (:private (meta #'x))"))))

(deftest could-not-resolve-symbol-test3
  (when-not tu/native?
    (is (thrown-with-data? #"resolve.*def"
                           {:phase "analysis"}
                           (eval* "def")))))

(deftest function-results-dont-have-metadata
  (is (nil? (eval* "(meta (fn []))")))
  (is (nil? (eval* "(meta (fn ([]) ([_])))"))))

(deftest fn-on-meta-test
  (is (= "foo" (eval* "(def ^{:test (fn [] \"foo\")} x) ((:test (meta #'x)))"))))

(defrecord ReaderTestRecord [foo])

(deftest readers-test
  (when-not tu/native?
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"No reader function" (tu/eval* "#x/str 5" {})))
    (is (string? (tu/eval* "#x/str 5" {:readers {'x/str str}})))
    (let [res (tu/eval* "#example.Record{:foo 1}" {:readers {'example.Record map->ReaderTestRecord}})]
      (is (record? res)))))

(deftest built-in-vars-are-read-only-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"read-only"
       (tu/eval* "(alter-var-root #'clojure.core/inc (constantly dec)) (inc 2)" {})))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"read-only"
       (tu/eval* "(alter-meta! #'clojure.core/inc assoc :foo)" {})))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"read-only"
       (tu/eval* "(alter-meta! #'-> dissoc :macro)" {}))))

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
  (is (= 3 (eval* "(load-string \"1 2 3\")")))
  (is (= 'user (eval* "(load-string \"(ns bar)\") (ns-name *ns*)")))
  #?(:clj (is (= :foo (eval* "(with-in-str \":foo\" (read))"))))
  (is (= :foo (eval* "(def f (load-string \"(with-meta (fn [ctx] :foo) {:sci.impl/op 'needs-ctx})\")) (f 1)")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"loop.*allowed"
                        (tu/eval* "(eval (read-string \"(loop [] (recur))\"))" {:deny '[loop]})))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"loop.*allowed"
                        (tu/eval* "(load-string \"(loop [] (recur))\")" {:deny '[loop]})))
  (is (nil? (meta (tu/eval* "(read-string \"(+ 1 2 3)\")" nil)))))

(deftest while-test
  (is (= 10 (eval* "(def a (atom 0)) (while (< @a 10) (swap! a inc)) @a"))))

(deftest meta-on-syntax-quote-test
  (is (:foo (eval* "(meta `^:foo (1 2 3))"))))

(deftest atom-with-meta-test
  (is (= 1 (eval* "@(atom 1 :meta {:a 1})"))))

(deftest resolve-unquote
  (is (= 'clojure.core/unquote (eval* "`unquote"))))

(deftest ctx-test
  (let [ctx (sci/init {:bindings {'x 1}})]
    (is (= 1 (sci/eval-string* ctx "x")))
    (is (= 2 (do (sci/eval-string* ctx "(def x 2)")
                 (sci/eval-string* ctx "x"))))
    (let [forked (sci/fork ctx)]
      (is (= 3 (do (sci/eval-string* forked "(def y 3)")
                   (sci/eval-string* forked "y"))))
      (is (thrown-with-msg?
           #?(:clj Exception :cljs js/Error)
           #"Could not resolve symbol: y" (sci/eval-string* ctx "y"))))))

(defmacro do-twice [x] `(do ~x ~x))
(defn ^:sci/macro do-twice* [_ _ x] `(do ~x ~x))
(def ^:dynamic *foo* 1)
(defn always-foo [& _args] :foo)

(deftest copy-var-test
  (let [foo-ns (sci/create-ns 'foo)
        do-twice-var (sci/copy-var do-twice foo-ns)
        do-twice*-var (sci/copy-var do-twice* foo-ns)
        foo-var (sci/copy-var *foo* foo-ns)
        always-foo-var (sci/copy-var always-foo foo-ns)
        opts {:namespaces {'foo {'do-twice do-twice-var
                                 'do-twice* do-twice*-var
                                 '*foo* foo-var
                                 'always-foo always-foo-var}}}
        effects (sci/with-out-str (sci/eval-string "
(foo/do-twice (prn 1))
(foo/do-twice* (prn 1))
(prn (foo/always-foo))
(prn foo/*foo*)
(binding [foo/*foo* 10] (prn foo/*foo*))" opts))
        do-twice-doc (sci/with-out-str (sci/eval-string "(clojure.repl/doc foo/do-twice)" opts))
        always-foo-doc (sci/with-out-str (sci/eval-string "(clojure.repl/doc foo/always-foo)" opts))]
    (is (= "1\n1\n1\n1\n:foo\n1\n10\n" effects))
    (is (= "-------------------------\nfoo/do-twice\n([x])\nMacro\n" do-twice-doc))
    (is (= "-------------------------\nfoo/always-foo\n([& _args])\n" always-foo-doc))))

(defn update-vals*
  "Same as `update-vals` from clojure 1.11 but included here so tests
  can run with older versions of Clojure/Script."
  [m f]
  (with-meta
    (persistent!
     (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                (if #?(:clj (instance? clojure.lang.IEditableCollection m)
                       :cljs (implements? IEditableCollection m))
                  (transient m)
                  (transient {}))
                m))
    (meta m)))

(deftest copy-var*-test
  (let [ens (sci/create-ns 'edamame.core)
        publics (ns-publics 'edamame.core)
        sci-ns (update-vals* publics #(sci/copy-var* % ens))
        ctx (sci/init {:namespaces {'edamame.core sci-ns}})
        output (sci/eval-string* ctx "(require '[edamame.core :as e]) (e/parse-string \"1\")")]
    (is (= 1 output)))
  (is (= 'inc (-> (sci/copy-var* #'inc (sci/create-ns 'clojure.core))
                  meta :name))))

(deftest data-readers-test
  (is (= 2 (sci/eval-string "#t/tag 1" {:readers {'t/tag inc}})))
  (is (= 2 (sci/eval-string "#t/tag 1" {:readers (sci/new-var 'readers {'t/tag inc})}))))

(deftest exception-without-message-location-test
  (is (thrown-with-data?
       {:line 1 :column 2}
       (sci/eval-string " (clojure.string/includes? nil :foo)")))
  #?(:clj
     (is (thrown-with-data? {:line 1 :column 2}
                            (sci/eval-string " (throw (Exception.))")))))

(deftest intern-test
  (testing "interning results in unbound var"
    (when-not tu/native?
      (is (str/includes? (str (sci/eval-string "(ns foo) (ns bar) (intern 'foo 'x) foo/x"))
                         "Unbound"))))
  (testing "interning existing var returns var"
    (is (= [1 true] (sci/eval-string "(ns foo) (def ^:a x 1) (ns bar) [@(intern 'foo 'x) (:a (meta #'foo/x))]"))))
  (testing "interning existing var with value returns same var with value"
    (is (= [2 true]
           (sci/eval-string
            "(ns foo) (def ^:a x 1) (ns bar) [@(intern 'foo 'x 2) (:a (meta #'foo/x))]"))))
  (testing "interning var copies meta from name symbol"
    (is (true?
         (sci/eval-string
          "(ns foo) (ns bar) (intern 'foo (with-meta 'x {:a true}) 1) (:a (meta #'foo/x))"))))
  (testing "intern via API"
    (let [ctx (sci/init {:namespaces {'foo {}}})]
      (sci/intern ctx 'foo 'x 2)
      (is (= 2 (sci/eval-string* ctx "foo/x")))))
  (testing "with-redefs + intern"
    (is (true?
         (binding [unrestrict/*unrestricted* true]
           (sci/eval-string
            "(ns foo) (ns bar) (with-redefs [intern intern] (intern 'foo (with-meta 'x {:a true}) 1)) (:a (meta #'foo/x))"))))))

(deftest instance?-test
  (is (false? (eval* "(defrecord Foo []) (instance? Foo 1)")))
  (is (true? (eval* "(defrecord Foo []) (instance? Foo (->Foo))")))
  #?(:clj (is (true? (eval* "(instance? Number 1)"))))
  (is (thrown? #?(:clj Exception :cljs js/Error) (eval* "(instance? 'Foo 1)"))))

(deftest threading-macro-test
  (testing "->"
    (is (= 4 (eval* 1 '(-> *in* inc inc (inc)))))
    (is (= '([0 1] [1 2] [2 3]) (eval* '(map-indexed #(-> [%1 %2]) [1 2 3]))))
    (is (= '(1 2 3) (eval* '(-> '(1 2 3))))))
  (testing "->>"
    (is (= 7 (eval* ["foo" "baaar" "baaaaaz"] "(->> *in* (map count) (apply max))"))))
  (testing "macroexpand ->"
    (is (= '(/ (inc 9) 100)
           (eval* "(macroexpand '(-> 9 inc (/ 100)))"))))
  (testing "macroexpand ->>"
    (is (= '(/ 100 (inc 9))
           (eval* "(macroexpand '(->> 9 inc (/ 100)))")))))

(deftest bound-test
  (is (false? (eval* "(def x) (bound? #'x)")))
  (is (true? (eval* "(def x 1) (bound? #'x)")))
  (is (false? (eval* "(def ^:dynamic x) (bound? #'x)")))
  (is (true? (eval* "(def ^:dynamic x) (binding [x 1] (bound? #'x))")))
  (is (false? (eval* "(def ^:dynamic x) (binding [x 1]) (bound? #'x)"))))

(deftest call-quoted-symbol-test
  (is (= 1 (eval* "('a {'a 1})"))))

(deftest meta-test
  (testing "Metadata can be changed by user, even if it conflicts with sci's metadata"
    (is (= {:column 14 :line 2} (eval* "(meta (with-meta [] {:line 2 :column 14}))"))))
  (testing "Reader metadata is preserved"
    (is (true? (eval* "(:foo (meta ^:foo #{1 2 3}))")))
    (is (true? (eval* "(:foo (meta ^:foo [1 2 3]))")))
    (is (true? (eval* "(:foo (meta ^:foo {:a 1}))"))))
  (testing "Reader metadata is evaluated on colls"
    (testing "constant colls"
      (is (true? (eval* "(symbol? (:foo (meta ^{:foo 'bar} {})))")))
      (is (true? (eval* "(= 6 (:foo (meta ^{:foo (+ 1 2 3)} [])))")))
      (is (true? (eval* "(= 6 (:foo (meta ^{:foo (+ 1 2 3)} #{})))"))))
    (testing "non-constant colls"
      (is (true? (eval* "(:foo (meta ^:foo {:x (rand-int 10)}))")))
      (is (true? (eval* "(:foo (meta ^:foo [(rand-int 10)]))")))
      (is (true? (eval* "(:foo (meta ^:foo #{(rand-int 10)}))")))))
  (testing "Metadata is evaluated after form"
    (is (= "123" (eval* "(with-out-str ^{(print 2) (print 3)} {:b (print 1)})")))
    (is (= "123" (eval* "(with-out-str ^{(print 2) (print 3)} #{(print 1)})")))
    (is (= "123" (eval* "(with-out-str ^{(print 2) (print 3)} [(print 1)])"))))
  (testing "Reader metadata is evaluated on fns"
    (is (true? (eval* "(= 6 (:foo (meta ^{:foo (+ 1 2 3)} (fn []))))")))
    (testing "Fns don't have :line and :column metadata"
      (is (true? (eval* "(nil? (:line (meta ^{:foo (+ 1 2 3)} (fn []))))")))))
  (testing "meta on nested maps"
    (is (= {:m true} (eval* "(meta ^:m {:foo :bar})")))
    (is (= {:m true} (eval* "(meta ^:m {:foo {}})")))
    (is (= {:m 6} (eval* "(meta ^{:m (+ 1 2 3)} {:foo {}})")))
    (is (= {:n 6} (eval* "(meta (:m (meta ^{:m ^{:n (+ 1 2 3)} {}} {:foo {}})))")))))

(deftest symbol-on-var-test
  (is (= 'user/x (eval* "(def x 1) (symbol #'x)"))))

(deftest macro-val-error-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs :default) #"value of a macro"
       (eval* "(defmacro foo []) foo")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs :default) #"value of a macro"
       (eval* "->")))
  (testing "throw at analysis time"
    (is (thrown-with-msg?
         #?(:clj Exception :cljs :default) #"value of a macro"
         (eval* "(defmacro foo []) (defn bar [] foo)")))))

(deftest var-isnt-fn
  (is (false? (eval* "(fn? #'inc)"))))

(deftest array-based-map-test
  (is (true? (eval* "(= (range 8) (keys {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7}))")))
  (is (true? (eval* "(= '(:a :b) (keys (let [x 1] {:a x :b 2})))")))
  (testing "This is an implementation detail of Clojure, but we use it to check if we really create hash-maps here"
    (is (false? (eval* "(= (range 100) (keys (zipmap (range 100) (range 100))))")))))

#?(:clj
   (deftest merge-opts-test
     (let [ctx (sci/init {:classes {'System System}})
           ctx2 (sci/merge-opts ctx {:classes {'Thread Thread}})]
       (is (sci/eval-string* ctx2 "System Thread"))))
   :cljs
   (deftest merge-opts-test
     (let [ctx (sci/init {:classes {'js goog/global :allow :all}})
           ctx2 (sci/merge-opts ctx {:namespaces {}})]
       (is (sci/eval-string* ctx2 "(try (assoc 1 1 1) (catch js/Error e 12))")))))

(deftest merge-opts-with-new-vars-test
  (let [C (atom (sci/init {:namespaces {'n {'foo 1}}}))]
    (is (= 1 (sci/eval-form @C 'n/foo)))
    (swap! C sci/merge-opts {:namespaces {'n {'foo 2}}})
    (is (= 2 (sci/eval-form @C 'n/foo)))))

(deftest merge-opts-preserves-features-test
  (let [ctx(sci/init {:features #{:cljs}})]
    (is (= 2 (sci/eval-string* ctx "#?(:clj 1 :cljs 2)")))
    (is (= 2 (sci/eval-string* (sci/merge-opts ctx {}) "#?(:clj 1 :cljs 2)")))))

(deftest dynamic-meta-def-test
  (is (= false (eval* "(def ^{:private (if (odd? 1) false true)} foo) (:private (meta #'foo))")))
  (is (= "6" (eval* "(def ^{:doc (str (+ 1 2 3))} foo) (:doc (meta #'foo))")))
  (is (= "6" (eval* "(defn ^{:doc (str (+ 1 2 3))} foo []) (:doc (meta #'foo))")))
  (let [ctx (sci/init {})]
    (sci/binding [sci/file "file1"] (sci/eval-string* ctx "(ns dude) (defn foo [])"))
    (is (= "file1" (:file (meta (sci/eval-string* ctx "#'dude/foo")))))
    (sci/binding [sci/file "file2"] (sci/eval-string* ctx "(ns dude) (defn foo [])"))
    (is (= "file2" (:file (meta (sci/eval-string* ctx "#'dude/foo")))))))

(deftest self-ref-test
  (testing "self-referantial function is equal to itself"
    (is (true? (eval* "(def f (fn foo [] foo)) (= f (f))")))
    (is (true? (eval* "(letfn [(f [] f)] (= f (f)))")))
    (is (= :a (eval* "
(defn foof [x]
  (let [f (fn f
            ([] (f nil))
            ([_] x))]
    f))

(def f1 (foof :a))
(def f2 (foof :b))

(f1)
")))))

(deftest more-than-twenty-args-test
  (is (nil? (eval* '(comment
                      0 1 2 3 4 5 6 7 8 9
                      0 1 2 3 4 5 6 7 8 9
                      0 1 2 3 4 5 6 7 8 9
                      0 1 2 3 4 5 6 7 8 9
                      0 1 2 3 4 5 6 7 8 9))))
  (is (= {1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 9, 9 10}
         (eval* '(assoc {} 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 10)))))

(deftest eval-file-meta-test
  (testing "error during analysis"
    (let [data (try (sci/eval-string "^{:clojure.core/eval-file \"dude.clj\"} (identity (hi))")
                    (catch #?(:clj Exception :cljs :default) e
                      (ex-data e)))]
      (is (= "analysis" (:phase data)))
      (is (= "dude.clj" (:file data)))))
  (testing "error at runtime"
    (let [data (try (sci/eval-string "^{:clojure.core/eval-file \"dude.clj\"} (let [x :foo] (assoc x :hello 1))")
                    (catch #?(:clj Exception :cljs :default) e
                      (ex-data e)))]
      (is (= "dude.clj" (:file data)))))
  (testing "eval at runtime"
    (let [data (sci/eval-string "^{:clojure.core/eval-file \"dude.clj\"} [{:a *file*}]")]
      (is (= "dude.clj" (-> data first :a))))
    (let [data (sci/eval-string "^{:clojure.core/eval-file \"dude.clj\"} [{:a *file*}]")]
      (is (= "dude.clj" (-> data first :a))))
    (let [data (sci/eval-string "(load-string \"^{:clojure.core/eval-file \\\"dude.clj\\\"} {:a [*file*]}\")")]
      (is (= "dude.clj" (-> data :a first))))))

#?(:cljs
   (deftest eval-js-obj-test
     (is (= 1 (sci/eval-string "(def o #js {:a 1}) (.-a o) "
                               {:classes {'js goog/global :allow :all}})))
     (is (= :a (sci/eval-string "(def o #js {:a :a}) (.-a o) "
                                {:classes {'js goog/global :allow :all}})))
     (is (= 1 (sci/eval-string "(def o #js {:a (fn [] 1)}) (.a o) "
                               {:classes {'js goog/global :allow :all}})))
     (testing "js objects are not instantiated at read time, but at runtime, rendering new objects each time"
       (sci/eval-string "(apply identical? (for [x [1 2]] #js {:a 1}))" {:classes {'js goog/global :allow :all}}))))

(deftest copy-ns-test
  (let [sci-ns (sci/copy-ns sci.copy-ns-test-ns
                            (sci/create-ns 'sci.copy-ns-test-ns)
                            {:exclude [baz quux]
                             :copy-meta [:doc :copy-this]})]
    (is (map? sci-ns))
    (is (= 5 (count sci-ns)))
    (is (= #{'foo 'bar 'ITest 'x 'vec-macro} (set (keys sci-ns))))
    (is (= [:foo :bar] (sci/eval-string
                        "(require '[sci.copy-ns-test-ns :refer [foo bar]])
                         [(foo) (bar)]"
                        {:namespaces {'sci.copy-ns-test-ns sci-ns}})))
    (is (= "YOLO" (:doc (meta (get sci-ns 'foo)))))
    (is (:copy-this (meta (get sci-ns 'foo)))))
  (let [sci-ns (sci/copy-ns sci.copy-ns-test-ns
                            (sci/create-ns 'sci.copy-ns-test-ns)
                            {:exclude-when-meta [:exclude-this]
                             :copy-meta :all})]
    (is (= 7 (count sci-ns)))
    (is (= #{'foo 'bar 'baz 'skip-wiki 'ITest 'x 'vec-macro} (set (keys sci-ns))))
    (is (= "YOLO" (:doc (meta (get sci-ns 'foo)))))
    (is (:copy-this (meta (get sci-ns 'foo))))
    (is (:awesome-meta (meta (get sci-ns 'baz))))
    (is (= [1 1] (sci/eval-string "(vec-macro 1)" {:namespaces {'user sci-ns}}))))
  ;; throws at compile time, so it's difficult to test this:
  #_(is (thrown? Exception (sci/copy-ns #_:clj-kondo/ignore non-existent.namespace nil))))

(deftest copy-ns-default-meta-test
  (testing "copy-ns default meta includes name"
    (let [sci-ns (sci/copy-ns sci.copy-ns-test-ns
                              (sci/create-ns 'sci.copy-ns-test-ns))]
      (is (every? (fn [[var-name meta]] (and (symbol? var-name) (= var-name (:name meta))))
                  (map (fn [[name var]] (vector name (meta var))) sci-ns))))))

(deftest vswap-test
  (is (= 2 (sci/eval-string
            "(def v (volatile! 1)) (vswap! v inc) @v"))))

(deftest to-array-2d-test
  (let [[alen type-eq] (eval* "(def arr (to-array-2d [[1 2][3 4]]))
                               [(alength arr)
                                (= (type (object-array 1)) (type (aget arr 0)))]")]
    (is (= 2 alen))
    (is type-eq)))

(deftest aclone-test
  (let [[eq a al b bl] (eval* "(let [a (into-array [1 2 3])
                            b (aclone a)]
                        [(= a b) (vec a) (alength a) (vec b) (alength b)])")]
    (is (not eq))
    (is (= al bl))
    (is (= (vec a) (vec b)))))

(deftest areduce-test
  (is (= 6.0 (eval* "(defn asum [xs]
                        (areduce xs i ret (float 0)
                                 (+ ret (aget xs i))))
                     (asum (into-array [1.0 2.0 3.0]))"))))

(deftest amap-test
  (let [mapped-array
        (eval* "(def an-array (into-array (range 5)))
                (vec (amap an-array
                      idx
                      ret
                      (+ (int 1)
                         (aget an-array idx))))")]
    (is (= [1 2 3 4 5] mapped-array))))

#?(:clj
   (deftest clojure-version-test
     (is (str/ends-with? (sci/eval-string "(clojure-version)") "SCI"))
     (is (str/ends-with? (:qualifier (sci/eval-string "*clojure-version*"))
                         "SCI"))))

(deftest empty-coll-identical-test
  (is (identical? [] (sci/eval-string "[]")))
  (is (identical? #{} (sci/eval-string "#{}"))))


(deftest var-name-test
  (testing "var name strings match their namespace and symbol"
    (is (empty? (sci/binding [sci/out *out*] (sci/eval-string "
 (require '[clojure.string :as str])
 (let [ns-maps  (->> (all-ns)
                    (map (fn [nmspc] [(ns-name nmspc) (ns-publics nmspc)]))
                    (into {}))] ; a map of { ns-name {symbol var, ...}}
  (->>
    (for [[ns-nm _] ns-maps
        [sym vr]  (ns-maps ns-nm)
        :let [{var-meta-ns :ns, var-meta-name :name} (meta vr)
              ;; _ (prn var-meta-name)
              var-meta-ns-name ((fnil ns-name (create-ns \"missing-ns\")) var-meta-ns)]]
      ; build a seq of maps containing the ns/symbol from the ns and the ns/symbol from the var's metadata
      {:actual-ns ns-nm :actual-ns-symbol sym :var-meta-ns var-meta-ns-name :var-meta-name var-meta-name})
    ; remove the protocol/interface-ish vars whose metas don't really match
    (remove (fn [{:keys [var-meta-name]}]
              (str/starts-with? (name var-meta-name) \"cljs.core.\")))
    (filter (complement #(and (= (:actual-ns %) (:var-meta-ns %)) (= (:actual-ns-symbol %) (:var-meta-name %)))))
    (doall)))"))))))

#?(:cljs
   (deftest require-cljs-core-test
     (is (= 3 (sci/eval-string "(require '[cljs.core :as c]) (c/inc 2)")))))

(deftest ns-aliases-test
  (is (= 1 (sci/eval-string "(require '[foobar :as foo]) (foo/read-string \"1\")"
                            {:ns-aliases '{foobar clojure.edn}})))
  (is (= 1 (sci/eval-string "(clojure.dude/inc 0)"
                            {:ns-aliases '{clojure.dude clojure.core}}))))

#?(:clj
   (deftest sandbox-print-method-test
     (is (thrown-with-msg?
          Exception #"allowed"
          (sci/eval-string "(defmethod print-method Integer [x w])")))))

(deftest memfn-test
  (is (true? (sci/eval-string "((memfn startsWith prefix) \"abc\" \"a\")" {:classes {:allow :all}}))))

(deftest sci-error-test
  (let [st (sci/eval-string
            (-> "(require '[sci.core :as sci]) (defn foo [] (assoc :foo :bar)) (defn bar [] (try (foo) (catch ^:sci/error Exception e (sci/format-stacktrace (sci/stacktrace e))))) (bar)"
                #?(:cljs (str/replace "Exception" "js/Error")))
            {:namespaces {'sci.core {'stacktrace sci/stacktrace
                                     'format-stacktrace sci/format-stacktrace}}})]
    (is (str/includes? (str st) "1:31"))))

(deftest var->sym-test
  (is (= 'clojure.core/inc (sci/var->symbol (sci/eval-string "#'inc")))))

(deftest api-resolve-test
  (is (= 2 ((sci/resolve (sci/init {}) 'clojure.core/inc) 1))))

(deftest do-and-or-test
  (let [ctx (sci/init {})]
    (is (nil? (sci/eval-form ctx '(or))))
    (is (true? (sci/eval-form ctx '(and))))
    (let [case-fn (fn [op fill n]
                    (map (fn [n]
                           (cons op (concat (repeat n fill) ['(+ 1 2 3)])))
                         (range n)))
          or-cases (case-fn 'clojure.core/or nil 20)
          and-cases (case-fn 'clojure.core/and true 20)
          do-cases (let [cases (case-fn 'clojure.core/do '(+ 1 2) 10000)]
                     (concat (take 20 cases) (take 1 (reverse cases))))]
      (doseq [case or-cases]
        (is (= 6 (sci/eval-form ctx case))))
      (doseq [case and-cases]
        (is (= 6 (sci/eval-form ctx case))))
      (doseq [case do-cases]
        (is (= 6 (sci/eval-form ctx case)))))))

(deftest type-test
  (is (= :user/foo (sci/eval-string "(def x ^{:type ::foo} []) (type x)")))
  (is (= :my-custom-type (sci/eval-string "(do (defrecord Person [name age]) (let [r (with-meta (->Person \"John\" 30) {:type :my-custom-type})] (type r)))"))))

(deftest conditional-var-test
  ;; NOTE: :file is not conform Clojure JVM, maybe fix this another day
  (is (= [:name :ns :file] (sci/eval-string "(when false (def ^:foobar x)) (keys (meta #'x))"))))

(deftest override-ns-test
  (is (= 3 (sci/eval-string "(ns 2)" {:namespaces {'clojure.core {'ns inc}}}))))

(deftest lazy-seq-macroexpand-test
  (is (= [1 2 3] (sci/eval-string "(eval (macroexpand '(lazy-seq [1 2 3])))"))))

(deftest eval-string+-test
  (let [ctx (sci/init {})
        {val1 :val ns1 :ns} (sci/eval-string+ ctx "(ns dude) (def x 1) (str #'x)")
        {val2 :val ns2 :ns} (sci/eval-string+ ctx "(def y 1) [(str #'x) (str #'y)]" {:ns ns1})]
    (is (= "#'dude/x" val1))
    (is (= "dude" (str ns1)))
    (is (= ["#'dude/x" "#'dude/y"] val2))
    (is (= "dude" (str ns2)))))

#?(:cljs
   (deftest queue-test
     (is (= #queue [1 2 3] (sci/eval-string "#queue [1 2 3]")))))

(deftest time-test
  #?(:clj
     (let [output (java.io.StringWriter.)]
       (is (= 1 (sci/binding [sci/out output] (sci/eval-string "(time 1)"))))
       (is (re-matches #"\"Elapsed time: \d\.\d+ msecs\"\s*" (str output))))
     :cljs
     (let [output (atom "")
           print-fn #(swap! output str %)]
       (is (= 1 (sci/binding [sci/print-fn print-fn] (sci/eval-string "(time 1)" {:classes {'js js/globalThis :allow :all}}))))
       (is (re-matches #"\"Elapsed time: \d\.\d+ msecs\"\s*" @output)))))

#?(:cljs
   (deftest exists?-test
     (is (true? (sci/eval-string "(exists? cljs.core.first)")))
     (is (true? (sci/eval-string "(exists? cljs.core/first)")))
     (is (true? (sci/eval-string "(exists? js/console)" {:classes {'js js/globalThis
                                                                   :allow :all}})))
     (is (true? (sci/eval-string "(exists? js/console.log)" {:classes {'js js/globalThis
                                                                       :allow :all}})))
     (is (false? (sci/eval-string "(exists? js/foo.bar)" {:classes {'js js/globalThis
                                                                               :allow :all}})))
     (is (false? (sci/eval-string "(exists? js/console.log.foobar)" {:classes {'js js/globalThis
                                                                               :allow :all}})))
     (is (false? (sci/eval-string "(exists? console.log)" {:classes {'js js/globalThis
                                                                     :allow :all}})))))

#?(:clj
   (deftest macros-can-be-used-with-apply-test
     (let [ctx (sci/init {})]
       (store/with-ctx ctx
         (is (true? (sci/eval-string* ctx "
(defprotocol Foo)
(eval (apply #'extend-protocol nil nil 'Foo '[String]))
(satisfies? Foo \"dude\")")))
         (is (true? (sci/eval-string* ctx "
(defprotocol Foo)
(eval (apply #'extend-type nil nil 'String '[Foo]))
(satisfies? Foo \"dude\")")))
         (is (true? (sci/eval-string* ctx "(eval (apply #'defmulti nil nil 'my-multi '[identity]))
(some? (resolve 'my-multi))")))
         (is (true? (sci/eval-string* ctx "(eval (apply #'defrecord nil nil 'Foo '[[]]))
(some? (->Foo))")))
         (is (true? (sci/eval-string* ctx "(eval (apply #'deftype nil nil 'Foo '[[]]))
(some? (->Foo))")))))))

(deftest issue-977-test
  (is (= :sci.impl.analyzer/recur (sci/eval-string "(ns sci.impl.analyzer) ((fn [] ::recur))"))))

(deftest var-without-configured-namespace-test
  (is (= "#'xxx/x" (str (sci/binding [sci/ns (sci/create-ns 'xxx)] (sci/eval-string "(def x 'x)"))))))

#?(:cljs
   (deftest set!-property-test
     (is (= {:a 1} (sci/eval-string "(do (def x #js {}) (set! x -a 1) (js->clj x :keywordize-keys true))")))
     (is (= {:a {:a 1}} (sci/eval-string "(do (def x #js {:a #js {}}) (set! x.a -a 1) (js->clj x :keywordize-keys true))")))))

;;;; Scratch

(comment
  (eval* 1 '(inc *in*))
  (test-difference "foo" "[10 10]" 0 10)
  (test-difference "rand" #(rand) 0 10))
