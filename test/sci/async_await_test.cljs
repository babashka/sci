(ns sci.async-await-test
  (:require [clojure.test :as test :refer [deftest is testing async]]
            [promesa.core :as p]
            [sci.core :as sci]))

;; Let bindings: simple, sequential, mixed, nested
(deftest async-fn-let-bindings-test
  (testing "let bindings with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-let []
                              {:simple
                               (let [x (await (js/Promise.resolve 1))]
                                 (inc x))

                               :sequential
                               (let [x (await (js/Promise.resolve 1))
                                     y (await (js/Promise.resolve 2))]
                                 (+ x y))

                               :mixed
                               (let [x 1
                                     y (await (js/Promise.resolve 2))
                                     z 3]
                                 (+ x y z))

                               :nested
                               (let [x (await (js/Promise.resolve 10))]
                                 (let [y (await (js/Promise.resolve 11))]
                                   (+ x y)))

                               :destructuring
                               (let [[x y] (await (js/Promise.resolve [1 2]))]
                                 (+ x y))})
                            (test-let)")]
                 (p/let [result v]
                   (is (= {:simple 2 :sequential 3 :mixed 6 :nested 21 :destructuring 3} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Do and expressions with await
(deftest async-fn-do-expressions-test
  (testing "do and expressions with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(def a (atom 0))
                            (defn ^:async test-do []
                              {:do-basic
                               (do
                                 (reset! a 1)
                                 (await (js/Promise.resolve nil))
                                 (swap! a inc)
                                 @a)

                               :await-in-expr
                               (let [x (await (js/Promise.resolve 10))]
                                 (let [y (inc (await (js/Promise.resolve 11)))]
                                   (+ x y)))

                               :await-in-do-binding
                               (let [x (await (js/Promise.resolve 10))]
                                 (let [y (do (await (js/Promise.resolve 11)))]
                                   (+ x y)))

                               :nested-await
                               (inc (await (inc (await (js/Promise.resolve 1)))))})
                            (test-do)")]
                 (p/let [result v]
                   (is (= {:do-basic 2 :await-in-expr 22 :await-in-do-binding 21 :nested-await 3} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Threading macros and macro expansion
(deftest async-fn-macros-test
  (testing "macros with await"
    (async done
           (-> (p/let [ctx (sci/init {:namespaces {'user {'my-await ^:sci/macro (fn [_ _ x] (list 'await x))}}
                                      :classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-macros []
                              {:thread-first
                               (-> (js/Promise.resolve 10)
                                   (await)
                                   (+ 5))

                               :local-shadowing
                               (let [-> (fn [x y] (+ x y))]
                                 (-> (await (js/Promise.resolve 1)) 1))

                               ;; inner fn must be async to use await
                               :param-shadowing
                               (await ((^:async fn [->]
                                         (-> (await (js/Promise.resolve 1)) 1))
                                       (fn [x y] (+ x y))))

                               :custom-macro
                               (inc (my-await (js/Promise.resolve 1)))})
                            (test-macros)")]
                 (p/let [result v]
                   (is (= {:thread-first 15 :local-shadowing 2 :param-shadowing 2 :custom-macro 2} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Async/anonymous functions
(deftest async-fn-functions-test
  (testing "async and anonymous functions"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-fns []
                              {:nested-async-fn
                               (let [add-async (^:async fn [x y] (+ (await x) y))]
                                 (await (add-async (js/Promise.resolve 1) 2)))

                               ;; async fns return promises, must await
                               :no-await-returns-promise
                               (await ((^:async fn [x] (+ x 1)) 41))

                               :anonymous-async
                               (await ((^:async fn [] (await (js/Promise.resolve 42)))))

                               :async-calling-async
                               (let [async-add (^:async fn [x y]
                                                 (+ (await (js/Promise.resolve x))
                                                    (await (js/Promise.resolve y))))
                                     async-mul (^:async fn [x y]
                                                 (* (await (async-add x 1))
                                                    (await (js/Promise.resolve y))))]
                                 (await (async-mul 2 3)))})
                            (test-fns)")]
                 (p/let [result v]
                   (is (= {:nested-async-fn 3 :no-await-returns-promise 42 :anonymous-async 42 :async-calling-async 9} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Try/catch/finally and throw
(deftest async-fn-try-catch-test
  (testing "try/catch/finally with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-try []
                              {:catch-rejected
                               (try
                                 (await (js/Promise.reject (js/Error. \"oops\")))
                                 (catch :default e
                                   \"caught\"))

                               :no-error
                               (try
                                 (await (js/Promise.resolve 42))
                                 (catch :default e
                                   \"caught\"))

                               :throw-with-await
                               (try
                                 (throw (js/Error. (await (js/Promise.resolve \"err\"))))
                                 (catch :default e
                                   (.-message e)))})
                            (test-try)")]
                 (p/let [result v]
                   (is (= {:catch-rejected "caught" :no-error 42 :throw-with-await "err"} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Case with await
(deftest async-fn-case-test
  (testing "case with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-case []
                              {:await-in-test
                               (case (await (js/Promise.resolve 1))
                                 1 :one
                                 2 :two)

                               :await-in-result
                               (case 2
                                 1 :one
                                 2 (await (js/Promise.resolve :two)))

                               :await-in-default
                               (case 999
                                 1 :one
                                 2 :two
                                 (await (js/Promise.resolve :default-value)))

                               ;; match constant is literal, not expanded
                               :match-constant-literal
                               (case (await (js/Promise.resolve 'await))
                                 (await 1) :matched-await-list
                                 2 :two
                                 :default)})
                            (test-case)")]
                 (p/let [result v]
                   (is (= {:await-in-test :one :await-in-result :two :await-in-default :default-value :match-constant-literal :matched-await-list} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Letfn with await
(deftest async-fn-letfn-test
  (testing "letfn with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-letfn []
                              {:basic
                               (letfn [(helper [x] (inc x))]
                                 (helper (await (js/Promise.resolve 1))))

                               :in-binding
                               (let [g (letfn [(f [x] x)]
                                         (await (js/Promise.resolve 1)))]
                                 [g])})
                            (test-letfn)")]
                 (p/let [result v]
                   (is (= {:basic 2 :in-binding [1]} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Loop/recur with await
(deftest async-fn-loop-recur-test
  (testing "loop/recur with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-loop []
                              {:basic-loop
                               (loop [x 0 acc []]
                                 (if (< x 3)
                                   (recur (inc x) (conj acc (await (js/Promise.resolve x))))
                                   acc))

                               :doseq
                               (let [a (atom [])]
                                 (doseq [x [1 2 3]]
                                   (swap! a conj (await (js/Promise.resolve x))))
                                 @a)})
                            (test-loop)")]
                 (p/let [result v]
                   (is (= {:basic-loop [0 1 2] :doseq [1 2 3]} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Collection literals with await
(deftest async-fn-collection-literals-test
  (testing "collection literals with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-colls []
                              {:vector
                               [(await (js/Promise.resolve 1))
                                (await (js/Promise.resolve 2))
                                (+ 1 (await (js/Promise.resolve 2)))]

                               :set
                               #{(await (js/Promise.resolve 1))
                                 (await (js/Promise.resolve 2))}

                               :map
                               {(await (js/Promise.resolve :a)) (await (js/Promise.resolve 1))
                                :b (await (js/Promise.resolve 2))}})
                            (test-colls)")]
                 (p/let [result v]
                   (is (= {:vector [1 2 3] :set #{1 2} :map {:a 1 :b 2}} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Comprehensive integration test
(deftest async-fn-integration-test
  (testing "^:async fn integration test with multiple features combined"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-complex []
                              ;; All results boxed in vectors to detect unwrapped promises
                              (let [a [(await (js/Promise.resolve 1))]
                                    b [(-> (first a) inc (* 2) (+ (await (js/Promise.resolve 3))))]
                                    c [(if (await (js/Promise.resolve true))
                                         (let [x (await (js/Promise.resolve 10))]
                                           (+ x (first b)))
                                         0)]
                                    d [(loop [i 0 acc 0]
                                         (if (< i 3)
                                           (recur (inc i) (+ acc (await (js/Promise.resolve i))))
                                           acc))]
                                    e [(try
                                         (+ (await (js/Promise.resolve 100))
                                            (throw (js/Error. \"oops\")))
                                         (catch :default err
                                           (await (js/Promise.resolve 42))))]
                                    f [(case (await (js/Promise.resolve :x))
                                         :x (await (js/Promise.resolve :matched-x))
                                         :y :matched-y
                                         :default)]
                                    g [(letfn [(double [n] (* 2 n))]
                                         (let [v (await (js/Promise.resolve 5))]
                                           (double v)))]
                                    h [(await ((^:async fn [x] (+ x (await (js/Promise.resolve 1)))) 10))]
                                    i [((fn [x] (+ x 1)) (await (js/Promise.resolve 100)))]
                                    j [(or (await (js/Promise.resolve nil))
                                           (await (js/Promise.resolve false))
                                           (await (js/Promise.resolve :found)))]
                                    k [(and (await (js/Promise.resolve 1))
                                            (await (js/Promise.resolve 2))
                                            (await (js/Promise.resolve 3)))]
                                    ;; test and short-circuit with falsy
                                    and-atom (atom 0)
                                    l [(and (await (js/Promise.resolve false))
                                            (do (swap! and-atom inc) :never))]
                                    l-side-effects [@and-atom]
                                    ;; test or short-circuit with truthy
                                    or-atom (atom 0)
                                    m [(or (await (js/Promise.resolve :truthy))
                                           (do (swap! or-atom inc) :never))]
                                    m-side-effects [@or-atom]
                                    ;; async fn calling another async fn
                                    n [(let [async-add (^:async fn [x y]
                                                         (+ (await (js/Promise.resolve x))
                                                            (await (js/Promise.resolve y))))
                                             async-mul (^:async fn [x y]
                                                         (* (await (async-add x 1))
                                                            (await (js/Promise.resolve y))))]
                                         (await (async-mul 2 3)))]
                                    ;; nested do blocks
                                    o [(do
                                         (do
                                           (await (js/Promise.resolve :inner))
                                           (do
                                             (await (js/Promise.resolve :deeper))
                                             :nested-result)))]
                                    ;; for comprehension with await
                                    p [(vec (for [x (await (js/Promise.resolve [1 2]))]
                                              (* x 2)))]
                                    ;; set literal with await
                                    q [#{(await (js/Promise.resolve :x))
                                         (await (js/Promise.resolve :y))}]
                                    ;; map literal with await in keys and values
                                    r [{(await (js/Promise.resolve :key)) (await (js/Promise.resolve :val))}]]
                                {:a a :b b :c c :d d :e e :f f :g g :h h :i i :j j :k k
                                 :l l :l-side-effects l-side-effects
                                 :m m :m-side-effects m-side-effects
                                 :n n :o o :p p :q q :r r}))
                            (test-complex)")]
                 (p/let [result v]
                   (is (= {:a [1] :b [7] :c [17] :d [3] :e [42] :f [:matched-x] :g [10] :h [11] :i [101]
                           :j [:found] :k [3]
                           :l [false] :l-side-effects [0]
                           :m [:truthy] :m-side-effects [0]
                           :n [9] :o [:nested-result] :p [[2 4]]
                           :q [#{:x :y}] :r [{:key :val}]} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

;; Comprehensive if edge case tests - regression tests for promise branch normalization
;; Key regression: when one branch has await and the other doesn't, both must return promises
(deftest async-fn-if-edge-cases-test
  (testing "if with await - all edge cases"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-if-cases []
                              {:await-in-test-truthy
                               (if (await (js/Promise.resolve true)) :then :else)

                               :await-in-test-falsy
                               (if (await (js/Promise.resolve false)) :then :else)

                               :await-in-then-only
                               (if true (await (js/Promise.resolve :then-value)) :else-value)

                               :await-in-else-only
                               (if false :then-value (await (js/Promise.resolve :else-value)))

                               ;; REGRESSION: await in else but taking then (was: p.then not a function)
                               :await-in-else-take-then
                               (if true :plain-value (await (js/Promise.resolve :never)))

                               ;; REGRESSION: await in then but taking else (was: p.then not a function)
                               :await-in-then-take-else
                               (if false (await (js/Promise.resolve :never)) :plain-value)

                               :await-both-truthy
                               (if true (await (js/Promise.resolve :then)) (await (js/Promise.resolve :else)))

                               :await-both-falsy
                               (if false (await (js/Promise.resolve :then)) (await (js/Promise.resolve :else)))

                               :no-else-await-in-test
                               (if (await (js/Promise.resolve true)) :result)

                               :no-else-await-in-then
                               (if true (await (js/Promise.resolve :result)))

                               ;; REGRESSION: loop with if having mixed branches
                               :nested-loop-mixed
                               (loop [sum 0 items [{:x 10} 5 {:x 20}]]
                                 (if (seq items)
                                   (let [item (first items)]
                                     (recur (+ sum (if (map? item)
                                                     (:x item)
                                                     (await (js/Promise.resolve item))))
                                            (rest items)))
                                   sum))

                               ;; REGRESSION: if in expression position with mixed branches
                               :expr-position-mixed
                               (+ 100 (if false (await (js/Promise.resolve 1)) 42))})
                            (test-if-cases)")]
                 (p/let [result v]
                   (is (= {:await-in-test-truthy :then
                           :await-in-test-falsy :else
                           :await-in-then-only :then-value
                           :await-in-else-only :else-value
                           :await-in-else-take-then :plain-value
                           :await-in-then-take-else :plain-value
                           :await-both-truthy :then
                           :await-both-falsy :else
                           :no-else-await-in-test :result
                           :no-else-await-in-then :result
                           :nested-loop-mixed 35
                           :expr-position-mixed 142}
                          result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))
