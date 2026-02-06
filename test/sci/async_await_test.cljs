(ns sci.async-await-test
  (:require [clojure.test :as test :refer [deftest is testing async]]
            [promesa.core :as p]
            [sci.core :as sci]))

(deftest async-fn-simple-await-test
  (testing "^:async fn with await in let"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (let [x (await (js/Promise.resolve 1))]
                                (inc x)))
                            (foo)")]
                 ;; v should be a Promise that resolves to 2
                 (p/let [result v]
                   (is (= 2 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-sequential-awaits-test
  (testing "^:async fn with sequential awaits"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async bar []
                              (let [x (await (js/Promise.resolve 1))
                                    y (await (js/Promise.resolve 2))]
                                (+ x y)))
                            (bar)")]
                 (p/let [result v]
                   (is (= 3 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-mixed-bindings-test
  (testing "^:async fn with mixed bindings"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async baz []
                              (let [x 1
                                    y (await (js/Promise.resolve 2))
                                    z 3]
                                (+ x y z)))
                            (baz)")]
                 (p/let [result v]
                   (is (= 6 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-nested-let-test
  (testing "^:async fn with nested let forms"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async qux []
                              (let [x (await (js/Promise.resolve 10))]
                                (let [y (await (js/Promise.resolve 11))]
                                  (+ x y))))
                            (qux)")]
                 (p/let [result v]
                   (is (= 21 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-do-test
  (testing "^:async fn with do and await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(def a (atom 0))
                            (defn ^:async quux []
                              (do
                                (reset! a 1)
                                (await (js/Promise.resolve nil))
                                (swap! a inc)
                                @a))
                            (quux)")]
                 (p/let [result v]
                   (is (= 2 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-await-in-expr-test
  (testing "^:async fn with await inside expression"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async calc []
                              (let [x (await (js/Promise.resolve 10))]
                                (let [y (inc (await (js/Promise.resolve 11)))]
                                  (+ x y))))
                            (calc)")]
                 (p/let [result v]
                   (is (= 22 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-await-in-do-binding-test
  (testing "^:async fn with await inside do in binding"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async calc2 []
                              (let [x (await (js/Promise.resolve 10))]
                                (let [y (do (await (js/Promise.resolve 11)))]
                                  (+ x y))))
                            (calc2)")]
                 (p/let [result v]
                   (is (= 21 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-thread-macro-test
  (testing "^:async fn with -> threading macro"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async calc3 []
                              (-> (js/Promise.resolve 10)
                                  (await)
                                  (+ 5)))
                            (calc3)")]
                 (p/let [result v]
                   (is (= 15 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-if-test
  (testing "^:async fn with if and await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v1 (sci/eval-string* ctx
                            "(defn ^:async choose [b]
                               (if (await (js/Promise.resolve b))
                                 (await (js/Promise.resolve 1))
                                 (await (js/Promise.resolve 2))))
                             (choose true)")
                       v2 (sci/eval-string* ctx "(choose false)")]
                 (p/let [r1 v1
                         r2 v2]
                   (is (= 1 r1))
                   (is (= 2 r2))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-try-catch-test
  (testing "^:async fn with try/catch"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       ;; Test catching a rejected promise
                       v1 (sci/eval-string* ctx
                            "(defn ^:async safe-fetch []
                               (try
                                 (await (js/Promise.reject (js/Error. \"oops\")))
                                 (catch :default e
                                   \"caught\")))
                             (safe-fetch)")
                       ;; Test successful try (no catch)
                       v2 (sci/eval-string* ctx
                            "(defn ^:async safe-fetch2 []
                               (try
                                 (await (js/Promise.resolve 42))
                                 (catch :default e
                                   \"caught\")))
                             (safe-fetch2)")]
                 (p/let [r1 v1
                         r2 v2]
                   (is (= "caught" r1))
                   (is (= 42 r2))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-nested-await-test
  (testing "^:async fn with nested await in expression"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (inc (await (inc (await (js/Promise.resolve 1))))))
                            (foo)")]
                 (p/let [result v]
                   (is (= 3 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-local-shadowing-macro-test
  (testing "^:async fn with local binding shadowing macro"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (let [-> (fn [x y] (+ x y))]
                                (-> (await (js/Promise.resolve 1)) 1)))
                            (foo)")]
                 (p/let [result v]
                   (is (= 2 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-nested-async-fn-test
  (testing "^:async fn with nested ^:async fn in let binding"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (let [add-async (^:async fn [x y] (+ (await x) y))]
                                (add-async (js/Promise.resolve 1) 2)))
                            (foo)")]
                 (p/let [result v]
                   (is (= 3 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-param-shadowing-macro-test
  (testing "^:async fn with parameter shadowing macro"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo [->]
                              (-> (await (js/Promise.resolve 1)) 1))
                            (foo (fn [x y] (+ x y)))")]
                 (p/let [result v]
                   (is (= 2 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-no-await-returns-promise-test
  (testing "^:async fn without await still returns a promise"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo [x] (+ x 1))
                            (foo 41)")]
                 (p/let [result v]
                   (is (= 42 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-anonymous-fn-test
  (testing "^:async anonymous fn with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "((^:async fn [] (await (js/Promise.resolve 42))))")]
                 (p/let [result v]
                   (is (= 42 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-macro-expanding-to-await-test
  (testing "^:async fn with macro that expands to await"
    (async done
           (-> (p/let [ctx (sci/init {:namespaces {'user {'my-await ^:sci/macro (fn [_ _ x] (list 'await x))}}
                                      :classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (inc (my-await (js/Promise.resolve 1))))
                            (foo)")]
                 (p/let [result v]
                   (is (= 2 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-destructuring-await-test
  (testing "^:async fn with destructuring in let binding with await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (let [[x y] (await (js/Promise.resolve [1 2]))]
                                (+ x y)))
                            (foo)")]
                 (p/let [result v]
                   (is (= 3 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-doseq-await-test
  (testing "^:async fn with doseq and await (loop/recur)"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (let [a (atom [])]
                                (doseq [x [1 2 3]]
                                  (swap! a conj (await (js/Promise.resolve x))))
                                @a))
                            (foo)")]
                 (p/let [result v]
                   (is (= [1 2 3] result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-loop-recur-await-test
  (testing "^:async fn with direct loop/recur and await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (loop [x 0 acc []]
                                (if (< x 3)
                                  (recur (inc x) (conj acc (await (js/Promise.resolve x))))
                                  acc)))
                            (foo)")]
                 (p/let [result v]
                   (is (= [0 1 2] result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-case-await-in-test-expr-test
  (testing "^:async fn with case and await in test expression"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (case (await (js/Promise.resolve 1))
                                1 :one
                                2 :two))
                            (foo)")]
                 (p/let [result v]
                   (is (= :one result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-case-await-in-match-constant-test
  (testing "^:async fn with case and await-like form in match constant (should not expand)"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           ";; (await 1) as match constant is a literal list, not expanded
                            (defn ^:async foo []
                              (case (await (js/Promise.resolve 'await))
                                (await 1) :matched-await-list
                                2 :two
                                :default))
                            (foo)")]
                 (p/let [result v]
                   (is (= :matched-await-list result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-case-await-in-result-expr-test
  (testing "^:async fn with case and await in result expression"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (case 2
                                1 :one
                                2 (await (js/Promise.resolve :two))))
                            (foo)")]
                 (p/let [result v]
                   (is (= :two result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-case-await-in-default-test
  (testing "^:async fn with case and await in default expression"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (case 999
                                1 :one
                                2 :two
                                (await (js/Promise.resolve :default-value))))
                            (foo)")]
                 (p/let [result v]
                   (is (= :default-value result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-letfn-await-test
  (testing "^:async fn with letfn and await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (letfn [(helper [x] (inc x))]
                                (helper (await (js/Promise.resolve 1)))))
                            (foo)")]
                 (p/let [result v]
                   (is (= 2 result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-letfn-await-in-binding-test
  (testing "^:async fn with letfn containing await used as let binding"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (let [g (letfn [(f [x] x)]
                                        (await (js/Promise.resolve 1)))]
                                [g]))
                            (foo)")]
                 (p/let [result v]
                   (is (= [1] result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-throw-await-test
  (testing "^:async fn with throw and await"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async foo []
                              (try
                                (throw (js/Error. (await (js/Promise.resolve \"err\"))))
                                (catch :default e
                                  (.-message e))))
                            (foo)")]
                 (p/let [result v]
                   (is (= "err" result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))

(deftest async-fn-integration-test
  (testing "^:async fn integration test with multiple features combined"
    (async done
           (-> (p/let [ctx (sci/init {:classes {'js js/globalThis :allow :all}})
                       v (sci/eval-string* ctx
                           "(defn ^:async test-complex []
                              (let [a (await (js/Promise.resolve 1))
                                    b (-> a inc (* 2) (+ (await (js/Promise.resolve 3))))
                                    c (if (await (js/Promise.resolve true))
                                        (let [x (await (js/Promise.resolve 10))]
                                          (+ x b))
                                        0)
                                    d (loop [i 0 acc 0]
                                        (if (< i 3)
                                          (recur (inc i) (+ acc (await (js/Promise.resolve i))))
                                          acc))
                                    e (try
                                        (+ (await (js/Promise.resolve 100))
                                           (throw (js/Error. \"oops\")))
                                        (catch :default err
                                          (await (js/Promise.resolve 42))))
                                    f (case (await (js/Promise.resolve :x))
                                        :x (await (js/Promise.resolve :matched-x))
                                        :y :matched-y
                                        :default)
                                    g (letfn [(double [n] (* 2 n))]
                                        (let [v (await (js/Promise.resolve 5))]
                                          (double v)))
                                    h (await ((^:async fn [x] (+ x (await (js/Promise.resolve 1)))) 10))
                                    i ((fn [x] (+ x 1)) (await (js/Promise.resolve 100)))]
                                {:a a :b b :c c :d d :e e :f f :g g :h h :i i}))
                            (test-complex)")]
                 (p/let [result v]
                   (is (= {:a 1 :b 7 :c 17 :d 3 :e 42 :f :matched-x :g 10 :h 11 :i 101} result))))
               (p/catch (fn [err]
                          (is false (str err))))
               (p/finally done)))))
