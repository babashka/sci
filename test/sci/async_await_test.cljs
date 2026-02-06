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
