(ns sci.error-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [sci.core :as sci :refer [eval-string]]))

(deftest stacktrace-test
  (let [stacktrace
        (try (eval-string "
(defn bar [] (subs nil 0))
(defn foo [] (bar))
(foo)")
             (catch #?(:clj Exception
                       :cljs js/Error) e
               (map #(-> %
                         (select-keys [:ns :name :line :column]))
                    (sci/stacktrace e))))
        expected '({:ns clojure.core, :name subs}
                   {:ns user, :name bar, :line 2, :column 14}
                   {:ns user, :name bar, :line 2, :column 1}
                   {:ns user, :name foo, :line 3, :column 14}
                   {:ns user, :name foo, :line 3, :column 1}
                   {:ns user, :name nil, :line 4, :column 1})]
    #_#_(println "-- ^ expected --- , actual")
    (doseq [st stacktrace]
      (prn st))
    (is (= expected
           stacktrace)))
  (let [stacktrace (try (eval-string "(1 2 3)")
                        (catch #?(:clj Exception
                                  :cljs js/Error) e
                          (map #(-> %
                                    (select-keys [:ns :name :line :column]))
                               (sci/stacktrace e))))]
    (is (= '({:ns user, :name nil, :line 1, :column 1}) stacktrace )))
  (testing "unresolved class in import"
    (let [stacktrace (try (eval-string "(ns foo (:import [java.io FooBar]))")
                          (catch #?(:clj Exception
                                    :cljs js/Error) e
                            (map #(-> %
                                      (select-keys [:ns :name :line :column]))
                                 (sci/stacktrace e))))]
      (is (= '({:ns foo, :name nil, :line 1, :column 9}) stacktrace))))
  (testing "local"
    (let [stacktrace (try (eval-string "(defn foo []) (defn g [x] (x 1)) (g (foo))")
                          (catch #?(:clj Exception
                                    :cljs :default) e
                            (sci/stacktrace e)))]
      (is (= '({:ns user, :line 1, :column 24, :name g, :file nil}
               {:ns user, :file nil, :line 1, :column 27, :name g}
               {:ns user, :name g, :file nil, :line 1, :column 15}
               {:ns user, :file nil, :line 1, :column 34, :name nil})
             stacktrace))
      (let [formatted (sci/format-stacktrace stacktrace)]
        (is (= '("user/g - <expr>:1:24"
                 "user/g - <expr>:1:27"
                 "user/g - <expr>:1:15"
                 "user   - <expr>:1:34")
               formatted))))))

(deftest locals-test
  (testing "defn does not introduce fn-named local binding"
    (let [locals
          (try (eval-string "(defn foo [x] (subs nil 0)) (foo :x)")
               (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
                 (:locals (ex-data e))))
          ks (keys locals)]
      (is (= '[x] ks)))))

(deftest arity-error-test
  (testing "The name of the correct function is reported"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"Wrong number of args \(0\) passed to: foo/echo-msg"
         (eval-string "
(ns foo)

(defn echo-msg [msg]
  msg)

(ns bar (:require foo))

(defn main []
  (foo/echo-msg))  ;; called with the wrong arity

(main)")))))

(deftest arity-error-hof-test
  (testing "apply is not reported when higher order argument causes arity error"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"Wrong number of args \(1\) passed to: function of arity 0"
         (eval-string "(apply (fn []) [1])")))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"Wrong number of args \(3\) passed to: function of arity 1"
         (eval-string "(apply (fn [_]) [1 2 3])")))
    (testing "varargs"
      (is (thrown-with-msg?
           #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
           #"Wrong number of args \(0\) passed to: function of arity 1"
           (eval-string "(apply (fn [_ & xs]) [])"))))))

(deftest inherited-ex-data-is-encapsulated
  (testing "The original ex-data is encapsulated."
    (is (= [{:column 22
             :file nil
             :line 2
             :locals {}
             :message "ex-message"
             :type :sci/error}
            {:column 3}]
           (try
             (eval-string "
(defn throwing-fn [] (throw (ex-info \"ex-message\" {:column 3})))

(throwing-fn)")
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
               [(dissoc (ex-data e) :sci.impl/callstack)
                (ex-data #?(:clj (.getCause e)
                            :cljs (ex-cause e)))]))))))
