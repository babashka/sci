(ns sci.error-test
  (:require #?(:clj [sci.addons.future :as fut])
            [clojure.test :as t :refer [deftest testing is]]
            [sci.core :as sci :refer [eval-string]]))

#?(:cljs (def Exception js/Error))

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
    (is (= '({:ns user, :name nil, :line 1, :column 1}) stacktrace)))
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
      (is (= '({:ns user, :name g, :file nil}
               {:ns user, :file nil, :line 1, :column 27, :name g}
               {:ns user, :name g, :file nil, :line 1, :column 15}
               {:ns user, :file nil, :line 1, :column 34, :name nil})
             stacktrace))
      (let [formatted (sci/format-stacktrace stacktrace)]
        (is (= '("user/g - <expr>:1:27"
                 "user/g - <expr>:1:15"
                 "user   - <expr>:1:34")
               formatted))))))

#_(deftest locals-test
    (testing "defn does not introduce fn-named local binding"
      (let [locals
            (try (eval-string "(defn foo [x] (subs nil 0)) (foo :x)")
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
                   (:locals (ex-data e))))
            ks (keys locals)]
        (is (= '[x] ks)))))

#?(:clj (deftest arity-error-test
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

(main)"))))))

#?(:clj
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
              (eval-string "(apply (fn [_ & xs]) [])")))))))

(deftest inherited-ex-data-is-encapsulated
  (testing "The original ex-data is encapsulated."
    (is (= [{:column 22
             :file nil
             :line 2
             :message "ex-message"
             :type :sci/error}
            {:column 3}]
           (try
             (eval-string "
(defn throwing-fn [] (throw (ex-info \"ex-message\" {:column 3})))

(throwing-fn)")
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
               [(dissoc (ex-data e) :sci.impl/callstack :locals)
                (ex-data #?(:clj (.getCause e)
                            :cljs (ex-cause e)))]))))))

(deftest implicit-do-error-test
  (let [expected {:type :sci/error
                  :line 1
                  :column 1
                  :message "Assert failed: false"}
        try-string #(try
                      (eval-string %)
                      (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
                        (dissoc (ex-data e) :sci.impl/callstack :file)))]
    (testing "top level try with implicit do wraps exception"
      (is (= expected
             (try-string "(try 1 (assert false))"))))
    (testing "top level let with implicit do wraps exception"
      (is (= expected
             (try-string "(let [] 1 (assert false))")))
      ; macroexpands to a let
      (is (= expected
             (try-string "(binding [] (assert false) 1)"))))))

(deftest analysis-error-test
  (is
   (= ["user - <expr>:1:1"]
      (sci/format-stacktrace
       (sci/stacktrace (try (sci/eval-string "(def n x)") (catch #?(:clj Exception :cljs js/Error) e e)))))))

(deftest try-finally-test
  (is
   (= ["clojure.core/assoc - <built-in>" "user/foo           - <expr>:1:14" "user/foo           - <expr>:1:1" "user               - <expr>:1:84" "user               - <expr>:1:65"]
      (sci/format-stacktrace
       (sci/stacktrace (try (sci/eval-string "(defn foo [] (assoc :foo :bar :baz)) (def ^:dynamic *foo* nil ) (binding [*foo* 3] (foo))") (catch #?(:clj Exception :cljs js/Error) e e)))))))

(deftest macroexpansion-with-unresolved-symbol-has-location-test
  (let [data (try (sci/eval-string "(defmacro dude [& body] `(do ~@body)) (dude x)") (catch Exception e (ex-data e)))]
    (is (:line data))
    (is (:column data))))

#?(:clj
   (deftest preserve-exception-type-in-threads-test
     (is (= java.lang.IllegalArgumentException
            (sci/eval-string "
(def f (future (throw (IllegalArgumentException. \"meh\"))))
(try @f
  (catch Throwable e
    (type (ex-cause e))))"
                             (-> {:classes {'IllegalArgumentException IllegalArgumentException
                                            'Throwable Throwable}}
                                 (fut/install)))))))

#?(:clj
   (deftest thread-test
     (let [invoke-ex-fn
           (fn [f]
             (try
               (f)
               (catch Throwable e
                 (ex-data e))))]
       (let [f (sci.core/eval-string "(fn [] (try (/ 1 0) (catch ^:sci/error Exception e (throw e))))")]
         (is (let [res @(future (invoke-ex-fn f))]
               (is (= {:line 1 :column 13} (select-keys res [:line :column])))))))))

(deftest let-test
  (doseq [[snippet [line col]]
          [["(str (let [[a] 1] a))"       [1 6]]
           ["(str (for [[a] [0]] :foo))"  [1 6]]
           ["(str (for [[a] 1] (/ 1 a)))" [1 6]]]]
    (try
      (sci.core/eval-string snippet)
      (is false)
      (catch Exception e
        (is (= [line col] ((juxt :line :column) (ex-data e))))))))
