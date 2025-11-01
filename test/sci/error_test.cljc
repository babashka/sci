(ns sci.error-test
  (:require #?@(:cljs [] :default [[sci.addons.future :as fut]])
            #?(:cljs [clojure.string :as str])
            [clojure.test :as t :refer [deftest testing is]]
            [sci.core :as sci :refer [eval-string]]))

#?(:cljs (def Exception js/Error))

(deftest stacktrace-test
  (let [stacktrace
        (try (eval-string "
(defn bar [] (subs nil 0))
(defn foo [] (bar))
(foo)")
             (catch #?(:cljs js/Error
                       :default Exception) e
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
                        (catch #?(:cljs js/Error
                                  :default Exception) e
                          (map #(-> %
                                    (select-keys [:ns :name :line :column]))
                               (sci/stacktrace e))))]
    (is (= '({:ns user, :name nil, :line 1, :column 1}) stacktrace)))
  (testing "unresolved class in import"
    (let [stacktrace (try (eval-string "(ns foo (:import [java.io FooBar]))")
                          (catch #?(:cljs js/Error
                                    :default Exception) e
                            (map #(-> %
                                      (select-keys [:ns :name :line :column]))
                                 (sci/stacktrace e))))]
      (is (= '({:ns foo, :name nil, :line 1, :column 9}) stacktrace))))
  (testing "local"
    (let [stacktrace (try (eval-string "(defn foo []) (defn g [x] (x 1)) (g (foo))")
                          (catch #?(:cljs :default
                                    :default Exception) e
                            (sci/stacktrace e)))]
      (is (= '({:ns user, :name g, :file nil}
               {:ns user, :file nil, :line 1, :column 27, :name g}
               {:ns user, :name g, :file nil, :line 1, :column 15}
               {:ns user, :file nil, :line 1, :column 34, :name nil})
             stacktrace))
      (let [formatted (sci/format-stacktrace stacktrace)]
        (is (= '("user/g - NO_SOURCE_PATH:1:27"
                 "user/g - NO_SOURCE_PATH:1:15"
                 "user   - NO_SOURCE_PATH:1:34")
               formatted))))))

#_(deftest locals-test
    (testing "defn does not introduce fn-named local binding"
      (let [locals
            (try (eval-string "(defn foo [x] (subs nil 0)) (foo :x)")
                 (catch #?(:cljs cljs.core/ExceptionInfo :default clojure.lang.ExceptionInfo) e
                   (:locals (ex-data e))))
            ks (keys locals)]
        (is (= '[x] ks)))))

#?(:cljs nil :default
   (deftest arity-error-test
     (testing "The name of the correct function is reported"
       (is (thrown-with-msg?
            #?(:cljs cljs.core/ExceptionInfo :default clojure.lang.ExceptionInfo)
            #"Wrong number of args \(0\) passed to: foo/echo-msg"
            (eval-string "
(ns foo)

(defn echo-msg [msg]
  msg)

(ns bar (:require foo))

(defn main []
  (foo/echo-msg))  ;; called with the wrong arity

(main)"))))))

#?(:cljs nil :default
   (deftest arity-error-hof-test
     (testing "apply is not reported when higher order argument causes arity error"
       (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"Wrong number of args \(1\) passed to: function of arity 0"
            (eval-string "(apply (fn []) [1])")))
       (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"Wrong number of args \(3\) passed to: function of arity 1"
            (eval-string "(apply (fn [_]) [1 2 3])")))
       (testing "varargs"
         (is (thrown-with-msg?
              clojure.lang.ExceptionInfo
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
             (catch #?(:cljs cljs.core/ExceptionInfo :default clojure.lang.ExceptionInfo) e
               [(dissoc (ex-data e) :sci.impl/callstack :locals)
                (ex-data #?(:clj (.getCause e)
                            :default (ex-cause e)))]))))))

(deftest implicit-do-error-test
  (let [expected {:type :sci/error
                  :line 1
                  :column 1
                  :message "Assert failed: false"}
        try-string #(try
                      (eval-string %)
                      (catch #?(:cljs cljs.core/ExceptionInfo :default clojure.lang.ExceptionInfo) e
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
   (= ["user - NO_SOURCE_PATH:1:1"]
      (sci/format-stacktrace
       (sci/stacktrace (try (sci/eval-string "(def n x)") (catch #?(:cljs js/Error :default Exception) e e)))))))

(deftest try-finally-test
  (is
   (= ["clojure.core/assoc - <built-in>" "user/foo           - NO_SOURCE_PATH:1:14" "user/foo           - NO_SOURCE_PATH:1:1" "user               - NO_SOURCE_PATH:1:84" "user               - NO_SOURCE_PATH:1:65"]
      (sci/format-stacktrace
       (sci/stacktrace (try (sci/eval-string "(defn foo [] (assoc :foo :bar :baz)) (def ^:dynamic *foo* nil ) (binding [*foo* 3] (foo))") (catch #?(:cljs js/Error :default Exception) e e)))))))

(deftest macroexpansion-with-unresolved-symbol-has-location-test
  (let [data (try (sci/eval-string "(defmacro dude [& body] `(do ~@body)) (dude x)") (catch Exception e (ex-data e)))]
    (is (:line data))
    (is (:column data))))

#?(:clj
   (deftest preserve-exception-type-in-threads-test
     (is (= java.lang.IllegalArgumentException
            (sci/eval-string
             "(def f (future (throw (IllegalArgumentException. \"meh\"))))
              (try @f
                (catch Throwable e
                  (type (ex-cause e))))"
             (-> {:classes {'IllegalArgumentException IllegalArgumentException
                        'Throwable Throwable}}
                 (fut/install))))))
   :cljr
   (deftest preserve-exception-type-in-threads-test
     (is (= System.ArgumentException
            (sci/eval-string
             "(def f (future (throw (System.ArgumentException. \"meh\"))))
              (try @f
                (catch System.Exception e
                  (type (ex-cause e))))"
             (-> {:classes {'System.ArgumentException System.ArgumentException
                            'System.Exception System.Exception}}
                 (fut/install)))))))


(deftest destructure-test
  (doseq [[snippet [line col]]
          [["(str (let [[a] 1] a))"        [1 6]]
           ["(str (for [[a] [0]] :foo))"   [1 6]]
           ["(str (for [[a] 1] (/ 1 a)))"  [1 6]]
           ["(str (map (fn [[a]] a) [0]))" [1 11]]
           ["(str (if-let [[a] 0] a))"     [1 6]]
           ["(str (when-let [[a] 0] a))"   [1 6]]
           ["(str (if-some [[a] 0] a))"    [1 6]]
           ["(str (when-some [[a] 0] a))"  [1 6]]
           ["(str (doseq [a 0] a))"        [1 6]]
           ["(str (doseq [[a] [0]] a))"    [1 6]]]]
    (try
      (sci.core/eval-string snippet)
      (is false)
      (catch Exception e
        (is (= [line col] ((juxt :line :column) (ex-data e))) snippet)))))

#?(:cljs
   (deftest js-interop-test
     (is (str/includes?
          (->> (try (sci/eval-string "(defn broken [] [:dude (js/is_sure) :foo]) (broken)"
                                     {:classes {'js js/globalThis :allow :all}}) (catch js/Error e (prn (ex-message e)) e))
               (sci/stacktrace) (sci/format-stacktrace) str)
          "1:24"))))
