(ns sci.error-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [sci.core :as sci :refer [eval-string]]
            [sci.impl.callstack :as cs]
            [sci.impl.namespaces :refer [sci-ns-name]]))

(deftest callstack-test
  (let [stacktrace
        (try (eval-string "
(defn bar [] (subs nil 0))
(defn foo [] (bar))
(foo)"
                          )
             (catch #?(:clj Exception
                       :cljs js/Error) e
               (map #(-> %
                         (select-keys [:ns :name :line :column])
                         (update :ns sci-ns-name))
                    (cs/stacktrace (:sci.impl/callstack (ex-data e))))))]
    #_(doseq [st stacktrace]
      (prn st))
    (is (= '({:ns clojure.core, :name subs}
             {:ns user, :name bar, :line 2, :column 14}
             {:ns user, :name bar, :line 2, :column 1}
             {:ns user, :name foo, :line 3, :column 14}
             {:ns user, :name foo, :line 3, :column 1}
             {:ns user, :name nil, :line 4, :column 1})
           stacktrace))))

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
