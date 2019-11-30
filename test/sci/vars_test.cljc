(ns sci.vars-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   [sci.test-utils :as tu]
   [sci.opts :as opts]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

#_(deftest foo
  (println (opts/with-dynamic-var {} foo 10)))

(deftest dynamic-var-test
  (is (= [0 1 2 0] (eval*
                    "(def a (atom []))
                     (defn add! [v] (swap! a conj v))
                     (def ^:dynamic x 0)
                     (add! x)
                     (binding [x 1]
                       (add! x)
                       (set! x (inc x))
                       (add! x))
                       (add! x)
                     @a"))))

(deftest with-redefs-test
  (is (= [10 11 10]
         (eval* "(def a (atom []))
                 (defn add! [v] (swap! a conj v))
                 (def ^:dynamic x 10)
                 (add! x)
                 (with-redefs [x 11] (add! x)) (add! x)"))))
