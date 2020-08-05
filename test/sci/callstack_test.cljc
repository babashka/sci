(ns sci.callstack-test
  (:require [sci.core :as sci :refer [eval-string]]
            [sci.test-utils :as tu]
            [sci.impl.vars :refer [callstack]]
            [clojure.test :as t :refer [deftest]]))

(defn print-stacktrace [callstack]
  (doseq [v callstack]
    (prn (meta v))))


(deftest callstack-test
  (try (eval-string "
(defn bar [] (/ 1 0))
(defn foo [] (bar))
(foo)"
                    )
       (catch Exception e
         (print-stacktrace (:callstack (ex-data e))))))

