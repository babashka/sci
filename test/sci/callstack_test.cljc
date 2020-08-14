(ns sci.callstack-test
  (:require [clojure.test :as t :refer [deftest]]
            [sci.core :as sci :refer [eval-string]]))

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

