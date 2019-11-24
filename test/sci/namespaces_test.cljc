(ns sci.namespaces-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest autoresolve-test
  (is (= :user/foo (eval* "::foo"))))
