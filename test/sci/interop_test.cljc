(ns sci.interop-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))


(defn with-string-class [expr]
  (tu/eval* expr {:classes {'java.lang.String {:class String
                                               :deny-methods '[getClass]}}
                  :imports {'String 'java.lang.String}}))

(deftest instance-methods
  (is (= 3 (with-string-class "(.length \"foo\")")))
  (is (= "java.lang.String" (with-string-class "(-> \"foo\" .getClass .getName)")))
  (is (= \o (with-string-class "(def x 0) (.charAt \"foo\" (inc x))"))))


#_{:classes {'java.lang.String {:class String
                              :deny-methods '[getClass]}}
 :imports {'String 'java.lang.String}}
