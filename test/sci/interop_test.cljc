(ns sci.interop-test
  (:require
   #?(:clj [clojure.test :as test :refer [deftest is]])
   [sci.test-utils :as tu]))

(defn with-string-class [expr]
  (tu/eval* expr {:classes {'java.lang.String {:class String
                                               :deny-methods '[getClass]}}
                  :imports {'String 'java.lang.String}}))
#?(:clj
   (deftest instance-methods
     (is (= 3 (with-string-class "(.length \"foo\")")))
     (is (= "java.lang.String" (with-string-class "(-> \"foo\" .getClass .getName)")))
     (is (= \o (with-string-class "(def x 0) (.charAt \"foo\" (inc x))")))))


#_{:classes {'java.lang.String {:class String
                              :deny-methods '[getClass]}}
 :imports {'String 'java.lang.String}}
