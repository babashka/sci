(ns sci.interop-test
  (:require
   #?(:clj [clojure.test :as test :refer [deftest is]])
   [sci.test-utils :as tu]))

(defn with-string-class [expr]
  (tu/eval* expr #?(:clj {:classes {'java.lang.String String}
                          :imports {'String 'java.lang.String}}
                    :cljs {})))

#?(:clj
   (deftest instance-methods
     (is (= 3 (with-string-class "(.length \"foo\")")))
     (is (thrown-with-msg? Exception #"getName.*Class.*allowed"
          (with-string-class "(-> \"foo\" .getClass .getName)")))
     (is (= \o (with-string-class "(def x 0) (.charAt \"foo\" (inc x))")))))
