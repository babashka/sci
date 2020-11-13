(ns sci.read-test
  #?(:clj (:require [clojure.test :refer [deftest testing is]]
                    [sci.core :as sci]
                    [sci.test-utils :as tu])))

#?(:clj
   (deftest read-test
     (when-not tu/native?
       (testing "Read works with PushbackReader"
         (is (= '(+ 1 2 3)
                (sci/eval-string
                 "(read (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. \"(+ 1 2 3)\")))"
                                 {:classes {'java.io.StringReader java.io.StringReader
                                            'clojure.lang.LineNumberingPushbackReader
                                            clojure.lang.LineNumberingPushbackReader}})))))))
