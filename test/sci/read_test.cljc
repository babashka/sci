(ns sci.read-test
  #?(:clj (:require [clojure.test :refer [deftest testing is]]
                    [sci.core :as sci]
                    [sci.test-utils :as tu])))

#?(:clj
   (do (deftest read-test
         (when-not tu/native?
           (testing "Read works with PushbackReader"
             (is (= '(+ 1 2 3)
                    (sci/eval-string
                     "(read (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. \"(+ 1 2 3)\")))"
                     {:classes {'java.io.StringReader java.io.StringReader
                                'clojure.lang.LineNumberingPushbackReader
                                clojure.lang.LineNumberingPushbackReader}}))))))

       (deftest read-cond-preserve-test
         (when-not tu/native?
           (testing "read can return ReaderConditional"
             (is (= '(:clj [1 2 3] :cljs [4 5 6])
                    (sci/binding [sci/in *in*]
                      (sci/eval-string
                       "
(def form (with-in-str \"#?(:clj [1 2 3] :cljs [4 5 6])\" (read {:read-cond :preserve} *in*)))
(and (instance? clojure.lang.ReaderConditional form)
     (:form form))
"
                       {:classes {'clojure.lang.ReaderConditional
                                  clojure.lang.ReaderConditional}})))))))
       (deftest platform-feature
         (when-not tu/native?
           (testing "able to read other than platform feature"
             (is (= [4 5 6]
                    (sci/binding [sci/in *in*]
                      (sci/eval-string
                       "
(def form
  (with-in-str \"#?(:cljs [4 5 6] :clj [1 2 3])\"
    (read {:eof ::eof :read-cond :allow :features [:cljs]} *in*)))
form
")))))
           (testing "always include platform feature"
             (is (= [1 2 3]
                    (sci/binding [sci/in *in*]
                      (sci/eval-string
                       "
(def form
  (with-in-str \"#?(:clj [1 2 3] :cljs [4 5 6])\"
    (read {:eof ::eof :read-cond :allow :features [:cljs]} *in*)))
form
")))))))

       (deftest eof-test
         (when-not tu/native?
           (testing "eof"
             (is (= :user/eof
                    (sci/binding [sci/in *in*]
                      (sci/eval-string
                       "(with-in-str \"\" (read {:eof ::eof} *in*))")))))))))
