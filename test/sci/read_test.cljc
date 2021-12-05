(ns sci.read-test
  #?(:clj (:require [clojure.test :refer [deftest testing is]]
                    [sci.core :as sci]
                    [sci.test-utils :as tu]
                    [clojure.string :as str])))

#?(:clj
   (do (deftest read-test
         (when-not tu/native?
           (testing "Read works with PushbackReader"
             (let [v (sci/eval-string
                      "(read (java.io.PushbackReader. (java.io.StringReader. \"(+ 1 2 3)\")))"
                      {:classes {'java.io.StringReader
                                 java.io.StringReader
                                 'java.io.PushbackReader
                                 java.io.PushbackReader}})]
               (is (= '(+ 1 2 3) v))
               (is (not (meta v))))
             (let [v (sci/eval-string
                      "(read (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. \"(+ 1 2 3)\")))"
                      {:classes {'java.io.StringReader java.io.StringReader
                                 'clojure.lang.LineNumberingPushbackReader
                                 clojure.lang.LineNumberingPushbackReader}})]
               (is (= '(+ 1 2 3) v))
               (is (meta v))))))

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
         (testing "able to read other than platform feature"
           (is (= [4 5 6]
                  (sci/eval-string
                   "
(def form
  (with-in-str \"#?(:cljs [4 5 6] :clj [1 2 3])\"
    (read {:eof ::eof :read-cond :allow :features [:cljs]} *in*)))
form
"))))
         (testing "always include platform feature"
           (is (= [1 2 3]
                  (sci/eval-string
                   "
(def form
  (with-in-str \"#?(:clj [1 2 3] :cljs [4 5 6])\"
    (read {:eof ::eof :read-cond :allow :features [:cljs]} *in*)))
form
")))))

       (deftest read-eval-test
         (testing "read-eval"
           (is (= [1 2 6]
                  (sci/eval-string "(with-in-str \"[1 2 #=(+ 1 2 3)]\" (read *in*))"))))
         (testing "read-eval disabled"
           (is (= "EvalReader not allowed when *read-eval* is false."
                  (sci/eval-string "
(with-in-str \"[1 2 #=(+ 1 2 3)]\"
  (try (binding [*read-eval* false] (read *in*))
    (catch Exception e (ex-message e))))")))))

       (deftest read-string-test
         (testing "read-eval"
           (is (= [1 2 6]
                  (sci/eval-string "(read-string \"[1 2 #=(+ 1 2 3)]\")"))))
         (testing "eof"
           (is (= :user/eof
                  (sci/eval-string "(read-string {:eof ::eof} \"\")")))))

       (deftest tag-fallback-test
         (testing "js"
           (is (= '[js [1 2 3]]
                  (sci/eval-string "
(binding [*default-data-reader-fn* vector]
  (read-string {:eof ::eof} \"#js [1 2 3]\"))
")))))
       (deftest readder-resolver-test
         (testing "js"
           (is (= '[:s/foo :the-current-ns/foo]
                  (sci/eval-string "
(binding [*reader-resolver* x]
  (read-string \"[::s/foo ::foo]\"))
"
                                   {:namespaces {'user {'x (reify clojure.lang.LispReader$Resolver
                                                             (currentNS [_]
                                                               'the-current-ns)
                                                             (resolveAlias [_ sym] sym))}}})))))))
