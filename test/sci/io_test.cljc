(ns sci.io-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is]]
   [sci.core :as sci]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest with-sci-out-str-test
  (when-not tu/native?
    (is (= "hello\n" (sci/with-out-str (eval* "(println \"hello\")"))))))

#?(:clj
   (deftest with-sci-in-str-test
     (when-not tu/native?
       (is (= "hello" (sci/with-in-str "hello\n" (eval* "(read-line)")))))))

(deftest with-out-str-test
  (is (= "hello\n" (eval* "(with-out-str (println \"hello\"))"))))

#?(:clj
   (deftest with-in-str-test
     (is (= "hello" (eval* "(with-in-str \"hello\" (read-line))")))))

(deftest print-test
  (when-not tu/native?
    (is (= "hello\n" (sci/with-out-str (eval* "(println \"hello\")"))))
    (is (= "hello" (sci/with-out-str (eval* "(print \"hello\")"))))
    (is (= "\"hello\"\n" (sci/with-out-str (eval* "(prn \"hello\")"))))
    (is (= "\"hello\"" (sci/with-out-str (eval* "(pr \"hello\")"))))
    (is (= "\n" (sci/with-out-str (eval* "(newline)"))))))

(deftest print-length-test
  (when-not tu/native?
    (is (str/includes?
         (sci/with-out-str (eval* "(binding [*print-length* 10] (println (range)))"))
         "1 2 3"))
    (is (str/includes?
         (sci/with-out-str
           (sci/binding [sci/print-length 10]
             (eval* "(println (range))")))
         "1 2 3"))))

(deftest print-meta-test
  (is (= true (eval* "(:a (meta (clojure.edn/read-string (binding [*print-meta* true] (pr-str ^:a [])))))")))
  (is (= true (eval* "(:a (meta (clojure.edn/read-string (binding [*print-meta* true] (prn-str ^:a [])))))")))
  #?(:cljs
     ;; for some reason this doesn't work in JVM Clojure
     (is (= true (eval* "(:a (meta (clojure.edn/read-string (binding [*print-meta* true] (print-str ^:a [])))))"))))
  (when-not tu/native?
    (is (= true (-> (sci/with-out-str (eval* "(binding [*print-meta* true] (prn ^:a []))"))
                    (edn/read-string)
                    meta
                    :a)))))
