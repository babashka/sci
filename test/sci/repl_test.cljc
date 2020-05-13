(ns sci.repl-test
  (:require [clojure.string :as str]
            [clojure.test :as t :refer [deftest is]]
            [sci.core :as sci]
            [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:bindings {'*in* binding}})))

(deftest repl-doc-test
  (when-not tu/native?
    (is (= (str/trim "
-------------------------
user/f
([x] [x y])
Macro
  foodoc")
           (str/trim (sci/with-out-str (eval* "(defmacro f \"foodoc\" ([x]) ([x y])) (clojure.repl/doc f)")))))
    (is (= (str/trim  #?(:clj "
-------------------------
clojure.core/inc
([x])
  Returns a number one greater than num. Does not auto-promote
  longs, will throw on overflow. See also: inc'"
                         :cljs "
-------------------------
clojure.core/inc
([x])
  Returns a number one greater than num."))
           (str/trim (sci/with-out-str (eval* "(clojure.repl/doc inc)")))))
    (is (= (str/trim
            "-------------------------\nfoo\n  foodoc\n")
           (str/trim (sci/with-out-str (eval* "(ns foo \"foodoc\") (clojure.repl/doc foo)")))))
    (is (empty? (sci/with-out-str (tu/eval* "(clojure.repl/doc x)" {:bindings {'x 1}}))))))

(deftest repl-find-doc-test
  (when-not tu/native?
    (is (= (str/trim "
-------------------------
foo-ns/foo-fun
([x] [x y])
Macro
  foodoc
-------------------------
foo-ns/foo-macro
([x] [x y])
Macro
  foodoc
-------------------------
foo-ns
  foodoc")
           (str/trim (sci/with-out-str (eval* "
(ns foo-ns \"foodoc\")
(defmacro foo-fun \"foodoc\" ([x]) ([x y]))
(defmacro foo-macro \"foodoc\" ([x]) ([x y]))

(clojure.repl/find-doc #\"foodoc\")")))))))

(deftest repl-dir-test
  (when-not tu/native?
    (let [output (sci/with-out-str (eval* "(require '[clojure.repl :refer [dir]]) (dir clojure.string)"))]
      (is (str/includes? output "includes?"))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"No namespace.*found "
                        (eval* "(require '[clojure.repl :refer [dir]]) (dir clojure.typo)"))))

(deftest repl-apropos-test
  (let [symbols (set (eval* "(require '[clojure.repl :refer [apropos]]) (apropos \"inc\")"))]
    (is (contains? symbols 'clojure.core/inc))
    (is (contains? symbols 'clojure.string/includes?))))

(deftest repl-pst-test
  #?(:clj
     (when-not tu/native?
       (let [sw (java.io.StringWriter.)]
         (sci/binding [sci/err sw]
           (eval* "(try (/ 1 0) (catch Exception e (clojure.repl/pst e 2)))"))
         (is (str/includes? (str sw) "Divide by zero")))
       (let [sw (java.io.StringWriter.)]
         (sci/binding [sci/err sw]
           (eval* "(try (/ 1 0) (catch Exception e (clojure.repl/pst e 2)))"))
         (is (str/includes? (str/trim (str sw)) (str/trim "
ArithmeticException Divide by zero
\tclojure.lang.Numbers.divide")))))))
