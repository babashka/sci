(ns sci.safety-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   [sci.core :as sci :refer [eval-string]]
   [sci.test-utils :as tu]))

(deftest iterate-max-test
  (when-not tu/native?
    (let [d (try (tu/eval* "(loop [i 1000] (if (zero? i) :done (recur (dec i))))" {:iterate-max 100})
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                   (ex-data e)))]
      (is (= :sci.error/iterated-beyond-max (:type d)))))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"iteration"
                        (tu/eval* "(reduce (fn [_ _]) (range 1000))" {:iterate-max 100})))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"iteration"
                        (tu/eval* "((fn pow [x n] (case n 1 x, (* x (pow x (dec n))))) 2 10)" {:iterate-max 10})))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"iteration"
                        (tu/eval*  "(loop [i 1000] (if (zero? i) :done (recur (dec i))))" {:iterate-max 100})))

  (is (= :done (tu/eval*  "(loop [i 10] (if (zero? i) :done (recur (dec i))))" {:iterate-max 100})))
  (is (= :done (tu/eval*  "(loop [i 1000] (if (zero? i) :done (recur (dec i))))" {})))

  (is (= 1024 (tu/eval* "((fn pow [x n] (case n 1 x, (* x (pow x (dec n))))) 2 10)" {:iterate 100})))
  (is (= 1024 (tu/eval* "((fn pow [x n] (case n 1 x, (* x (pow x (dec n))))) 2 10)" {}))))
