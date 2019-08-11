(ns sci.core-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.test-utils :as tu]))

(defn eval* [binding form]
  (tu/eval* form {'*in* binding}))

(deftest core-test
  (testing "if and when"
    (is (= 1 (eval* 0 '(if (zero? *in*) 1 2))))
    (is (= 2 (eval* 1 '(if (zero? *in*) 1 2))))
    (is (= 1 (eval* 0 '(when (zero? *in*) 1))))
    (is (nil? (eval* 1 '(when (zero? *in*) 1)))))
  (testing "and and or"
    (is (= false (eval* 0 '(and false true *in*))))
    (is (= 0 (eval* 0 '(and true true *in*))))
    (is (= 1 (eval* 1 '(or false false *in*))))
    (is (= false (eval* false '(or false false *in*))))
    (is (= 3 (eval* false '(or false false *in* 3)))))
  (testing "fn"
    (is (= 2 (eval* 1 "(#(+ 1 %) *in*)")))
    (is (= [1 2 3] (eval* 1 "(map #(+ 1 %) [0 1 2])")))
    (is (eval* 1 "(#(when (odd? *in*) *in*) 1)")))
  (testing "map"
    (is (= [1 2 3] (eval* 1 '(map inc [0 1 2])))))
  (testing "keep"
    (is (= [false true false] (eval* 1 '(keep odd? [0 1 2])))))
  (testing "->"
    (is (= 4 (eval* 1 '(-> *in* inc inc (inc))))))
  (testing "->>"
    (is (= 7 (eval* ["foo" "baaar" "baaaaaz"] "(->> *in* (map count) (apply max))"))))
  (testing "literals"
    (is (= {:a 4
            :b {:a 2}
            :c [1 1]
            :d #{1 2}}
           (eval* 1 '{:a (+ 1 2 *in*)
                   :b {:a (inc *in*)}
                   :c [*in* *in*]
                      :d #{*in* (inc *in*)}}))))
  (testing "quoting"
    (is (= {:a '*in*} (eval* 1 (str "'{:a *in*}"))))
    (is (= '#{1 2 3 *in*} (eval* 4 "'#{1 2 3 *in*}")))
    (is (= '[1 2 3 *in*] (eval* 4 "'[1 2 3 *in*]")))
    (is (= '(1 2 3 *in*) (eval* 4 "'(1 2 3 *in*)"))))
  (testing "calling ifns"
    (is (= 3 (eval* nil '({:a 1} 2 3))))
    (is (= 1 (eval* nil '({:a 1} :a 3))))
    (is (= 3 (eval* nil '((hash-map :a 1) 2 3))))
    (is (= 1 (eval* nil '((hash-map :a 1) :a 3))))
    (is (= :a (eval* nil '(#{:a :b :c} :a)))))
  (testing "cannot call x as a function"
    (doseq [example ['(1 2 3) '("foo" 2 3)]]
      (if (not tu/native?)
        (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"call.*function"
                              (eval* nil example)))
        (is (re-find #"call.*function" (:stderr (eval* nil example))))))))

;;;; Scratch

(comment
  (eval* 1 '(inc *in*)))
