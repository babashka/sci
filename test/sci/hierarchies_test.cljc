(ns sci.hierarchies-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [sci.test-utils :as tu]))

(defn eval* [expr]
  (tu/eval* expr {}))

(deftest derive-test
  (is (true? (eval* "(derive ::foo ::bar) (isa? ::foo ::bar)")))
  (testing "hierarchies are derived per sci session"
    (is (false? (eval* "(isa? ::foo ::bar)")))))

(deftest descendants-test
  (is (= #{:user/foo :user/baz}
         (eval* "(derive ::foo ::bar) (derive ::baz ::bar) (descendants ::bar)"))))

(deftest ancestors-test
  (is (= #{:user/bar :user/baz}
         (eval* "(derive ::foo ::bar) (derive ::bar ::baz) (ancestors ::foo)"))))

(deftest parents-test
  (is (= #{:user/bar}
         (eval* "(derive ::foo ::bar) (parents ::foo)"))))

(deftest underive-test
  (is (empty? (eval* "(derive ::foo ::bar) (underive ::foo ::bar) (parents ::foo)"))))
