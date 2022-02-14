(ns sci.impl.analyzer-test
  (:require [clojure.test :refer [deftest is]]
            [sci.core :as sci]))

(deftest closure-bindings
  (let [bnds (volatile! {})]
    (sci/eval-string* (assoc (sci/init {})
                             :closure-bindings bnds)
                      "((fn wrap [] (((let [x 1 y 1] (fn fsuper []  (fn f1 [] x) (fn f2 [] y)))))))")
    (let [bnds @bnds
          [_ v] (first bnds)
          v (dissoc v :syms)]
      ;; TODO: update after refactor
      #_#_(is (= 1 (count v)))
      (let [[_ v] (first v)]
        (is (= 2 (count v)))
        (let [[v1 v2] (vals v)]
          (is (:syms v1))
          (is (set? (:syms v1)))
          (is (= 1 (count (:syms v1))))
          (is (:syms v2))
          (is (set? (:syms v2)))
          (is (= 1 (count (:syms v2))))
          (not= (:syms v1) (:syms v2)))))))
