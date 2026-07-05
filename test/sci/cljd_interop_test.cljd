(ns sci.cljd-interop-test
  (:require [clojure.test :refer #?(:cljd [deftest is testing]
                                    :default [deftest is testing])]
            [sci.test-utils :as tu]))

#?(:cljd
   (deftest instance-method-override-test
     (testing "instance method dispatches to override fn"
       (is (= 3 (tu/eval* "(.foo \"abc\")"
                          {:classes {'String {:instance-methods {'foo (fn [s] (count s))}}}}))))
     (testing "override receives args"
       (is (= "abc!" (tu/eval* "(.foo \"abc\" \"!\")"
                               {:classes {'String {:instance-methods {'foo (fn [s x] (str s x))}}}}))))
     (testing "unlisted method throws"
       (is (thrown? cljd.core/ExceptionInfo
                    (tu/eval* "(.bar \"abc\")"
                              {:classes {'String {:instance-methods {'foo (fn [s] :x)}}}}))))))
