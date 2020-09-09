(ns sci.deref-test
  (:require [clojure.test :refer [deftest is]]
            [sci.test-utils :as tu]))

(def prog
  #?(:clj  "@(reify clojure.lang.IDeref (deref [_] :value))"
     :cljs "@(reify IDeref (-deref [_] :value))"))

(deftest deref-test
  (is (= (tu/eval* prog {}) :value)))
