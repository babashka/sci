(ns sci.js-test
  (:require [clojure.test :refer [deftest is]]
            [sci.impl.js :as js-api]))

(deftest interop-test
  (is (= 4 (js-api/evalString "(foo/bar)"
                              #js {:classes #js {:allow "all"}
                                   :namespaces #js {:foo #js {:bar (fn [] 4)}}})))
  (is (= 4 (js-api/evalString "(.f foo)"
                              #js {:classes #js {:allow "all"}
                                   :bindings #js {:foo #js {:f (fn [] 4)}}})))
  (is (thrown-with-msg?
       js/Error #"allowed"
       (js-api/evalString "(loop)"
                          #js {:deny ["loop"]})))
  (is (thrown-with-msg?
       js/Error #"allowed"
       (js-api/evalString "(loop)"
                          #js {:preset "termination-safe"}))))
