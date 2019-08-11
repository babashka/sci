(ns sci.native-test
  (:require [clojure.test :as t :refer [deftest is]]
            [me.raynes.conch :refer [let-programs] :as sh]))

(deftest native-test
  (when (= "native" (System/getenv "SCI_TEST_ENV"))
    (let-programs [sci "./sci"]
      (binding [sh/*throw* false]
        (is (= "2\n" (sci "(inc 1)")))))))
