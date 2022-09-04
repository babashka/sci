(ns sci.parse-test
  (:require  [clojure.test :as t :refer [deftest is]]
             [sci.core :as sci]))

(deftest parse-test
  (let [prog "(defn foo [] 1) (foo)"
        reader (sci/reader prog)
        ctx (sci/init {})
        form-1 (sci/parse-next ctx reader)
        form-2 (sci/parse-next ctx reader)]
    (is (= '(defn foo [] 1) form-1))
    (is (= '(foo) form-2))
    (sci/eval-form ctx form-1)
    (is (= 1 (sci/eval-form ctx form-2)))
    (is (= ::sci/eof (sci/parse-next ctx reader)))
    (is (= 1 (sci/get-line-number reader)))
    (is (= 22 (sci/get-column-number reader)))))
