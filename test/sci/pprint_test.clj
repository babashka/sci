(ns sci.pprint-test
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is]]
   [sci.core :as sci]
   [sci.pprint]))

(defn pprint [o]
  (binding [*out* @sci/out]
    (pp/pprint o)))

(deftest pprint-simple-dispatch-test
  (is (= "<6>"
         (str/trim
          (sci/with-out-str
            (sci/eval-string "
(require '[clojure.pprint :as pprint])

(defrecord Foo [x])

(defmethod pprint/simple-dispatch Foo [o]
  (print (format \"<%s>\" (:x o))))

(pprint/pprint (->Foo 6))
"
                             {:namespaces
                              {'clojure.pprint {'pprint pprint
                                                'simple-dispatch pp/simple-dispatch}}}))))))
