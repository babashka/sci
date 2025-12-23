(ns sci.global-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [sci.core :as sci]))

(deftest refer-global-test
  (testing "init"
    (let [ctx #(sci/init {:classes {'js js/globalThis}})]
      (sci/binding [sci/ns sci/ns]
        (is (true?
             (sci/eval-form (ctx) '(do (refer-global :only '[String])
                                       (instance? String (new String "foo"))))))
        (is (true?
             (sci/eval-form (ctx) '(do (ns foo (:refer-global :only [String]))
                                       (instance? String (new String "foo"))))))
        (is (true?
             (sci/eval-form (ctx) '(do (ns foo (:refer-global :only [String] :rename {String Str}))
                                       (instance? Str (new Str "foo") ))))))
      (is (thrown? js/Error
                   (sci/eval-string* (ctx)
                                     "(ns foo (:refer-global :only [String] :rename {String Str}))
                                      (instance? String (new String \"foo\"))"))))))
