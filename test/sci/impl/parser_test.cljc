(ns sci.impl.parser-test
  (:require
   [sci.impl.parser :as p]
   [clojure.test :as t :refer [deftest is]]))

(deftest parser-test
  (is (= "foo" (p/parse-string "\"foo\"")))
  (is (= 'foo (p/parse-string "foo")))
  (is (= :foo (p/parse-string ":foo")))
  (is (= :foo/bar (p/parse-string ":foo/bar")))
  (is (= '(1 2 3) (p/parse-string "(1 2 3)")))
  (is ((every-pred vector? #(= % [1 2 3])) (p/parse-string "[1 2 3]")))
  (is (= #{1 2 3} (p/parse-string "#{1 2 3}")))
  (is (= {:a 1 :b 2} (p/parse-string "{:a 1 :b 2}")))
  (is (= {:row 1 :col 2}  (meta (first (p/parse-string "[{:a 1 :b 2}]")))))
  (is (= {:foo true :row 1 :col 1} (meta (p/parse-string "^:foo {:a 1 :b 2}"))))
  (let [p (p/parse-string ";; foo\n{:a 1}")]
    (is (= {:a 1} p))
    (is (= {:row 2 :col 1} (meta p))))
  (is (= '(deref foo) (p/parse-string "@foo")))
  (is (= '(defn foo []) (p/parse-string "(defn foo [])")))
  (let [foo-sym (second (p/parse-string "(defn foo [])"))]
    (is (= {:row 1 :col 7} (meta foo-sym))))
  (is (:sci/fn (p/parse-string "#(inc 1 2 %)")))
  (is (re-find (p/parse-string "#\"foo\"") "foo"))
  (is (= '(do (+ 1 2 3)) (p/parse-string "(do (+ 1 2 3)\n)"))))

