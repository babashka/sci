(ns sci.safety-test
  (:require
   [clojure.test :as test :refer [deftest is]]
   [sci.test-utils :as tu]))

(defn max-iterate-fn [n]
  (let [cnt (atom n)]
    (fn [_] (when (zero? (swap! cnt dec))
              (throw (ex-info "iteration" {:type :sci.error/iterated-beyond-max}))))))

(deftest iterate-max-test
  (when-not tu/native?
    (let [d (try (tu/eval* "(loop [i 1000] (if (zero? i) :done (recur (dec i))))" {:invoke-callback (max-iterate-fn 1000)})
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                   (ex-data e)))]
      (is (= :sci.error/iterated-beyond-max (:type d)))))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"iteration"
                        (tu/eval* "(reduce (fn [_ _]) (range 1000))" {:invoke-callback (max-iterate-fn 100)})))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"iteration"
                        (tu/eval* "((fn pow [x n] (case n 1 x, (* x (pow x (dec n))))) 2 10)" {:invoke-callback (max-iterate-fn 10)})))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                        #"iteration"
                        (tu/eval*  "(loop [i 1000] (if (zero? i) :done (recur (dec i))))" {:invoke-callback (max-iterate-fn 100)})))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
               (tu/eval* "((fn a [n] (if (#{0 1} n) 1 (+ (a (- n 2)) (a (- n 1)))))  30)" {:invoke-callback (max-iterate-fn 10000)})))

  (is (= :done (tu/eval*  "(loop [i 10] (if (zero? i) :done (recur (dec i))))" {:invoke-callback (max-iterate-fn 100)})))
  (is (= :done (tu/eval*  "(loop [i 1000] (if (zero? i) :done (recur (dec i))))" {})))

  (is (= 1024 (tu/eval* "((fn pow [x n] (case n 1 x, (* x (pow x (dec n))))) 2 10)" {:invoke-callback (max-iterate-fn 100)})))
  (is (= 1024 (tu/eval* "((fn pow [x n] (case n 1 x, (* x (pow x (dec n))))) 2 10)" {}))))
