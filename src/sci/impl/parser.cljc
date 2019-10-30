(ns sci.impl.parser
  "This code is largely inspired by rewrite-clj(sc), so thanks to all
  who contribured to those projects."
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   [sci.impl.readers :as readers]
   [edamame.core :as edamame]))

#?(:clj (set! *warn-on-reflection* true))

(def opts
  (edamame.impl.parser/normalize-opts
   {:all true
    :read-eval false
    :fn readers/read-fn}))

(defn parse-next
  ([r]
   (edamame.impl.parser/parse-next opts r))
  ([r features]
   (edamame.impl.parser/parse-next (assoc opts
                                          :read-cond :allow
                                          :features features)
                                   r)))

(defn parse-string
  ([s]
   (edamame/parse-string s opts))
  ([s features]
   (edamame/parse-string s (assoc opts
                                  :read-cond :allow
                                  :features features))))

(defn parse-string-all
  ([s]
   (edamame/parse-string-all s opts))
  ([s features]
   (edamame/parse-string-all s (assoc opts
                                      :read-cond :allow
                                      :features features))))

;;;; Scratch

(comment
  (parse-string "{:a 1} {:a 2}")
  (parse-string-all "{:a 1} {:a 2}")
  (parse-string "#(-> % :foo :bar)")
  (parse-string "#\"foo\"")
  #?(:clj (with-in-str "#{:a :b :c}" (parse-next *in*)))
  (parse-string-all "(try (/ 1 0) (catch java.lang.ArithmeticException _e 1)
                       (finally (prn \"dude\"))")
  (parse-string-all "(try 1")
  (parse-string-all "(try (/ 1 0) (catch java.lang.ArithmeticException _e 1) (finally (prn \"dude\")))")
  (parse-string-all "(prn \"DO\") (try (/ 1 0) (catch java.lang.ArithmeticException _e 1) (finally (prn \"dude\")))")
  (parse-string-all "  ")
  (parse-string "(")
  )
