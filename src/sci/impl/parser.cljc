(ns sci.impl.parser
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   [sci.impl.readers :as readers]
   [edamame.core :as edamame]
   [edamame.impl.parser :as parser]))

#?(:clj (set! *warn-on-reflection* true))

(def opts
  (parser/normalize-opts
   {:all true
    :read-eval false
    :fn readers/read-fn}))

(defn parse-next
  ([r]
   (parser/parse-next opts r))
  ([r features auto-resolve qualify-fn]
   (parser/parse-next (assoc opts
                             :read-cond :allow
                             :features features
                             :auto-resolve auto-resolve
                             :qualify-fn qualify-fn)
                      r)))

(defn parse-string
  ([s]
   (edamame/parse-string s opts))
  ([s features auto-resolve]
   (edamame/parse-string s (assoc opts
                                  :read-cond :allow
                                  :features features
                                  :auto-resolve auto-resolve))))

(defn parse-string-all
  ([s]
   (edamame/parse-string-all s opts))
  ([s features auto-resolve]
   (edamame/parse-string-all s (assoc opts
                                      :read-cond :allow
                                      :features features
                                      :auto-resolve auto-resolve))))

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
