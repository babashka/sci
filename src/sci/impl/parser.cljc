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
  {:dispatch {\' (fn [expr] (list 'quote expr))
              \@ (fn [val] (list 'deref val))
              \# {\( readers/read-fn
                  \" re-pattern}}})

(defn parse-next [r]
  (edamame.impl.parser/parse-next opts r))

(defn parse-string [s]
  (edamame/parse-string s opts))

(defn parse-string-all [s]
  (edamame/parse-string-all s opts))

;;;; Scratch

(comment
  (parse-string "{:a 1} {:a 2}")
  (parse-string-all "{:a 1} {:a 2}")
  (parse-string "#(-> % :foo :bar)")
  (parse-string "#\"foo\"")
  (with-in-str "#{:a :b :c}" (parse-next *in*))
  )
