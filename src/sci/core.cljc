(ns sci.core
  (:require
   [sci.impl.interpreter :as i]))

(defn ^:export eval-string
  "Evaluates string `s` as a Clojure form using the Small Clojure Interpreter.

  The map `opts` may contain the following:

  - `:bindings`: a map with bindings that are used to resolve symbols
  in the Clojure form, e.g. `{'x 1}`."
  ([s] (eval-string s nil))
  ([s opts]
   (i/eval-string s opts)))

;;;; Scratch

(comment
  (eval-string "(inc x)" {:bindings {'x 2}})
  )
