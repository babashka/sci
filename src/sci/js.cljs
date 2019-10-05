(ns sci.js
  "JavaScript interface to sci."
  (:require [sci.core :as sci]))

(defn- js-opts->cljs-opts [opts]
  (let [cljs (js->clj opts)
        bindings (get cljs "bindings")
        bindings (zipmap (map symbol (keys bindings)) (vals bindings))]
    {:bindings bindings}))

(defn ^:export evalString
  "Evaluates string `s` as a Clojure form using the Small Clojure Interpreter.

  The object `opts` may contain the following:

  - `\"bindings\"`: an object with bindings that are used to resolve symbols
  in the Clojure form, e.g. `{'x 1}`."
  ([s] (evalString s nil))
  ([s opts]
   (sci/eval-string s (js-opts->cljs-opts opts))))
