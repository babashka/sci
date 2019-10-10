(ns sci.js
  "JavaScript interface to sci."
  (:require [sci.core :as sci]))

(defn map-vals [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

(defn map-keys [f m]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m))

(defn symbolize-keys [m]
  (map-keys symbol m))

(defn- js-opts->cljs-opts [opts]
  (let [cljs (js->clj opts)
        bindings (get cljs "bindings")
        bindings (symbolize-keys bindings)
        namespaces (get cljs "namespaces")
        namespaces (map-vals symbolize-keys namespaces)
        namespaces (symbolize-keys namespaces)]
    {:bindings bindings
     :namespaces namespaces}))

(defn ^:export toJS [fn]
  (if (instance? MetaFn fn)
    ;; when returning a function, make it callable from JS
    (.-afn fn)
    (clj->js fn)))

(defn ^:export evalString
  "Evaluates string `s` as a Clojure form using the Small Clojure Interpreter.

  The object `opts` may contain the following:

  - `\"bindings\"`: an object with bindings that are used to resolve symbols
  in the Clojure form, e.g. `{'x 1}`."
  ([s] (evalString s nil))
  ([s opts]
   (sci/eval-string s (js-opts->cljs-opts opts))))
