(ns sci.impl.js
  "JavaScript interface to sci."
  {:no-doc true}
  (:require
   [goog.object :as gobject]
   [sci.core :as sci]))

(defn map-vals [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

(defn map-keys [f m]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m))

(defn obj->clj [x k-fn]
  (persistent!
   (reduce (fn [r k] (assoc! r (k-fn k) (gobject/get x k)))
           (transient {}) (js-keys x))))

(defn- js-opts->cljs-opts [opts]
  (let [cljs (obj->clj opts keyword)
        bindings (get cljs :bindings)
        bindings (obj->clj bindings symbol)
        namespaces (get cljs :namespaces)
        namespaces (obj->clj namespaces symbol)
        namespaces (map-vals (fn [namespace]
                               (obj->clj namespace symbol))
                             namespaces)
        allow (get cljs :allow)
        allow (mapv symbol allow)
        deny (get cljs :deny)
        deny (mapv symbol deny)
        realize-max (get cljs :realizeMax)
        preset (when-let [v (get cljs :preset)]
                 (keyword v))
        classes (get cljs :classes)
        classes (obj->clj classes symbol)
        allow-all-classes (get classes 'allow)
        classes (if allow-all-classes (assoc classes :allow allow-all-classes) classes)
        env (get cljs :env)]
    {:bindings bindings
     :namespaces namespaces
     :allow allow
     :deny deny
     :preset preset
     :realize-max realize-max
     :classes classes
     :env env}))

(defn ^:export toJS [v]
  (if (instance? MetaFn v)
    ;; when returning a function, make it callable from JS
    (.-afn v)
    (clj->js v)))

(defn ^:export evalString
  "Evaluates string `s` as a Clojure form using the Small Clojure Interpreter.

  The object `opts` may contain the following:

  - `\"bindings\"`: an object with bindings that are used to resolve symbols
  in the Clojure form, e.g. `{'x 1}`."
  ([s] (evalString s nil))
  ([s opts]
   (sci/eval-string s (js-opts->cljs-opts opts))))
