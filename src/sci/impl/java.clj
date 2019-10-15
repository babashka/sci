(ns sci.impl.java
  (:require [sci.core :as sci])
  (:gen-class
   :name borkdude.sci.Sci
   :methods [^:static [evalString [java.lang.String] java.lang.Object]
             ^:static [evalString [java.lang.String java.lang.Object] java.lang.Object]])
  (:import [borkdude.sci.options Options Namespace]
           [java.util Map List]))

(set! *warn-on-reflection* true)

(defn Map->map [Map]
  (zipmap (map symbol (keys Map)) (vals Map)))

(defn List->vec [List]
  (mapv symbol List))

(defn Namespace->map [^Namespace ns]
  (let [v (.val ns)]
    (Map->map v)))

(defn Options->map [^Options opts]
  (let [v (.val opts)
        namespaces (.get v "namespaces")
        ns-keys (map symbol (keys namespaces))
        ns-vals (map Namespace->map (vals namespaces))
        namespaces (zipmap ns-keys ns-vals)
        bindings (.get v "bindings")
        bindings (Map->map bindings)
        allow (List->vec (.get v "allow"))
        deny (List->vec (.get v "deny"))]
    {:namespaces namespaces
     :bindings bindings
     :allow allow
     :deny deny}))

(defn -evalString
  ([s] (sci/eval-string s))
  ([s ^Options opts]
   (sci/eval-string s (Options->map opts))))
