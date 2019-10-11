(ns sci.java
  (:require [sci.core :as sci])
  (:gen-class
   :name borkdude.sci.Java
   :methods [^:static [evalString [java.lang.String] java.lang.Object]
             ^:static [evalString [java.lang.String java.lang.Object] java.lang.Object]]))

(gen-class
 :name "borkdude.sci.Options"
 :constructors {[] []}
 :state "state"
 :init "init"
 :prefix "opts-"
 :methods [["addBinding" [java.lang.String java.lang.Object] void]
           ["val" [] Object]])

(defn opts-init []
  [[] (atom {})])

(defn opts-addBinding [this k v]
  (let [state (.state this)]
    (swap! state update :bindings assoc (symbol k) v))
  nil)

(defn opts-val [this]
  @(.state this))

(defn -evalString
  ([s] (sci/eval-string s))
  ([s ^borkdude.sci.Options opts]
   (let [opts (.val opts)]
     (sci/eval-string s opts))))




