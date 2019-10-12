(ns sci.impl.java
  (:require [sci.core :as sci]
            [sci.impl.java.options])
  (:gen-class
   :name borkdude.sci.Sci
   :methods [^:static [evalString [java.lang.String] java.lang.Object]
             ^:static [evalString [java.lang.String java.lang.Object] java.lang.Object]])
  (:import [borkdude.sci.options Options]))

(defn -evalString
  ([s] (sci/eval-string s))
  ([s ^Options opts]
   (let [opts (.val opts)]
     (sci/eval-string s opts))))




