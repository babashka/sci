(ns sci.addons
  {:no-doc true}
  (:refer-clojure :exclude [future pmap])
  #?(:clj (:require [sci.impl.addons.future :as f])))

;; For backward compatibility
#?(:clj
   (defn future [opts]
     (f/install opts)))
