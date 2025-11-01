(ns sci.addons
  {:no-doc true}
  (:refer-clojure :exclude [future pmap])
  #?@(:cljs [] :default [(:require [sci.addons.future :as f])]))

;; For backward compatibility
#?(:cljs nil :default
   (defn future [opts]
     (f/install opts)))
