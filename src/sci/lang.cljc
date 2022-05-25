(ns sci.lang
  {:no-doc true}
  (:require [sci.impl.types]))

;; marker interface for vars, clj only for now
#?(:clj (definterface IVar))

(deftype SciType [^:volatile-mutable
                  #_:clj-kondo/ignore
                  __data]
  sci.impl.types.IBox
  (getVal [_] __data)
  (setVal [_ v] (set! __data v))
  Object
  (toString [_]
    (str (:sci.impl/type-name __data)))
  #?@(:clj
      [clojure.lang.IMeta
       (meta [_] __data)])
  #?@(:clj
      [clojure.lang.IObj
       (withMeta
        [this m]
        (set! __data m)
        this)]))

#?(:clj (defmethod print-method SciType [this w]
          (.write w (str this))))
