(ns sci.lang
  {:no-doc true}
  (:require [clojure.string :as str]
            [sci.impl.types]))

;; marker interface for vars, clj only for now
#?(:clj (definterface IVar))

(defn- class-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s (inc i))
    s))

(defn- package-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s 0 i)
    s))

(deftype Type [^:volatile-mutable
                  #_:clj-kondo/ignore
                  __data]
  sci.impl.types/IBox
  (getVal [_] __data)
  (setVal [_ v] (set! __data v))
  Object
  (toString [_]
    (str (:sci.impl/type-name __data)))
  #?@(:clj
      [clojure.lang.IMeta
       (meta [_] __data)]
      :cljs
      [IMeta
       (-meta [_] __data)])
  #?@(:clj
      [clojure.lang.IObj
       (withMeta
        [this m]
        (set! __data m)
        this)]
      :cljs
      [IWithMeta
       (-with-meta
        [this m]
        (set! __data m)
        this)])
  #?@(:clj
      [clojure.lang.Named
       (getNamespace [this]
                     (package-name (str this)))
       (getName [this]
                (class-name (str this)))]
      :cljs
      [INamed
       (-namespace [this]
                     (package-name (str this)))
       (-name [this]
              (class-name (str this)))]))

#?(:clj (defmethod print-method Type [this w]
          (.write w (str this))))
