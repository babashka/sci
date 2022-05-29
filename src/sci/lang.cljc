(ns sci.lang
  (:require [clojure.string :as str]
            [sci.impl.types]))

#?(:clj (set! *warn-on-reflection* true))

;; marker interface for vars, clj only for now
#?(:clj (definterface ^{:doc "Marker interface for SCI vars."} IVar))

(defn- class-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s (inc i))
    s))

(defn- package-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s 0 i)
    s))

(deftype ^{:doc "Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`."}
    Type [^:volatile-mutable --data-impl
          ^:volatile-mutable --namespace-impl
          ^:volatile-mutable --name-impl]
  sci.impl.types/IBox
  (getVal [_] --data-impl)
  (setVal [_ v] (set! --data-impl v))
  Object
  (toString [_]
    (str (:sci.impl/type-name --data-impl)))

  ;; meta is only supported to get our implementation! keys out
  #?@(:clj
      [clojure.lang.IMeta
       (meta [_] --data-impl)]
      :cljs
      [IMeta
       (-meta [_] --data-impl)])

  ;; we need to support Named for `derive`
  #?@(:clj
      [clojure.lang.Named
       (getNamespace [this]
                     (if (nil? --namespace-impl)
                       (let [ns (package-name (str this))]
                         (set! --namespace-impl ns)
                         ns)
                       --namespace-impl))
       (getName [this]
                (if (nil? --name-impl)
                  (let [nom (class-name (str this))]
                    (set! --name-impl nom)
                    nom)
                  --name-impl))]
      :cljs
      [INamed
       (-namespace [this]
                   (if (nil? --namespace-impl)
                     (let [ns (package-name (str this))]
                       (set! --namespace-impl ns)
                       ns)
                     --namespace-impl))
       (-name [this]
              (if (nil? --name-impl)
                (let [nom (class-name (str this))]
                  (set! --name-impl nom)
                  nom)
                --name-impl))]))

#?(:clj (defmethod print-method Type [this w]
          (.write ^java.io.Writer w (str this))))
