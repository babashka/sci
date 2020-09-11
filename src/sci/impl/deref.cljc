(ns sci.impl.deref
  (:require [sci.impl.types :as types]
            [sci.impl.vars :as vars]))

(defn- new-var
  "Returns a new sci var."
  ([name] (doto (new-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name meta false)))

(def deref-protocol
  #?(:clj
     (new-var
      'clojure.lang.IDeref
      {:class clojure.lang.IDeref
       :methods #{'deref}
       :ns (vars/->SciNamespace 'clojure.lang nil)})
     :cljs
     (new-var
      'cljs.core.IDeref
      {:methods #{'-deref}
       :ns (vars/->SciNamespace 'cljs.core nil)})))

;; TODO: add deref as multimethod

(defn deref*
  ([ref]
   (case (types/type-impl ref)
     :sci.impl.protocols/reified
     (let [methods (types/getMethods ref)]
       ((get methods #?(:clj 'deref :cljs '-deref)) ref))
     (deref ref)))
  #?(:clj
     ([ref timeout-ms timeout-val]
      (deref ref timeout-ms timeout-val))))
