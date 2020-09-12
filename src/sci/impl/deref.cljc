(ns sci.impl.deref
  (:refer-clojure :exclude [deref])
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
       :ns (vars/->SciNamespace 'clojure.lang nil)}
      {:ns (vars/->SciNamespace 'clojure.lang nil)})
     :cljs
     (new-var
      'cljs.core.IDeref
      {:methods #{'-deref}
       :ns (vars/->SciNamespace 'cljs.core nil)}
      {:ns (vars/->SciNamespace 'cljs.core nil)})))

(defmulti deref types/type-impl)

(defmethod deref :sci.impl.protocols/reified [ref]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:clj 'deref
                     :cljs '-deref)) ref)))

;; All other implementations redirect to core deref
;; For clojure.core this is a multi-arity definition (promise, future)
(defmethod deref :default [ref & args]
  (apply clojure.core/deref ref args))
