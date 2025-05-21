(ns sci.impl.types
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  #?(:clj (:require [sci.impl.macros :as macros]))
  #?(:cljs (:require-macros [sci.impl.macros :as macros]
                            [sci.impl.types :refer [->Node]]))
  #?(:clj (:import [sci.impl.types IReified])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

#?(:cljs
   (defprotocol IReified
     (getInterfaces [_])
     (getMethods [_])
     (getProtocols [_])))

#?(:clj
   (do (defn getMethods [obj]
         (.getMethods ^IReified obj))
       (defn getInterfaces [obj]
         (.getInterfaces ^IReified obj))
       (defn getProtocols [obj]
         (.getProtocols ^IReified obj))))

(deftype Reified [interfaces meths protocols]
  IReified
  (getInterfaces [_] interfaces)
  (getMethods [_] meths)
  (getProtocols [_] protocols))

(defprotocol SciTypeInstance
  (-get-type [_])
  (-mutate [_ k v]))

(defn type-impl
  "Must be varargs because used in multimethods
  Only for internal use!"
  [x & _]
  (or (when #?(:clj (instance? sci.impl.types.IReified x)
               :cljs (cljs.core/implements? sci.impl.types.IReified x))
        :sci.impl.protocols/reified)
      (when (#?(:clj instance?
                :cljs cljs.core/implements?) sci.impl.types.SciTypeInstance x)
        (-get-type x))
      (some-> x meta :type)
      #?(:clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

(defn type-impl2
  "Externally available type implementation."
  [x]
  (or (some-> x meta :type)
      (when (#?(:clj instance?
                :cljs cljs.core/implements?) sci.impl.types.SciTypeInstance x)
        (-get-type x))
      #?(:clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

;; returned from analyzer when macroexpansion needs interleaved eval
(deftype EvalForm [form]
  IBox
  (getVal [_this] form))

(defprotocol Stack
  (stack [this]))

(extend-protocol Stack
  #?(:clj Object :cljs default) (stack [_this] nil))

#?(:clj (defprotocol Eval
          (eval [expr ctx ^objects bindings])))

#?(:cljs
   (defrecord NodeR [f stack]
     Stack (stack [_] stack)))

#?(:cljs
   ;; For performance reasons on CLJS we do not use eval as a protcol method but
   ;; as a separate function which does an instance check on a concrete type.
   (defn eval [expr ctx bindings]
     (if (instance? NodeR expr)
       ((.-f expr) expr ctx bindings)
       expr)))

(macros/deftime
  (defmacro ->Node
    [body stack]
    (macros/?
     :clj `(reify
             sci.impl.types/Eval
             (~'eval [~'this ~'ctx ~'bindings]
              ~body)
             sci.impl.types/Stack
             (~'stack [_#] ~stack))
     :cljs `(->NodeR
             (fn [~'this ~'ctx ~'bindings]
               ~body)
             ~stack))))

#?(:clj
   (deftype ConstantNode [x]
     Eval (eval [_expr _bindings _ctx]
            x)
     Stack (stack [_] nil)))

(defn ->constant [x]
  #?(:clj (->ConstantNode x)
     :cljs x))

(defprotocol HasName ;; INamed was already taken by CLJS
  (getName [_]))
