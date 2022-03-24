(ns sci.impl.types
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  #?(:clj (:require [sci.impl.macros :as macros]))
  #?(:cljs (:require-macros [sci.impl.macros :as macros]
                            [sci.impl.types :refer [->Node]])))

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(defprotocol IReified
  (getInterfaces [_])
  (getMethods [_])
  (getProtocols [_]))

(deftype Reified [interfaces meths protocols]
  IReified
  (getInterfaces [_] interfaces)
  (getMethods [_] meths)
  (getProtocols [_] protocols))

(defn type-impl [x & _xs]
  (or (when (instance? #?(:clj sci.impl.types.IReified :cljs sci.impl.types/Reified) x)
        :sci.impl.protocols/reified)
      (some-> x meta :type)
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
