(ns sci.impl.types
  {:no-doc true}
  (:refer-clojure :exclude [eval]))

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

(defprotocol Sexpr
  (sexpr [this]))

(extend-protocol Sexpr
  #?(:clj Object :cljs default) (sexpr [this] this))

(defprotocol Stack
  (stack [this]))

(extend-protocol Stack
  #?(:clj Object :cljs default) (stack [_this] nil))

(defprotocol Eval
  (eval [expr ctx ^objects bindings]))
