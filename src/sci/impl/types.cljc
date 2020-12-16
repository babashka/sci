(ns sci.impl.types
  {:no-doc true})

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(defprotocol IReified
  (getInterface [_])
  (getMethods [_]))

(deftype Reified [interface meths]
  IReified
  (getInterface [_] interface)
  (getMethods [_] meths))

(defn type-impl [x & _xs]
  (or (when (instance? sci.impl.types.Reified x)
        :sci.impl.protocols/reified)
      (some-> x meta :sci.impl/type)
      (type x)))

;; returned from analyzer when macroexpansion needs interleaved eval
(deftype EvalForm [form]
  IBox
  (getVal [this] form))

(defprotocol IEval
  (-eval [this ctx]))

(defprotocol IOp
  (-op [this]))

(deftype Eval [f x op]
  IEval
  (-eval [_ ctx]
    (f ctx))
  IBox
  (getVal [_] x)
  IOp
  (-op [_] op))

(defn ->eval [f x op]
  (Eval. f x op))
