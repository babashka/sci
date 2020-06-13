(ns sci.impl.types
  {:no-doc true})

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(deftype EvalVar [v]
  IBox
  (getVal [this] v))

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

(defn instance-impl [clazz x]
  (if (and (symbol? clazz)
           (let [m (meta clazz)]
             (:sci.impl/record m)))
    (= clazz (some-> x meta :sci.impl/type))
    (instance? clazz x)))
