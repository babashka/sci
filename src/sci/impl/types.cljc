(ns sci.impl.types
  {:no-doc true})

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(deftype EvalVar [v]
  IBox
  (getVal [_this] v))

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

(declare ->EvalFn)

(defprotocol Sexpr
  (sexpr [this]))

(extend-protocol Sexpr
  #?(:clj Object :cljs default) (sexpr [this] this))

(defprotocol Info
  (info [this]))

(defprotocol Stack
  (stack [this]))

(extend-protocol Stack
  #?(:clj Object :cljs default) (stack [this] nil))

(deftype EvalFn [f info expr stack]
  ;; f = (fn [ctx] ...)
  ;; m = meta
  IBox
  (getVal [_this] f)
  #?(:clj clojure.lang.IMeta
     :cljs IMeta)
  (#?(:clj meta
      :cljs -meta) [_this] (meta expr))
  #?(:clj clojure.lang.IObj
     :cljs IWithMeta)
  (#?(:clj withMeta
      :cljs -with-meta) [_this m]
    (->EvalFn f info (with-meta expr m) stack))
  Info
  (info [_] info)
  Sexpr
  (sexpr [_] expr)
  Object
  (toString [_this]
    (str expr))
  Stack
  (stack [_] stack))
