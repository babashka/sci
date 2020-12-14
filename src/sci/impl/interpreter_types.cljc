(ns sci.impl.interpreter-types
  #_(:require [sci.impl.vars :as vars]))

;; new way of interpreting: we don't walk the AST, the AST walks itself

(defprotocol IInterpret
  (-interpret [this ctx])
  ;; this is faster than satisfies? (according to bsless!)
  (-interpret? [this]))

(extend-protocol IInterpret
  #?(:clj Object :cljs default)
  (-interpret? [this] false)
  nil
  (-interpret? [this] false))

(deftype EvalVarExpr [v]
  IInterpret
  (-interpret? [this] true)
  (-interpret [this ctx]
    (deref v)))
