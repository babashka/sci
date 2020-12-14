(ns sci.impl.interpreter-types
  (:require [sci.impl.types :as types]
            [sci.impl.utils :as utils]))

;; new way of interpreting: we don't walk the AST, the AST walks itself

(deftype EvalVarExpr [v]
  types/IInterpret
  (-interpret? [this] true)
  (-interpret [this ctx]
    (deref v))
  (-expr [this] v)
  (-tag [this] :eval-var))

(defn ->eval-var [v]
  (EvalVarExpr. v))

(deftype EvalCall [v]
  types/IInterpret
  (-interpret? [this] true)
  (-interpret [this ctx]
    (@utils/eval-call ctx v))
  (-expr [this] v)
  (-tag [this] :call))

(defn ->eval-call [v]
  (EvalCall. v))
