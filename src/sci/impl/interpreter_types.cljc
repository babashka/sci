(ns sci.impl.interpreter-types
  (:require [sci.impl.types :as types]
            [sci.impl.utils :as utils]))

;; new way of interpreting: we don't walk the AST, the AST walks itself

(deftype EvalVarExpr [v]
  types/IInterpret
  (-interpret? [_] true)
  (-interpret [_ _]
    (deref v))
  (-expr [_] v)
  (-tag [_] :eval-var))

(defn ->eval-var [v]
  (EvalVarExpr. v))

(deftype EvalCall [v]
  types/IInterpret
  (-interpret? [_] true)
  (-interpret [_ ctx]
    (@utils/eval-call ctx v))
  (-expr [_] v)
  (-tag [_] :call))

(defn ->eval-call [v]
  (EvalCall. v))

(deftype EvalConstant [v]
  types/IInterpret
  (-interpret? [_] true)
  (-interpret [_ _] v)
  (-expr [_] v)
  (-tag [_] :constant))

(defn ->eval-constant [v]
  (EvalConstant. v))

(deftype ResolveSymbol [sym]
  types/IInterpret
  (-interpret? [_this] true)
  (-interpret [_this ctx]
    (let [^java.util.Map bindings (.get ^java.util.Map ctx :bindings)]
      (#?@(:clj [if (.containsKey bindings sym) (.get bindings sym)]
           :cljs [if-let [v (find bindings sym)] (second v)])
       ;; TODO: check if symbol is in macros and then emit an error: cannot take
       ;; the value of a macro
       (utils/throw-error-with-location
        (str "Could not resolve symbol: " sym "\nks:" (keys (:bindings ctx)))
        sym)))
    )
  (-expr [_] sym)
  (-tag [_] :resolve-symbol))

(defn ->resolve-symbol [sym]
  (ResolveSymbol. sym))
