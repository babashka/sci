(ns sci.impl.interpreter-types
  (:require [sci.impl.vars :as vars]))

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
    ;; TODO: we can already do this in the analyzer, but let's not refactor too
    ;; much in one go
    (if-not (vars/isMacro v)
      (deref v)
      (throw (new #?(:clj IllegalStateException :cljs js/Error)
                  (str "Can't take value of a macro: " v ""))))))
