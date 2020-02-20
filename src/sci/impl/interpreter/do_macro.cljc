(ns sci.impl.interpreter.do-macro
  {:no-doc true}
  (:require [sci.impl.protocols :refer [IInterpret -interpret]]
            [sci.impl.utils :as utils :refer [rethrow-with-location-of-node]]))

(defn eval-do*
  [ctx exprs]
  (loop [[expr & exprs] exprs]
    (let [ret (try (-interpret expr ctx)
                   (catch #?(:clj Throwable :cljs js/Error) e
                     (rethrow-with-location-of-node ctx e expr)))]
      (if-let [exprs (seq exprs)]
        (recur exprs)
        ret))))

(defn eval-do
  [ctx expr]
  (when-let [exprs (next expr)]
    (eval-do* ctx exprs)))
