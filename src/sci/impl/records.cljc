(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord])
  (:require [sci.impl.utils :refer [split-when]]))

(defn defrecord [_ _ _ctx record-name fields & protocol-impls]
  (let [factory-fn-sym (symbol (str "->" record-name))
        constructor-sym (symbol (str record-name "."))
        keys (mapv keyword fields)
        protocol-impls (split-when symbol? protocol-impls)
        protocol-impls
        (mapv (fn [[_protocol impl]]
                (let [args (second impl)
                      this (first args)
                      bindings (vec (mapcat (fn [field]
                                              [field (list (keyword field) this)])
                                            fields))]
                  `(defmethod ~(first impl) '~record-name ~(second impl)
                     (let ~bindings
                       ~@(nnext impl)))))
              protocol-impls)]
    `(do
       ;; (prn '~record-name)
       (def ~record-name '~record-name)
       (defn ~factory-fn-sym [& args#]
           (vary-meta (zipmap ~keys args#)
                      assoc
                      :sci.impl/record true
                      :sci.impl/type '~record-name))
       (def ~constructor-sym ~factory-fn-sym)
         ~@protocol-impls)))
