(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn defrecord [_ _ ctx record-name fields & protocol-impls]
  (let [factory-fn-sym (symbol (str "->" record-name))
        constructor-sym (symbol (str record-name "."))
        keys (mapv keyword fields)
        protocol-impls (utils/split-when symbol? protocol-impls)
        protocol-impls
        (mapv (fn [[protocol-name impl]]
                (let [protocol-var (@utils/eval-resolve-state ctx protocol-name)
                      protocol-ns (-> protocol-var deref :ns)
                      pns (str (vars/getName protocol-ns))
                      fq-meth-name #(symbol pns %)
                      args (second impl)
                      this (first args)
                      bindings (vec (mapcat (fn [field]
                                              [field (list (keyword field) this)])
                                            fields))]
                  `(defmethod ~(fq-meth-name (str (first impl))) '~record-name ~(second impl)
                     (let ~bindings
                       ~@(nnext impl)))))
              protocol-impls)]
    `(do
       ;; (prn '~record-name)
       (def ~record-name (with-meta '~record-name
                           {:sci.impl/record true}))
       (defn ~factory-fn-sym [& args#]
           (vary-meta (zipmap ~keys args#)
                      assoc
                      :sci.impl/record true
                      :sci.impl/type '~record-name))
       (def ~constructor-sym ~factory-fn-sym)
         ~@protocol-impls)))

(defn sci-record? [x]
  (or
   (when (map? x)
     (some-> x meta :sci.impl/record))
   (clojure.core/record? x)))
