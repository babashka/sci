(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord])
  (:require [sci.impl.utils :refer [split-when]]))

(defn defrecord [_ _ _ctx record-name fields & protocol-impls]
  (let [factory-fn-sym (symbol (str "->" record-name))
        keys (mapv keyword fields)
        protocol-impls (split-when symbol? protocol-impls)
        protocol-impls (mapv (fn [impl]
                               ;; TODO: impl body may have references to field, e.g. a should become (:a this)
                               (prn "impl" impl)) protocol-impls)]
    `(do (defn ~factory-fn-sym [& args#]
           (vary-meta (zipmap ~keys args#)
                      assoc
                      :sci.impl/record true
                      :sci.impl/type '~record-name))
         )))
