(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord]))

(defn defrecord [_ _ _ctx record-name fields & protocol-impls]
  (let [factory-fn-sym (symbol (str "->" record-name))
        keys (mapv keyword fields)
        protocol-impls (split-with symbol? protocol-impls)
        protocol-impls (partition 2 protocol-impls)]
    (prn protocol-impls)
    `(do (defn ~factory-fn-sym [& args#]
           (vary-meta (zipmap ~keys args#)
                      assoc
                      :sci.impl/record true
                      :sci.impl/type '~record-name))
         )))
