(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord]))

(defn defrecord [_ _ _ctx record-name fields & _sigs]
  (let [factory-fn-sym (symbol (str "->" record-name))
        keys (mapv keyword fields)]
    `(do (defn ~factory-fn-sym [& args#]
           (zipmap ~keys args# )))))
