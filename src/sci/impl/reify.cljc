(ns sci.impl.reify
  {:no-doc true}
  (:refer-clojure :exclude [reify])
  (:require [sci.impl.types :as t])
  #?(:clj (:import [sci.impl.types IReified])))

(defn reify [_ _ _ctx & args]
  (let [{classes true methods false} (group-by symbol? args)
        methods (->> (group-by first methods)
                     (map (fn [[meth bodies]]
                            `['~meth (fn ~@(map rest bodies))]))
                     (into {}))]
    `(clojure.core/reify* ~(vec classes) ~methods)))

(defn reify* [#?(:clj ctx
                 :cljs _ctx) classes methods]
  #?(:clj (let [{classes true protocols false} (group-by class? classes)
                protocols? (seq protocols)

                classes (if protocols? (distinct (conj classes IReified)) classes)
                class-names (->> classes
                                 (map #(symbol (.getName ^Class %)))
                                 set)]
            (if-let [factory (get-in ctx [:reify class-names])]
              (factory methods)
              (throw (ex-info (str "No reify factory for: " class-names)
                              {:class class}))))
     :cljs (t/->Reified classes methods)))
