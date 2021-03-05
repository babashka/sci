(ns sci.impl.reify
  {:no-doc true}
  (:refer-clojure :exclude [reify])
  (:require [sci.impl.types :as t]
            [sci.impl.utils :refer [split-when]])
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
                                 set)

                reify-methods {'getInterfaces (constantly protocols)
                               'getMethods (constantly methods)}
                methods (if protocols? (merge reify-methods methods) methods)

                ;; The reify factories get the fn by classname and method name
                ;; however it shouldn't actually matter which class the method
                ;; belongs to, as different classes may have the same method
                ;; names with different arities.
                ;; classes->methods puts all methods for all classes.
                classes->methods (into {} (map vector class-names (repeat methods)))]
            (if-let [factory (get-in ctx [:reify class-names])]
              (factory classes->methods)
              (throw (ex-info (str "No reify factory for: " class-names)
                              {:class class}))))
     :cljs (t/->Reified classes methods)))
