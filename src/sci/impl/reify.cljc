(ns sci.impl.reify
  {:no-doc true}
  (:refer-clojure :exclude [reify])
  (:require [sci.impl.types :as t]
            [sci.impl.utils :refer [split-when]])
  (:import [sci.impl.types IReified]))

(defn reify [_ _ _ctx & args]
  (let [classes->methods (split-when symbol? args)
        classes->methods
        (into {} (map (fn [[class & methods]]
                        (let [methods (group-by first methods)]
                          [class (into {}
                                       (map (fn [[meth bodies]]
                                              `['~meth (fn ~@(map rest bodies))])
                                            methods))]))
                      classes->methods))]
    `(clojure.core/reify* ~classes->methods)))

(defn reify* [#?(:clj ctx
                 :cljs _ctx) classes->methods]
  #?(:clj (let [{classes true protocols false} (group-by (comp class? key) classes->methods)
                interfaces (keys protocols)
                reify-methods {'getInterfaces (constantly interfaces)
                               'getMethods (constantly (apply merge (vals protocols)))}
                classes->methods (->> (if (seq protocols)
                                        (conj classes
                                             [IReified reify-methods])
                                        classes)
                                      (map (fn [[^Class c methods]] [(symbol (.getName c)) methods]))
                                      (into {}))
                class-names (set (keys classes->methods))]
            (if-let [factory (get-in ctx [:reify class-names])]
              (factory classes->methods)
              (throw (ex-info (str "No reify factory for: " class-names)
                              {:class class}))))
     :cljs (let [interfaces (keys classes->methods)
                 methods (apply merge (vals classes->methods))]
             (t/->Reified interfaces methods))))
