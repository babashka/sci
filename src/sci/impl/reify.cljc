(ns sci.impl.reify
  {:no-doc true}
  (:refer-clojure :exclude [reify])
  (:require [sci.impl.types :as t]
            [sci.impl.utils :refer [split-when]]))

(defn reify [_ _ _ctx & args]
  (let [classes->methods (split-when symbol? args)
        classes->methods (into {} (map (fn [[class &  methods]]
                                         [class (into {}
                                                      (map (fn [meth]
                                                             `['~(first meth) (fn ~(second meth) ~@(nnext meth))])
                                                          methods))])
                                       classes->methods))]
    `(clojure.core/reify* ~classes->methods)))

(defn reify* [#?(:clj ctx
                 :cljs _ctx) classes->methods]
  #?(:clj (let [ks (keys classes->methods)]
            ;; NOTE: if the first thing in reify is a class, we assume all
            ;; classes and no protocols. This should be addressed in a future version.
            (if (class? (first ks))
              (let [class-names (set (map #(symbol (.getName ^Class %)) ks))]
                (if-let [factory (get-in ctx [:reify class-names])]
                  (factory (zipmap class-names (vals classes->methods)))
                  (throw (ex-info (str "No reify factory for: " class-names)
                                  {:class class}))))
              ;; So far we only supported reify-ing one protocol at a time. This
              ;; should be addressed in a future version
              (let [[interface methods] (first classes->methods)]
                (t/->Reified interface methods))))
     :cljs (let [[interface methods] (first classes->methods)]
             (t/->Reified interface methods))))
