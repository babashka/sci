(ns sci.impl.reify
  (:refer-clojure :exclude [reify])
  (:require [sci.impl.types :as t]))

(defn reify [_ _ _ctx interface & meths]
  (let [meths (into {} (map (fn [meth]
                              `['~(first meth) (fn ~(second meth) ~@(nnext meth))])
                            meths))]
    `(clojure.core/reify* ~interface ~meths)))

(defn reify* [#?(:clj ctx
                 :cljs _ctx) interface meths]
  #?(:clj (if (class? interface)
            (let [class-name (symbol (.getName ^Class interface))]
              (if-let [factory (get-in ctx [:reify class-name])]
                (:object (factory {:class interface :methods meths}))
                (throw (ex-info (str "No reify factory for: " class-name)
                                {:class class}))))
            (t/->Reified interface meths))
     :cljs (t/->Reified interface meths)))
