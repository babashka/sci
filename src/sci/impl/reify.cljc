(ns sci.impl.reify
  {:no-doc true}
  (:refer-clojure :exclude [reify])
  #?(:cljs (:require [sci.impl.types :as t])))

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
                interfaces (->> classes
                                (map #(symbol (.getName ^Class %)))
                                set)
                interfaces (if protocols?
                              (conj interfaces 'sci.impl.types.IReified)
                              interfaces)]
            (if-let [factory (:reify-fn ctx)]
              (factory interfaces methods (set protocols))
              (throw (ex-info (str "No reify factory for: " interfaces)
                              {:class class}))))
     ;; NOTE: in CLJS everything is a protocol in reify, except Object
     ;; So it's probably better if we dissoc-ed that from the set of classes
     ;; However, we only use that set to test in satisfies?
     :cljs (t/->Reified classes methods (set classes))))
