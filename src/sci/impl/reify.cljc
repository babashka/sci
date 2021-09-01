(ns sci.impl.reify
  {:no-doc true}
  (:refer-clojure :exclude [reify])
  #?(:cljs (:require [sci.impl.types :as t])))

(defn reify [form _ _ctx & args]
  (let [{classes true methods false} (group-by symbol? args)
        methods (->> (group-by first methods)
                     (map (fn [[meth bodies]]
                            (let [meth (if (simple-symbol? meth)
                                         meth
                                         (symbol (name meth)))]
                              `['~meth (fn ~@(map rest bodies))])))
                     (into {}))]
    `(clojure.core/reify* '~form ~(vec classes) ~methods)))

(defn reify*
  #?(:clj [ctx form classes methods]
     :cljs [_ctx _form classes methods])
     #?(:clj (let [{interfaces true protocols false} (group-by class? classes)]
            (if-let [factory (:reify-fn ctx)]
              (with-meta (factory {:interfaces (set interfaces)
                                   :methods methods
                                   :protocols (set protocols)})
                (meta form))
              (throw (ex-info (str "No reify factory for: " interfaces)
                              {:class class}))))
     ;; NOTE: in CLJS everything is a protocol in reify, except Object
     ;; So it's probably better if we dissoc-ed that from the set of classes
     ;; However, we only use that set to test in satisfies?
     :cljs (t/->Reified classes methods (set classes))))
