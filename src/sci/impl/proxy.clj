(ns sci.impl.proxy
  {:no-doc true}
  (:refer-clojure :exclude [proxy])
  (:require [sci.ctx-store :as store]))

(defn proxy [form _ classes _args & methods]
  (let [abstract-class (first classes)
        interfaces (vec (rest classes))
        methods (into {}
                      (map (fn [meth]
                             (let [[meth-name & bodies] meth
                                   meth-name (if (namespace meth-name)
                                               (symbol (name meth-name))
                                               meth-name)
                                   bodies (if (vector? (first bodies))
                                            [bodies]
                                            bodies)
                                   bodies (map (fn [[args & body]]
                                                 (list* (into ['this] args) body))
                                               bodies)]
                               [(list 'quote meth-name) (list* 'fn bodies)]))
                           methods))]
    `(clojure.core/proxy* '~form ~abstract-class ~interfaces ~methods)))

(defn proxy*
  [_form abstract-class interfaces methods]
  (if-let [pfn (:proxy-fn (store/get-ctx))]
    (let [{interfaces true protocols false} (group-by class? interfaces)]
      (pfn {:class abstract-class
            :interfaces (set interfaces)
            :protocols protocols
            :methods methods}))
    (throw (Exception. "no proxy-fn"))))
