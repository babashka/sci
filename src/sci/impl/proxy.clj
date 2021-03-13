(ns sci.impl.proxy
  {:no-doc true}
  (:refer-clojure :exclude [proxy]))

(defn proxy [form _ _ctx classes _args & methods]
  (let [abstract-class (first classes)
        methods (into {}
                      (map (fn [meth]
                             (let [[meth-name & bodies] meth
                                   bodies (if (vector? (first bodies))
                                            [bodies]
                                            bodies)
                                   bodies (map (fn [[args & body]]
                                                 (list* (into ['this] args) body))
                                               bodies)]
                               [(list 'quote meth-name) (list* 'fn bodies)]))
                           methods))]
    `(clojure.core/proxy* '~form ~abstract-class ~methods)))

(defn proxy*
  [ctx _form abstract-class methods]
  (if-let [pfn (:proxy-fn ctx)]
    (pfn {:class abstract-class
          :methods methods})
    (throw (Exception. "no proxy-fn"))))
