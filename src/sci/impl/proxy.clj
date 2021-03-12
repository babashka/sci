(ns sci.impl.proxy
  {:no-doc true}
  (:refer-clojure :exclude [proxy]))

(defn proxy [form _ _ctx classes _args & methods]
  (let [abstract-class (first classes)
        methods (into {}
                      (map (fn [meth]
                             [(list 'quote (first meth)) (list* 'fn (rest meth))])
                             methods))]
    `(clojure.core/proxy* '~form ~abstract-class ~methods)))

(defn proxy*
  [ctx _form abstract-class methods]
  (if-let [pfn (:proxy-fn ctx)]
    (pfn {:class abstract-class
          :methods methods})
    (throw (Exception. "no proxy-fn"))))
