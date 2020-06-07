(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend-protocol
                            extend extend-type reify])
  (:require [sci.impl.multimethods :as mms]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn defprotocol [_ _ _ctx protocol-name & signatures]
  (let [expansion `(do
                     (def ~protocol-name {})
                     ~@(map (fn [[method-name & _]]
                              `(defmulti ~method-name clojure.core/protocol-type-impl))
                            signatures))]
    ;; (prn expansion)
    expansion))

(defn extend-protocol [_ _ _ctx _protocol-name & impls]
  (let [impls (utils/split-when #(not (seq? %)) impls)
        expansion
        `(do ~@(map (fn [[type & meths]]
                      `(do
                         ~@(map (fn [meth]
                                  `(defmethod ~(first meth) ~type ~(second meth) ~@(nnext meth)))
                                meths)))
                    impls))]
    #_(prn expansion)
    expansion))

(defn extend [ctx atype & proto+mmaps]
  (doseq [[_proto mmap] (partition 2 proto+mmaps)]
    #_(when-not (protocol? proto)
        (throw (new #?(:clj IllegalArgumentException
                       :cljs js/Error)
                    (str proto " is not a protocol"))))
    #_(when (implements? proto atype)
        (throw (new #?(:clj IllegalArgumentException
                       :cljs js/Error)
                    (str atype " already directly implements " (:on-interface proto) " for protocol:"
                         (:var proto)))))
    (doseq [[fn-name f] mmap]
      (let [fn-sym (symbol (name fn-name))
            cns (vars/current-ns-name)
            env @(:env ctx)
            multi-method-var (get-in env [:namespaces cns fn-sym])
            multi-method @multi-method-var]
        (mms/multi-fn-add-method-impl multi-method atype f)))
    #_(-reset-methods (vars/alter-var-root (:var proto) assoc-in [:impls atype] mmap))))

(defn extend-type [_ _ _ctx type & proto+meths]
  (let [proto+meths (utils/split-when #(not (seq? %)) proto+meths)]
    `(do ~@(map (fn [[_proto & meths]]
                  `(do
                     ~@(map (fn [meth]
                              `(defmethod ~(first meth) ~type ~(second meth) ~@(nnext meth)))
                            meths))) proto+meths))))

(defn reify [_ _ _ctx & args]
  (prn args)
  ",")
