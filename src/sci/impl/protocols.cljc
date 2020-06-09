(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend-protocol
                            extend extend-type reify satisfies?
                            extends? implements?])
  (:require [sci.impl.multimethods :as mms]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(clojure.core/defprotocol IReified
  (getInterface [_])
  (getMethods [_]))

(deftype Reified [interface meths]
  IReified
  (getInterface [_] interface)
  (getMethods [_] meths))

(defn defprotocol [_ _ _ctx protocol-name & signatures]
  (let [[docstring signatures]
        (let [sig (first signatures)]
          (if (string? sig) [sig (rest signatures)]
              [nil signatures]))
        expansion
        `(do
           (def  ~(with-meta protocol-name
                    {:doc docstring}) {:methods #{}
                                       :ns *ns*})
           ~@(map (fn [[method-name & _]]
                    `(do
                       (defmulti ~method-name clojure.core/protocol-type-impl)
                       (defmethod ~method-name :sci.impl.protocols/reified [x# & args#]
                         (let [methods# (clojure.core/-reified-methods x#)]
                           (apply (get methods# '~method-name) x# args#)))
                       #?(:clj (alter-var-root (var ~protocol-name)
                                               update :methods conj ~method-name)
                          :cljs (def ~protocol-name
                                  (update ~protocol-name :methods conj ~method-name)))))
                  signatures))]
    ;; (prn expansion)
    expansion))

(defn extend-protocol [_ _ ctx protocol-name & impls]
  (let [impls (utils/split-when #(not (seq? %)) impls)
        protocol-var (@utils/eval-resolve-state ctx protocol-name)
        protocol-ns (-> protocol-var deref :ns)
        pns (str (vars/getName protocol-ns))
        fq-meth-name #(symbol pns %)
        expansion
        `(do ~@(map (fn [[type & meths]]
                      `(do
                         ~@(map (fn [meth]
                                  `(defmethod ~(fq-meth-name (str (first meth)))
                                     ~type
                                     ~(second meth) ~@(nnext meth)))
                                meths)))
                    impls))]
    #_(prn expansion)
    expansion))

(defn extend [ctx atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)
          :let [proto-ns (:ns proto)
                pns (vars/getName proto-ns)]]
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
            env @(:env ctx)
            multi-method-var (get-in env [:namespaces pns fn-sym])
            multi-method @multi-method-var]
        (mms/multi-fn-add-method-impl multi-method atype f))
      )
    #_(-reset-methods (vars/alter-var-root (:var proto) assoc-in [:impls atype] mmap))))

(defn extend-type [_ _ ctx type & proto+meths]
  (let [proto+meths (utils/split-when #(not (seq? %)) proto+meths)]
    `(do ~@(map (fn [[proto & meths]]
                  (let [protocol-var (@utils/eval-resolve-state ctx proto)
                        protocol-ns (-> protocol-var deref :ns)
                        pns (str (vars/getName protocol-ns))
                        fq-meth-name #(symbol pns %)]
                    `(do
                       ~@(map (fn [meth]
                                `(defmethod ~(fq-meth-name (str (first meth)))
                                   ~type ~(second meth) ~@(nnext meth)))
                              meths)))) proto+meths))))

(defn reify [_ _ _ctx interface & meths]
  (let [meths (into {} (map (fn [meth]
                              `['~(first meth) (fn ~(second meth) ~@(nnext meth))])
                            meths))]
    `(clojure.core/-reified ~interface ~meths)))

(defn type-impl [x & _xs]
  (or (when (instance? sci.impl.protocols.Reified x)
        :sci.impl.protocols/reified)
      (some-> x meta :sci.impl/type)
      (type x)))

(defn satisfies? [protocol obj]
  (boolean (some #(get-method % (type-impl obj)) (:methods protocol))))

(defn extends?
  "Returns true if atype extends protocol"
  [protocol atype]
  (boolean (some #(get-method % atype) (:methods protocol))))
