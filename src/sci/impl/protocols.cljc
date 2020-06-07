(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend extend-protocol
                            -reset-methods -cache-protocol-fn
                            find-protocol-method find-protocol-impl])
  (:require [sci.impl.vars :as vars]
            [sci.impl.multimethods :as mms]
            [sci.impl.utils :as utils]))

;; user=> (defprotocol P (feed [_]))
;; P
;; user=> (meta #'P)
;; {:line 1, :column 1, :file "NO_SOURCE_PATH", :name P, :ns #object[clojure.lang.Namespace 0xc6e0f32 "user"], :doc nil}
;; user=> (meta #'feed)
;; {:name feed, :arglists ([_]), :doc nil, :protocol #'user/P, :ns #object[clojure.lang.Namespace 0xc6e0f32 "user"]}

;; (defn -reset-methods [protocol]
;;   (doseq [[^clojure.lang.Var v build] (:method-builders protocol)]
;;     (let [cache (clojure.lang.MethodImplCache. (symbol v) protocol (keyword (.sym v)))]
;;       (.bindRoot v (build cache)))))

;; user=> (:method-builders P)
;; {#'user/feed #object[user$eval145$fn__146 0x6e57b5e9 "user$eval145$fn__146@6e57b5e9"]}

(defn defprotocol [_ _ ctx protocol-name & signatures]
  (let [expansion `(do
                     (def ~protocol-name {})
                     ~@(map (fn [[method-name & _]]
                              `(defmulti ~method-name clojure.core/protocol-type-impl))
                            signatures))]
    ;; (prn expansion)
    expansion))

#_(defn- assert-same-protocol [protocol-var method-syms]
    (doseq [m method-syms]
      (let [v (resolve m)
            p (:protocol (meta v))]
        (when (and v #_(bound? v) (not= protocol-var p))
          (binding [*out* *err*]
            (println "Warning: protocol" protocol-var "is overwriting"
                     (if p
                       (str "method " (.sym v) " of protocol " (.sym p))
                       (str "function " (.sym v)))))))))

(defn extend-protocol [_ _ ctx protocol-name & impls]
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

;; (defprotocol Dateable
;;   (to-ms [t]))

;; (extend java.lang.Number
;;   Dateable
;;   {:to-ms identity})

;; user=> (macroexpand '(extend-protocol Foo String (foo [this x] (inc x))))
;; (do (clojure.core/extend-type String Foo (foo [this x] (inc x))))
;; user=> (macroexpand '(clojure.core/extend-type String Foo (foo [this x] (inc x))))
;; (clojure.core/extend String Foo {:foo (clojure.core/fn ([this x] (inc x)))})

#_(defn- implements? [protocol atype]
  (prn protocol (:on-interface protocol) (type (:on-interface protocol)))
  #?(:clj (and atype (.isAssignableFrom ^Class (:on-interface protocol) atype))))

(defn extend [ctx atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)]
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
