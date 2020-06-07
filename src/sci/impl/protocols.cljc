(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend extend-protocol
                            -reset-methods -cache-protocol-fn
                            find-protocol-method find-protocol-impl])
  (:require [sci.impl.vars :as vars]))

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
  (let [expansion `(do ~@(map (fn [[method-name & _]]
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

(defn extend-protocol [_ _ ctx protocol-name & pairs]
  (let [expansion
        `(do ~@(map (fn [[type impl]]
                      `(defmethod ~(first impl) ~type ~(second impl) ~@(nnext impl)))
                    (partition 2 pairs)))]
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

(defn extend [atype & proto+mmaps]
  #_(doseq [[proto mmap] (partition 2 proto+mmaps)]
    (when-not (protocol? proto)
      (throw (new #?(:clj IllegalArgumentException
                     :cljs js/Error)
                  (str proto " is not a protocol"))))
    #_(when (implements? proto atype)
      (throw (new #?(:clj IllegalArgumentException
                     :cljs js/Error)
                  (str atype " already directly implements " (:on-interface proto) " for protocol:"
                       (:var proto)))))
    (-reset-methods (vars/alter-var-root (:var proto) assoc-in [:impls atype] mmap))))
