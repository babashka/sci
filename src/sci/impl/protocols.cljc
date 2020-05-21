(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend-protocol]))

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
                                `(defmulti ~method-name type))
                              signatures))]
    ;; (prn expansion)
    expansion))

(defn extend-protocol [_ _ ctx protocol-name & pairs]
  (let [expansion
        `(do ~@(map (fn [[type impl]]
                      `(defmethod ~(first impl) ~type ~(second impl) ~@(nnext impl)))
                    (partition 2 pairs)))]
    #_(prn expansion)
    expansion))
