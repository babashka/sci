(ns sci.impl.protocols
  {:no-doc true}
  (:refer-clojure :exclude [defprotocol extend extend-protocol -reset-methods])
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

(defn- emit-method-builder [on-interface method on-method arglists extend-via-meta]
  (let [methodk (keyword method)
        gthis (with-meta (gensym) {:tag 'clojure.lang.AFunction})
        ginterf (gensym)]
    `(fn [cache#]
       (let [~ginterf
             (fn
               ~@(map
                  (fn [args]
                    (let [gargs (map #(gensym (str "gf__" % "__")) args)
                          target (first gargs)]
                      `([~@gargs]
                        (. ~(with-meta target {:tag on-interface}) (~(or on-method method) ~@(rest gargs))))))
                  arglists))
             ^clojure.lang.AFunction f#
             (fn ~gthis
               ~@(map
                  (fn [args]
                    (let [gargs (map #(gensym (str "gf__" % "__")) args)
                          target (first gargs)]
                      (if extend-via-meta
                        `([~@gargs]
                          (let [cache# (.__methodImplCache ~gthis)
                                f# (.fnFor cache# (clojure.lang.Util/classOf ~target))]
                            (if (identical? f# ~ginterf)
                              (f# ~@gargs)
                              (if-let [meta# (when-let [m# (meta ~target)] ((.sym cache#) m#))]
                                (meta# ~@gargs)
                                (if f#
                                  (f# ~@gargs)
                                  ((-cache-protocol-fn ~gthis ~target ~on-interface ~ginterf) ~@gargs))))))
                        `([~@gargs]
                          (let [cache# (.__methodImplCache ~gthis)
                                f# (.fnFor cache# (clojure.lang.Util/classOf ~target))]
                            (if f#
                              (f# ~@gargs)
                              ((-cache-protocol-fn ~gthis ~target ~on-interface ~ginterf) ~@gargs)))))))
                  arglists))]
         (set! (.__methodImplCache f#) cache#)
         f#))))

(deftype MethodCache [method-sym protocol-map method-kw])

(defn -reset-methods [protocol]
  (doseq [[sci-var build] (:method-builders protocol)]
    (let [name (vars/getName sci-var)
          cache (->MethodCache name
                               protocol
                               (keyword name))]
      (vars/bindRoot sci-var (build cache)))))

(defn- emit-protocol [name opts+sigs]
  (let [iname name #_(symbol (str (munge (namespace-munge *ns*)) "." (munge name)))
        [opts sigs]
        (loop [opts {:on (list 'quote iname) :on-interface iname} sigs opts+sigs]
          (condp #(%1 %2) (first sigs)
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        sigs (when sigs
               (reduce (fn [m s]
                         (let [name-meta (meta (first s))
                               mname (with-meta (first s) nil)
                               [arglists doc]
                               (loop [as [] rs (rest s)]
                                 (if (vector? (first rs))
                                   (recur (conj as (first rs)) (next rs))
                                   [(seq as) (first rs)]))]
                           (when (some #{0} (map count arglists))
                             (throw (IllegalArgumentException. (str "Definition of function " mname " in protocol " name " must take at least one arg."))))
                           (when (m (keyword mname))
                             (throw (IllegalArgumentException. (str "Function " mname " in protocol " name " was redefined. Specify all arities in single definition."))))
                           (assoc m (keyword mname)
                                  (merge name-meta
                                         {:name (vary-meta mname assoc :doc doc :arglists arglists)
                                          :arglists arglists
                                          :doc doc}))))
                       {} sigs))
        meths (mapcat (fn [sig]
                        (let [m (munge (:name sig))]
                          (map #(vector m (vec (repeat (dec (count %)) 'Object)) 'Object)
                               (:arglists sig))))
                      (vals sigs))]
    `(do
       (defonce ~name {})
       ;; TODO:
       #_(gen-interface :name ~iname :methods ~meths)
       (alter-meta! (var ~name) assoc :doc ~(:doc opts))
       ;; TODO:
       #_~(when sigs
            `(#'assert-same-protocol (var ~name) '~(map :name (vals sigs))))
       (alter-var-root (var ~name)
                       merge
                       (assoc ~opts
                                          :sigs '~sigs
                                          :var (var ~name)
                                          :method-map
                                          ~(and (:on opts)
                                                (apply hash-map
                                                       (mapcat
                                                        (fn [s]
                                                          [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
                                                        (vals sigs))))
                                          :method-builders
                                          ~(apply hash-map
                                                  (mapcat
                                                   (fn [s]
                                                     [`(intern *ns* (with-meta '~(:name s) (merge '~s {:protocol (var ~name)})))
                                                      ;; TODO:
                                                      nil
                                                      #_(emit-method-builder (:on-interface opts) (:name s) (:on s) (:arglists s)
                                                                             (:extend-via-metadata opts))])
                                                   (vals sigs)))))
       ;; TODO:
       #_(-reset-methods ~name)
       '~name)))

(defn defprotocol2 [_ _ name & opts+sigs]
  (emit-protocol name opts+sigs))

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

(defn- protocol?
  [maybe-p]
  (boolean (:on-interface maybe-p)))

#_(defn- implements? [protocol atype]
  (prn protocol (:on-interface protocol) (type (:on-interface protocol)))
  #?(:clj (and atype (.isAssignableFrom ^Class (:on-interface protocol) atype))))

(defn extend [atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)]
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
