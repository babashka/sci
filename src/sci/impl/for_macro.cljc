(ns sci.impl.for-macro
  {:no-doc true}
  (:require [sci.impl.utils :refer [allowed-loop allowed-recur]]))

;; based on the source of clojure.core/for

(defn assert-args [seq-exprs _body-expr]
  (when-not (vector? seq-exprs)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "for requires a vector for its binding")))
  (when-not (even? (count seq-exprs))
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "for requires an even number of forms in binding vector"))))

;; see clojurescript core.cljc defmacro for
(defn expand-for
  [_ [_ seq-exprs body-expr]]
  (assert-args seq-exprs body-expr)
  (let [to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (new #?(:clj IllegalArgumentException
                                       :cljs js/Error) ^String (apply str msg))))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (~allowed-recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                     `(let [iterys# ~(emit-bind next-groups)
                                            fs# (seq (iterys# ~next-expr))]
                                        (if fs#
                                          (concat fs# (~giter (rest ~gxs)))
                                          (~allowed-recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs]
                           (lazy-seq
                            (~allowed-loop [~gxs ~gxs]
                             (when-first [~bind ~gxs]
                               ~(do-mod mod-pairs)))))
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (~allowed-recur
                                                          (unchecked-inc ~gi)))
                                          (keyword? k)
                                          (err "Invalid 'for' keyword " k)
                                          :else
                                          `(do (chunk-append ~gb ~body-expr)
                                               (~allowed-recur (unchecked-inc ~gi)))))]
                          `(fn ~giter [~gxs]
                             (lazy-seq
                              (~allowed-loop [~gxs ~gxs]
                               (let [~gxs (seq ~gxs)]
                                 (when ~gxs
                                  (if (chunked-seq? ~gxs)
                                    (let [c# (chunk-first ~gxs)
                                          size# (int (count c#))
                                          ~gb (chunk-buffer size#)]
                                      (if (~allowed-loop [~gi (int 0)]
                                           (if (< ~gi size#)
                                             (let [~bind (nth c# ~gi)]
                                               ~(do-cmod mod-pairs))
                                             true))
                                        (chunk-cons
                                         (chunk ~gb)
                                         (~giter (chunk-rest ~gxs)))
                                        (chunk-cons (chunk ~gb) nil)))
                                    (let [~bind (first ~gxs)]
                                      ~(do-mod mod-pairs))))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# ~(second seq-exprs)))))
