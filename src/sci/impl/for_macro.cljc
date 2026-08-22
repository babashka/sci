(ns sci.impl.for-macro
  {:no-doc true}
  (:require [sci.impl.utils :refer [allowed-loop allowed-recur
                                    rethrow-driver-error
                                    throw-error-with-location]]))

;; based on the source of clojure.core/for

(defn assert-args [expr seq-exprs _body-expr]
  (let [arg-count (dec (count expr))]
    (when-not (= 2 arg-count)
      (throw-error-with-location (str "Wrong number of args (" arg-count ") passed to: clojure.core/for")
                                 expr)))
  (when-not (vector? seq-exprs)
    (throw-error-with-location "for requires a vector for its binding"
                               expr))
  (when-not (even? (count seq-exprs))
    (throw-error-with-location "for requires an even number of forms in binding vector"
                               expr)))

;; sentinels for the driver expansion below; unique objects that user code
;; cannot produce, so a body value can never be mistaken for one
(def skip #?(:cljd (Object.) :clj (Object.) :cljs (js-obj)))
(def stop #?(:cljd (Object.) :clj (Object.) :cljs (js-obj)))

(defn seq-guard
  ;; seq creation on the driven collection happens in compiled code; a
  ;; failure there must still report the location of the for/doseq form
  [coll loc]
  (try (seq coll)
       (catch #?(:cljd Object :clj Throwable :cljs :default) e
         (rethrow-driver-error e loc))))

(defn for-driver
  ;; innermost level: lazy chunk-aware map of f over coll, f may return
  ;; skip (:when failed) or stop (:while failed)
  [coll f loc]
  (let [step (fn step [s]
               (lazy-seq
                (when-let [s (seq s)]
                  (if (chunked-seq? s)
                    (let [c (chunk-first s)
                          n (count c)
                          b (chunk-buffer n)
                          stopped? (loop [i 0]
                                     (if (< i n)
                                       (let [v (f (nth c i))]
                                         (cond (identical? skip v) (recur (inc i))
                                               (identical? stop v) true
                                               :else (do (chunk-append b v)
                                                         (recur (inc i)))))
                                       false))]
                      (chunk-cons (chunk b) (when-not stopped?
                                              (step (chunk-rest s)))))
                    (let [v (f (first s))]
                      (cond (identical? skip v) (step (rest s))
                            (identical? stop v) nil
                            :else (cons v (step (rest s)))))))))]
    (lazy-seq (step (seq-guard coll loc)))))

(defn for-driver-cat
  ;; outer level: f returns the (already lazy) inner seq per element,
  ;; concatenated lazily
  [coll f loc]
  (let [step (fn step [s]
               (lazy-seq
                (when-let [s (seq s)]
                  (let [v (f (first s))]
                    (cond (identical? skip v) (step (rest s))
                          (identical? stop v) nil
                          :else (concat v (step (rest s))))))))]
    (lazy-seq (step (seq-guard coll loc)))))

(defn expand-for-driver
  [expr seq-exprs body-expr]
  (let [loc (meta expr)
        to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (new #?(:cljd ArgumentError
                                       :clj IllegalArgumentException
                                       :cljs js/Error) ^String (apply str msg))))
        emit (fn emit [[[bind coll & mod-pairs] & next-groups]]
               (let [inner (if next-groups
                             (emit next-groups)
                             body-expr)
                     do-mod (fn do-mod [[[k v] & etc :as pairs]]
                              (cond
                                (not (seq pairs)) inner
                                (= :let k) `(let ~v ~(do-mod etc))
                                (= :while k) `(if ~v ~(do-mod etc) ~stop)
                                (= :when k) `(if ~v ~(do-mod etc) ~skip)
                                (keyword? k) (err "Invalid 'for' keyword " k)
                                :else inner))
                     driver (if next-groups
                              `clojure.core/for-driver-cat
                              `clojure.core/for-driver)
                     thunk-body (do-mod (seq mod-pairs))]
                 (with-meta
                   (if (simple-symbol? bind)
                     `(~driver ~coll (fn* [~bind] ~thunk-body) ~loc)
                     (let [g (gensym "x_")]
                       `(~driver ~coll (fn* [~g] ~(with-meta
                                                    `(let [~bind ~g] ~thunk-body)
                                                    loc)) ~loc)))
                   loc)))]
    (emit (to-groups seq-exprs))))

;; see clojurescript core.cljc defmacro for
(defn expand-for-loop
  [expr seq-exprs body-expr]
  (let [loc (meta expr)
        to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (new #?(:cljd ArgumentError
                                       :clj IllegalArgumentException
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
                                                          (#?(:cljd ~'unchecked-inc :default unchecked-inc) ~gi)))
                                          (keyword? k)
                                          (err "Invalid 'for' keyword " k)
                                          :else
                                          `(do (chunk-append ~gb ~body-expr)
                                               (~allowed-recur (#?(:cljd ~'unchecked-inc :default unchecked-inc) ~gi)))))
                              c-sym (gensym "c")]
                          `(fn ~giter [~gxs]
                             (lazy-seq
                               (~allowed-loop [~gxs ~gxs]
                                (let [~gxs ~(with-meta `(seq ~gxs)
                                              loc)]
                                   (when ~gxs
                                     (if (chunked-seq? ~gxs)
                                       (let [~c-sym (chunk-first ~gxs)
                                             size# (int (count ~c-sym))
                                             ~gb (chunk-buffer size#)]
                                         (if (~allowed-loop [~gi (int 0)]
                                              (if (< ~gi size#)
                                                ~(with-meta
                                                   `(let [~bind (nth ~c-sym ~gi)]
                                                      ~(do-cmod mod-pairs))
                                                   loc)
                                                true))
                                           (chunk-cons
                                            (chunk ~gb)
                                            (~giter (chunk-rest ~gxs)))
                                           (chunk-cons (chunk ~gb) nil)))
                                       ~(with-meta
                                          `(let [~bind (first ~gxs)]
                                             ~(do-mod mod-pairs))
                                          loc)))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# ~(second seq-exprs)))))

(defn expand-for
  [expr _ seq-exprs body-expr]
  (assert-args expr seq-exprs body-expr)
  (if (seq seq-exprs)
    (expand-for-driver expr seq-exprs body-expr)
    ;; degenerate (for [] ...): keep the old expansion's behavior
    (expand-for-loop expr seq-exprs body-expr)))
