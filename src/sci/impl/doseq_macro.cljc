(ns sci.impl.doseq-macro
  {:no-doc true}
  (:require [sci.impl.utils :refer [allowed-loop allowed-recur
                                    rethrow-driver-error]]))

;; based on the source of clojure.core/doseq

(defn assert-args [seq-exprs _body-exprs]
  (when-not (vector? seq-exprs)
    (throw (new #?(:cljd ArgumentError :clj IllegalArgumentException :cljs js/Error)
                "doseq requires a vector for its binding")))
  (when-not (even? (count seq-exprs))
    (throw (new #?(:cljd ArgumentError :clj IllegalArgumentException :cljs js/Error)
                "doseq requires an even number of forms in binding vector"))))

(defn doseq-driver
  ;; iteration driver compiled into the host image: doseq expansions call
  ;; this instead of interpreting the chunked-seq loop scaffold
  [coll f loc]
  (try (reduce (fn [_ x] (f x) nil) nil coll)
       (catch #?(:cljd Object :clj Throwable :cljs :default) e
         (rethrow-driver-error e loc)))
  nil)

(defn expand-doseq-driver
  [expr seq-exprs body]
  (let [loc (meta expr)
        step (fn step [exprs]
               (if-not exprs
                 `(~'do ~@body)
                 (let [k (first exprs)
                       v (second exprs)
                       subform (step (nnext exprs))]
                   (if (keyword? k)
                     (cond
                       (= :let k) `(let ~v ~subform)
                       (= :when k) `(~'when ~v ~subform))
                     (with-meta
                       (if (simple-symbol? k)
                         `(~doseq-driver ~v (fn* [~k] ~subform) ~loc)
                         (let [g (gensym "x_")]
                           `(~doseq-driver ~v (fn* [~g] ~(with-meta
                                                           `(let [~k ~g] ~subform)
                                                           loc)) ~loc)))
                       loc)))))]
    (step (seq seq-exprs))))

(defn expand-doseq-loop
  [expr seq-exprs body]
  (let [loc (meta expr)
        step (fn step [recform exprs]
               (if-not exprs
                 [true `(~'do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(~'when ~v
                                               ~subform
                                               ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     (let [seq- (gensym "seq_")
                           chunk- (gensym "chunk_")
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(~allowed-recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk
                           `(~allowed-recur ~seq- ~chunk- ~count- (#?(:cljd ~'unchecked-inc :default unchecked-inc) ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(~allowed-loop [~seq- ~(with-meta `(seq ~v)
                                                  loc) ~chunk- nil,
                                         ~count- 0, ~i- 0]
                          (if (< ~i- ~count-)
                            ~(with-meta
                               `(let [~k (nth ~chunk- ~i-)]
                                  ~subform-chunk
                                  ~@(when needrec [recform-chunk]))
                               loc)
                            (let [~seq- (seq ~seq-)]
                              (~'when ~seq-
                               (if (chunked-seq? ~seq-)
                                 (let [c# (chunk-first ~seq-)]
                                   (~allowed-recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))

                                 ~(with-meta
                                    `(let [~k (first ~seq-)]
                                       ~subform
                                       ~@(when needrec [recform]))
                                    loc))))))])))))
        ret (nth (step nil (seq seq-exprs)) 1)]
    ;; (prn :ret ret)
    ret))

(defn expand-doseq
  [expr _ seq-exprs & body]
  (assert-args seq-exprs body)
  ;; :while needs early exit from the driver's reduce; keep the loop
  ;; expansion for that case
  (if (some #(= :while %) (take-nth 2 seq-exprs))
    (expand-doseq-loop expr seq-exprs body)
    (expand-doseq-driver expr seq-exprs body)))
