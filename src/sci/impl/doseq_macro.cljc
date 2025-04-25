(ns sci.impl.doseq-macro
  {:no-doc true}
  (:require [sci.impl.utils :refer [allowed-loop allowed-recur]]))

;; based on the source of clojure.core/doseq

(defn assert-args [seq-exprs _body-exprs]
  (when-not (vector? seq-exprs)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "doseq requires a vector for its binding")))
  (when-not (even? (count seq-exprs))
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "doseq requires an even number of forms in binding vector"))))

(defn expand-doseq
  [expr _ seq-exprs & body]
  (assert-args seq-exprs body)
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
                           `(~allowed-recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
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
