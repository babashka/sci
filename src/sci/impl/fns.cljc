(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.evaluator :as eval]
            [sci.impl.faster :refer [nth-2]]
            [sci.impl.types :as types]
            [sci.impl.utils :as utils :refer [kw-identical?]]
            [sci.impl.vars :as vars]
            )
  #?(:cljs (:require-macros [sci.impl.fns :refer [gen-fn]])))

#?(:clj (set! *warn-on-reflection* true))

(defmacro gen-fn
  ([n]
   `(gen-fn ~n false))
  ([n disable-arity-checks]
   `(gen-fn ~n ~disable-arity-checks false))
  ([n _disable-arity-checks varargs]
   (if (zero? n)
     (let [varargs-param (when varargs (gensym))]
       `(fn ~'arity-0 ~(cond-> []
                         varargs (conj '& varargs-param))
          (let [~'invoc-array (object-array ~'invoc-size)]
            (when ~'enclosed->invocation
              (~'enclosed->invocation ~'enclosed-array ~'invoc-array))
            ~@(when varargs
                [`(aset ~'invoc-array ~'vararg-idx ~varargs-param)])
            (loop []
              (let [ret# (types/eval ~'body ~'ctx ~'invoc-array)]
                (if (kw-identical? :sci.impl.analyzer/recur ret#)
                  (recur)
                  ret#))))))
     (let [locals (repeatedly n gensym)
           fn-params (vec (repeatedly n gensym))
           varargs-param (when varargs (gensym))
           rnge (range n)
           nths (map (fn [n] `(nth-2 ~'params ~n)) rnge)
           let-vec (vec (mapcat (fn [local ith]
                                  [local ith]) locals nths))
           asets `(do ~@(map (fn [fn-param idx]
                               `(aset ~(with-meta 'invoc-array
                                         {:tag 'objects}) ~idx ~fn-param))
                             fn-params (range)))]
       `(let ~let-vec
          (fn ~(symbol (str "arity-" n)) ~(cond-> fn-params
                                            varargs (conj '& varargs-param))
            (let [~'invoc-array (object-array ~'invoc-size)]
              (when ~'enclosed->invocation
                (~'enclosed->invocation ~'enclosed-array ~'invoc-array))
              ~asets
              ~@(when varargs
                  [`(aset ~'invoc-array ~'vararg-idx ~varargs-param)])
              (loop []
                (let [ret# (types/eval ~'body ~'ctx ~'invoc-array)]
                  (if (kw-identical? :sci.impl.analyzer/recur ret#)
                    (recur)
                    ret#))))))))))

#_(require '[clojure.pprint :as pprint])
#_(binding [*print-meta* true]
    (pprint/pprint (macroexpand '(gen-run-fn 2))))

(defn fun
  [#?(:clj ^clojure.lang.Associative ctx :cljs ctx)
   enclosed-array
   bindings
   fn-body
   #_:clj-kondo/ignore fn-name
   #_:clj-kondo/ignore macro?]
  (let [#_:clj-kondo/ignore
        fixed-arity (:fixed-arity fn-body)
        enclosed->invocation (:copy-enclosed->invocation fn-body)
        var-arg-name (:var-arg-name fn-body)
        #_:clj-kondo/ignore
        params (:params fn-body)
        body (:body fn-body)
        invoc-size (:invoc-size fn-body)
        self-ref-idx (:self-ref-idx fn-body)
        #_:clj-kondo/ignore nsm (vars/current-ns-name)
        vararg-idx (:vararg-idx fn-body)
        f (if vararg-idx
            (case (int fixed-arity)
              0 (gen-fn 0 true true)
              1 (gen-fn 1 true true)
              2 (gen-fn 2 true true)
              3 (gen-fn 3 true true)
              4 (gen-fn 4 true true)
              5 (gen-fn 5 true true)
              6 (gen-fn 6 true true)
              7 (gen-fn 7 true true)
              8 (gen-fn 8 true true)
              9 (gen-fn 9 true true)
              10 (gen-fn 10 true true)
              11 (gen-fn 11 true true)
              12 (gen-fn 12 true true)
              13 (gen-fn 13 true true)
              14 (gen-fn 14 true true)
              15 (gen-fn 15 true true)
              16 (gen-fn 16 true true)
              17 (gen-fn 17 true true)
              18 (gen-fn 18 true true)
              19 (gen-fn 19 true true)
              20 (gen-fn 20 true true))
            (case (int fixed-arity)
              0 (gen-fn 0)
              1 (gen-fn 1)
              2 (gen-fn 2)
              3 (gen-fn 3)
              4 (gen-fn 4)
              5 (gen-fn 5)
              6 (gen-fn 6)
              7 (gen-fn 7)
              8 (gen-fn 8)
              9 (gen-fn 9)
              10 (gen-fn 10)
              11 (gen-fn 11)
              12 (gen-fn 12)
              13 (gen-fn 13)
              14 (gen-fn 14)
              15 (gen-fn 15)
              16 (gen-fn 16)
              17 (gen-fn 17)
              18 (gen-fn 18)
              19 (gen-fn 19)
              20 (gen-fn 20)))]
    f))

(defn lookup-by-arity [arities arity]
  (or (get arities arity)
      (:variadic arities)))

(defn fn-arity-map [ctx enclosed-array bindings fn-name macro? fn-bodies]
  (reduce
   (fn [arity-map fn-body]
     (let [f (fun ctx enclosed-array bindings fn-body fn-name macro?)
           var-arg? (:var-arg-name fn-body)
           fixed-arity (:fixed-arity fn-body)]
       (if var-arg?
         (assoc arity-map :variadic f)
         (assoc arity-map fixed-arity f))))
   {}
   fn-bodies))

(defn eval-fn [ctx bindings fn-name fn-bodies macro? single-arity self-ref? bindings-fn]
  (let [;; each evaluated fn should have its own self-ref!
        enclosed-array (bindings-fn bindings)
        f (if single-arity
            (fun ctx enclosed-array bindings single-arity fn-name macro?)
            (let [arities (fn-arity-map ctx enclosed-array bindings fn-name macro? fn-bodies)]
              (fn [& args]
                (let [arg-count (count args)]
                  (if-let [f (lookup-by-arity arities arg-count)]
                    (apply f args)
                    (throw (new #?(:clj Exception
                                   :cljs js/Error)
                                (let [actual-count (if macro? (- arg-count 2)
                                                       arg-count)]
                                  (str "Cannot call " fn-name " with " actual-count " arguments")))))))))
        f (if macro?
            (vary-meta f
                       #(assoc %
                               :sci/macro macro?
                               ;; added for better error reporting
                               :sci.impl/inner-fn f))
            f)]
    (when self-ref?
      (aset ^objects enclosed-array (dec (count enclosed-array)) f))
    f))

(vreset! utils/eval-fn eval-fn)

;;;; Scratch

(comment
  )
