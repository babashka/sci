(ns sci.impl.fns
  {:no-doc true}
  (:require
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer [kw-identical?]])
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
          (let [~'invoc-array (when-not (zero? ~'invoc-size)
                                (object-array ~'invoc-size))]
            (when ~'enclosed->invocation
              (~'enclosed->invocation ~'enclosed-array ~'invoc-array))
            ~@(when varargs
                [`(aset ~'invoc-array ~'vararg-idx ~varargs-param)])
            (loop []
              (let [ret# (types/eval ~'body ~'ctx ~'invoc-array)]
                (if (kw-identical? :sci.impl.analyzer/recur ret#)
                  (recur)
                  ret#))))))
     (let [fn-params (vec (repeatedly n gensym))
           varargs-param (when varargs (gensym))
           asets `(do ~@(map (fn [fn-param idx]
                               `(aset ~(with-meta 'invoc-array
                                         {:tag 'objects}) ~idx ~fn-param))
                             fn-params (range)))]
       `(fn ~(symbol (str "arity-" n)) ~(cond-> fn-params
                                          varargs (conj '& varargs-param))
          (let [~'invoc-array (when-not (zero? ~'invoc-size)
                                (object-array ~'invoc-size))]
            (when ~'enclosed->invocation
              (~'enclosed->invocation ~'enclosed-array ~'invoc-array))
            ~asets
            ~@(when varargs
                [`(aset ~'invoc-array ~'vararg-idx ~varargs-param)])
            (loop []
              (let [ret# (types/eval ~'body ~'ctx ~'invoc-array)]
                (if (kw-identical? :sci.impl.analyzer/recur ret#)
                  (recur)
                  ret#)))))))))

#_(require '[clojure.pprint :as pprint])
#_(binding [*print-meta* true]
    (pprint/pprint (macroexpand '(gen-run-fn 2))))

#_{:clj-kondo/ignore [:unused-binding]}
(defn fun
  ([#?(:clj ^clojure.lang.Associative ctx :cljs ctx)
    enclosed-array
    fn-body
    fn-name
    macro?]
   (fun ctx enclosed-array fn-body fn-name macro?
        (:fixed-arity fn-body)
        (:copy-enclosed->invocation fn-body)
        (:body fn-body)
        (:invoc-size fn-body)
        (utils/current-ns-name)
        (:vararg-idx fn-body)))
  ([#?(:clj ^clojure.lang.Associative ctx :cljs ctx)
    enclosed-array
    fn-body
    fn-name
    macro?
    fixed-arity
    enclosed->invocation
    body
    invoc-size
    nsm vararg-idx]
   (let [f (if vararg-idx
             (case #?(:clj (int fixed-arity)
                      :cljs fixed-arity)
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
             (case #?(:clj (int fixed-arity)
                      :cljs fixed-arity)
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
     f)))

(defn lookup-by-arity [arities arity]
  (or (get arities arity)
      (:variadic arities)))

(defn fn-arity-map [ctx enclosed-array fn-name macro? fn-bodies]
  (reduce
   (fn [arity-map fn-body]
     (let [f (fun ctx enclosed-array fn-body fn-name macro?)
           var-arg? (:var-arg-name fn-body)
           fixed-arity (:fixed-arity fn-body)]
       (if var-arg?
         (assoc arity-map :variadic f)
         (assoc arity-map fixed-arity f))))
   {}
   fn-bodies))

;;;; Macros

(defn ^{:private true}
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

(defn fn**
  [form _ & sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (utils/throw-error-with-location
                  (if (seq sigs)
                    (str "Parameter declaration "
                         (first sigs)
                         " should be a vector")
                    (str "Parameter declaration missing"))
                  form)))
        psig (fn* [sig]
                  ;; Ensure correct type before destructuring sig
                  (when (not (seq? sig))
                    (throw (utils/throw-error-with-location
                            (str "Invalid signature " sig
                                 " should be a list")
                            form)))
                  (let [[params & body] sig
                        _ (when (not (vector? params))
                            (utils/throw-error-with-location
                             (if (seq? (first sigs))
                               (str "Parameter declaration " params
                                    " should be a vector")
                               (str "Invalid signature " sig
                                    " should be a list"))
                             form))
                        conds (when (and (next body) (map? (first body)))
                                (first body))
                        body (if conds (next body) body)
                        conds (or conds (meta params))
                        pre (:pre conds)
                        post (:post conds)
                        body (if post
                               `((let [~'% ~(if (< 1 (count body))
                                              `(do ~@body)
                                              (first body))]
                                   ~@(map (fn* [c] `(assert ~c)) post)
                                   ~'%))
                               body)
                        body (if pre
                               (concat (map (fn* [c] `(assert ~c)) pre)
                                       body)
                               body)]
                    (maybe-destructured params body)))
        new-sigs (map psig sigs)
        expr (with-meta
               (if name
                 (list* 'fn* name new-sigs)
                 (cons 'fn* new-sigs))
               (meta form))]
    expr))

(def
  ^{:private true}
  sigs
  (fn [fdecl]
    #_(assert-valid-fdecl fdecl)
    (let [asig
          (fn [fdecl]
            (let [arglist (first fdecl)
                                        ;elide implicit macro args
                  arglist (if (= '&form (first arglist))
                            (subvec arglist 2 (count arglist))
                            arglist)
                  body (next fdecl)]
              (if (map? (first body))
                (if (next body)
                  (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                  arglist)
                arglist)))
          #_#_resolve-tag (fn [argvec]
                            (let [m (meta argvec)
                                  ^clojure.lang.Symbol tag (:tag m)]
                              (if (instance? clojure.lang.Symbol tag)
                                (if (clojure.lang.Util/equiv (.indexOf (.getName tag) ".") -1)
                                  (if (clojure.lang.Util/equals nil (clojure.lang.Compiler$HostExpr/maybeSpecialTag tag))
                                    (let [c (clojure.lang.Compiler$HostExpr/maybeClass tag false)]
                                      (if c
                                        (with-meta argvec (assoc m :tag (clojure.lang.Symbol/intern (.getName c))))
                                        argvec))
                                    argvec)
                                  argvec)
                                argvec)))]
      (if (seq? (first fdecl))
        (loop [ret [] fdecls fdecl]
          (if fdecls
            (recur (conj ret (identity #_resolve-tag (asig (first fdecls)))) (next fdecls))
            (seq ret)))
        (list (identity #_resolve-tag (asig fdecl)))))))

(defn defn* [form _ name & fdecl]
  (if (symbol? name)
    nil
    (utils/throw-error-with-location "First argument to defn must be a symbol" form))
  (let [m (if (string? (first fdecl))
            {:doc (first fdecl)}
            {})
        fdecl (if (string? (first fdecl))
                (next fdecl)
                fdecl)
        m (if (map? (first fdecl))
            (conj m (first fdecl))
            m)
        fdecl (if (map? (first fdecl))
                (next fdecl)
                fdecl)
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        m (if (map? (last fdecl))
            (conj m (last fdecl))
            m)
        fdecl (if (map? (last fdecl))
                (butlast fdecl)
                fdecl)
        ;; deleted sigs here
        ;; _ (prn :fdecl fdecl)
        m (conj {:arglists (list 'quote (sigs fdecl))} m)
        ;; deleted inline here
        name-m (meta name)
        m (conj (if name-m name-m {}) m)
        macro? (:macro name-m)
        expr (cons `fn fdecl)
        expr (list 'def (with-meta name m)
                   (if (or macro? name)
                     (with-meta expr
                       {:sci.impl/fn {:macro macro?
                                      :fn-name name}})
                     expr))]
    expr))

(defn defmacro*
  [_&form _&env name & args]
  (let [name (vary-meta name assoc :macro true)
        prefix (loop [p (list name) args args]
                 (let [f (first args)]
                   (if (string? f)
                     (recur (cons f p) (next args))
                     (if (map? f)
                       (recur (cons f p) (next args))
                       p))))
        fdecl (loop [fd args]
                (if (string? (first fd))
                  (recur (next fd))
                  (if (map? (first fd))
                    (recur (next fd))
                    fd)))
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        add-implicit-args (fn [fd]
                            (let [args (first fd)]
                              (cons (vec (cons '&form (cons '&env args))) (next fd))))
        add-args (fn [acc ds]
                   (if (nil? ds)
                     acc
                     (let [d (first ds)]
                       (if (map? d)
                         (conj acc d)
                         (recur (conj acc (add-implicit-args d)) (next ds))))))
        fdecl (seq (add-args [] fdecl))
        decl (loop [p prefix d fdecl]
               (if p
                 (recur (next p) (cons (first p) d))
                 d))]
    (list 'do
          (cons `defn decl)
          (list 'var name))))

;;;; Scratch

(comment)
