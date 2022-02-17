(ns sci.impl.analyzer
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   [clojure.string :as str]
   #?(:cljs [goog.object :as gobj])
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.evaluator :as eval]
   [sci.impl.faster :as faster]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.load :as load]
   [sci.impl.records :as records]
   [sci.impl.resolve :as resolve]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer
    [ana-macros constant? ctx-fn kw-identical? macro?
     maybe-destructured rethrow-with-location-of-node set-namespace!]]
   [sci.impl.vars :as vars]
   #?(:cljs [cljs.tagged-literals :refer [JSValue]]))
  #?(:clj (:import [sci.impl Reflector]))
  #?(:cljs
     (:require-macros
      [sci.impl.analyzer :refer [gen-return-do
                                 gen-return-or
                                 gen-return-and
                                 gen-return-recur
                                 gen-return-binding-call
                                 gen-return-needs-ctx-call
                                 gen-return-call]])))

(defn recur-target [ctx]
  (:recur-target ctx))

(defn with-recur-target [ctx v]
  (assoc ctx :recur-target v))

(defn without-recur-target [ctx]
  (assoc ctx :recur-target false))

(defn recur-target? [ctx]
  (:recur-target ctx))

#?(:clj (set! *warn-on-reflection* true))

;; derived from (keys (. clojure.lang.Compiler specials))
;; (& monitor-exit case* try reify* finally loop* do letfn* if clojure.core/import* new deftype* let* fn* recur set! . var quote catch throw monitor-enter def)
(def special-syms '#{try finally do if new recur quote catch throw def . var set!})

(defn- throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(declare analyze analyze-children analyze-call return-call return-map)

;;;; Macros

(defn macroexpand-1 [ctx expr]
  (let [ctx (assoc ctx :sci.impl/macroexpanding true)
        original-expr expr]
    (if (seq? expr)
      (let [op (first expr)]
        (if (symbol? op)
          (cond (get special-syms op) expr
                (contains? #{'for} op) (analyze ctx expr)
                (= 'clojure.core/defrecord op) expr
                :else
                (let [f (try (resolve/resolve-symbol ctx op true)
                             (catch #?(:clj Exception :cljs :default)
                                 _ ::unresolved))]
                  (if (kw-identical? ::unresolved f)
                    expr
                    (let [macro-var? (and (vars/var? f)
                                          (vars/isMacro f))
                          needs-ctx? (identical? utils/needs-ctx (some-> f meta :sci.impl/op))
                          f (if macro-var? @f f)]
                      (if (or macro-var? (macro? f))
                        (if needs-ctx?
                          (apply f original-expr (:bindings ctx) ctx (rest expr))
                          (apply f original-expr (:bindings ctx) (rest expr)))
                        expr)))))
          expr))
      expr)))

(defn macroexpand
  [ctx form]
  (let [ex (macroexpand-1 ctx form)]
    (if (identical? ex form)
      form
      (macroexpand ctx ex))))

(vreset! utils/macroexpand* macroexpand)
(vreset! utils/macroexpand-1* macroexpand-1)

;;;; End macros

(defn analyze-children-tail [ctx children]
  (let [rt (recur-target ctx)
        non-tail-ctx (without-recur-target ctx)
        analyzed-children-non-tail (mapv #(analyze non-tail-ctx %) (butlast children))
        ret-child (analyze (with-recur-target ctx rt) (last children))]
    (conj analyzed-children-non-tail ret-child)))

(defmacro gen-return-do
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth (deref ~'analyzed-children) ~j)])
                                            (range i)))])
                          (range 2 4))]
    `(defn ~'return-do
       ~'[ctx expr children]
       (let [~'analyzed-children (delay (analyze-children-tail ~'ctx ~'children))]
         (case (count ~'children)
           ~@(concat
              [0 nil]
              [1 `(nth (deref ~'analyzed-children) 0)]
              (mapcat (fn [[i binds]]
                        [i `(let ~binds
                              (ctx-fn
                               (fn [~'ctx ~'bindings]
                                 ~@(map (fn [j]
                                          `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                        (range i)))
                               ~'expr))])
                      let-bindings)
              `[(ctx-fn
                 (let [~'analyzed-children (deref ~'analyzed-children)]
                   (fn [~'ctx ~'bindings]
                     (eval/eval-do ~'ctx ~'bindings ~'analyzed-children)))
                 ~'expr)]))))))

;; (require '[clojure.pprint :refer [pprint]])
;; (pprint (clojure.core/macroexpand '(gen-return-do)))

(declare return-do) ;; for clj-kondo
(gen-return-do)

(defmacro gen-return-or
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth (deref ~'analyzed-children) ~j)])
                                            (range i)))])
                          (range 2 20))]
    `(defn ~'return-or
       ~'[ctx expr children]
       (let [~'analyzed-children (delay (analyze-children-tail ~'ctx ~'children))]
         (case (count ~'children)
           ~@(concat
              [0 nil]
              [1 `(analyze ~'ctx (first ~'children))]
              (mapcat (fn [[i binds]]
                        [i `(let ~binds
                              (ctx-fn
                               (fn [~'ctx ~'bindings]
                                 (or
                                  ~@(map (fn [j]
                                           `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                         (range i))))
                               ~'expr))])
                      let-bindings)
              `[(ctx-fn
                 (let [~'analyzed-children (deref ~'analyzed-children)]
                   (fn [~'ctx ~'bindings]
                     (eval/eval-or ~'ctx ~'bindings ~'analyzed-children)))
                 ~'expr)]))))))

(declare return-or) ;; for clj-kondo
(gen-return-or)

(defmacro gen-return-and
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth (deref ~'analyzed-children) ~j)])
                                            (range i)))])
                          (range 2 20))]
    `(defn ~'return-and
       ~'[ctx expr children]
       (let [~'analyzed-children (delay (analyze-children-tail ~'ctx ~'children))]
         (case (count ~'children)
           ~@(concat
              [0 nil]
              [1 `(analyze ~'ctx (first ~'children))]
              (mapcat (fn [[i binds]]
                        [i `(let ~binds
                              (ctx-fn
                               (fn [~'ctx ~'bindings]
                                 (and
                                  ~@(map (fn [j]
                                           `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                         (range i))))
                               ~'expr))])
                      let-bindings)
              `[(ctx-fn
                 (let [~'analyzed-children (deref ~'analyzed-children)]
                   (fn [~'ctx ~'bindings]
                     (eval/eval-and ~'ctx ~'bindings ~'analyzed-children)))
                 ~'expr)]))))))

(declare return-and) ;; for clj-kondo
(gen-return-and)

(defmacro gen-return-recur
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)
                                               (symbol (str "param" j))
                                               `(nth ~'params ~j)])
                                            (range i)))])
                          (range 1 20))]
    `(defn ~'return-recur
       ~'[ctx expr analyzed-children]
       (when-not (recur-target? ~'ctx)
         (throw-error-with-location "Can only recur from tail position" ~'expr))
       (let [~'params (:params ~'ctx)]
         (case (count ~'analyzed-children)
           ~@(concat
              [0 `(ctx-fn
                   (fn [~'_ ~'bindings]
                     ::recur)
                   ~'expr)]
              (mapcat (fn [[i binds]]
                        [i `(let ~binds
                              (ctx-fn
                               (fn [~'ctx ~'bindings]
                                 ;; important, recur vals must be evaluated with old bindings!
                                 (let [~@(mapcat (fn [j]
                                                   [(symbol (str "eval-" j) )
                                                    `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j)))])
                                                 (range i))]
                                   (do ~@(map (fn [j]
                                                `(aset
                                                  ~(with-meta 'bindings
                                                     {:tag 'objects}) ~j
                                                  ~(symbol (str "eval-" j))))
                                              (range i))))
                                 ::recur)
                               ~'expr))])
                      let-bindings)))))))

;; (require 'clojure.pprint)
;; (clojure.pprint/pprint
;;  (clojure.core/macroexpand '(gen-return-recur)))

(declare return-recur) ;; for clj-kondo
(gen-return-recur)

(defn analyze-children [ctx children]
  (mapv #(analyze ctx %) children))

(defrecord FnBody [params body fixed-arity var-arg-name self-ref-idx iden->invoke-idx])

(declare update-parents)

(defn expand-fn-args+body [{:keys [:fn-expr] :as ctx} [binding-vector & body-exprs] macro? fn-name fn-id]
  (when-not binding-vector
    (throw-error-with-location "Parameter declaration missing." fn-expr))
  (when-not (vector? binding-vector)
    (throw-error-with-location "Parameter declaration should be a vector" fn-expr))
  (let [binding-vector (if macro? (into ['&form '&env] binding-vector)
                           binding-vector)
        next-body (next body-exprs)
        conds (when next-body
                (let [e (first body-exprs)]
                  (when (map? e) e)))
        body-exprs (if conds next-body body-exprs)
        conds (or conds (meta binding-vector))
        pre (:pre conds)
        post (:post conds)
        body-exprs (if post
                     `((let [~'% ~(if (< 1 (count body-exprs))
                                    `(do ~@body-exprs)
                                    (first body-exprs))]
                         ~@(map (fn* [c] `(assert ~c)) post)
                         ~'%))
                     body-exprs)
        body-exprs (if pre
                     (concat (map (fn* [c] `(assert ~c)) pre)
                             body-exprs)
                     body-exprs)
        {:keys [:params :body]} (maybe-destructured binding-vector body-exprs)
        [fixed-args [_ var-arg-name]] (split-with #(not= '& %) params)
        fixed-args (vec fixed-args)
        fixed-arity (count fixed-args)
        ;; param-names = all simple symbols, no destructuring
        param-names (cond-> fixed-args
                      var-arg-name (conj var-arg-name))
        ctx (assoc ctx :params param-names)
        param-count (count param-names)
        param-idens (repeatedly param-count gensym)
        param-bindings (zipmap param-names param-idens)
        iden->invoke-idx (zipmap param-idens (range))
        bindings (:bindings ctx)
        ctx (assoc ctx :bindings (merge bindings param-bindings))
        ctx (assoc ctx :iden->invoke-idx iden->invoke-idx)
        ctx (update ctx :parents conj fixed-arity)
        _ (vswap! (:closure-bindings ctx) assoc-in (conj (:parents ctx) :syms) (zipmap param-idens (range)))
        self-ref-idx (when fn-name (update-parents ctx (:closure-bindings ctx) fn-id))
        body (return-do (with-recur-target ctx true) fn-expr body)
        iden->invoke-idx (get-in @(:closure-bindings ctx) (conj (:parents ctx) :syms))]
    (cond-> (->FnBody params body fixed-arity var-arg-name self-ref-idx iden->invoke-idx)
      var-arg-name
      (assoc :vararg-idx (get iden->invoke-idx (last param-idens))))))

(defn analyzed-fn-meta [ctx m]
  (let [;; seq expr has location info with 2 keys
        meta-needs-eval? (> (count m) 2)
        m (if meta-needs-eval? (-> (analyze (assoc ctx :meta true) m)
                                   (vary-meta assoc :sci.impl/op :eval))
              m)]
    m))

(defn analyze-fn* [ctx [_fn name? & body :as fn-expr] macro?]
  (let [ctx (assoc ctx :fn-expr fn-expr)
        fn-name (if (symbol? name?)
                  name?
                  nil)
        body (if fn-name
               body
               (cons name? body))
        ;; fn-name (or fn-name (gensym* "fn"))
        bodies (if (seq? (first body))
                 body
                 [body])
        fn-id (gensym)
        parents ((fnil conj []) (:parents ctx) fn-id)
        ctx (assoc ctx :parents parents)
        ctx (if fn-name (-> ctx
                            (assoc-in [:bindings fn-name] fn-id))
                ctx)
        bindings (:bindings ctx)
        bound-idens (set (vals bindings))
        ;; reverse-bindings (zipmap binding-vals (keys bindings))
        ctx (assoc ctx :outer-idens bound-idens)
        closure-bindings (:closure-bindings ctx)
        analyzed-bodies (reduce
                         (fn [{:keys [:max-fixed :min-varargs] :as acc} body]
                           (let [orig-body body
                                 arglist (first body)
                                 body (expand-fn-args+body ctx body macro? fn-name fn-id)
                                 ;; body (assoc body :sci.impl/arglist arglist)
                                 var-arg-name (:var-arg-name body)
                                 fixed-arity (:fixed-arity body)
                                 new-min-varargs (when var-arg-name fixed-arity)]
                             (when (and var-arg-name min-varargs)
                               (throw-error-with-location "Can't have more than 1 variadic overload" fn-expr))
                             (when (and (not var-arg-name) min-varargs (> fixed-arity min-varargs))
                               (throw-error-with-location
                                "Can't have fixed arity function with more params than variadic function" fn-expr))
                             (-> acc
                                 (assoc :min-varargs new-min-varargs
                                        :max-fixed (max fixed-arity
                                                        max-fixed))
                                 (update :bodies conj (assoc body :orig orig-body))
                                 (update :arglists conj arglist))))
                         {:bodies []
                          :arglists []
                          :min-var-args nil
                          :max-fixed -1} bodies)
        cb-idens-by-arity (get-in @closure-bindings parents)
        ;; all let-bound idens + closed over idens
        cb-idens (apply merge (map :syms (vals cb-idens-by-arity)))
        self-ref? (when fn-name (contains? cb-idens fn-id))
        ;; all closed over idens
        closed-over-idens (filter bound-idens (keys cb-idens))
        iden->invoke-idx (get-in @closure-bindings (conj (pop parents) :syms))
        ;; this represents the indices of enclosed values in old bindings
        ;; we need to copy those to a new array, the enclosed-array
        closed-over-iden->binding-idx (when iden->invoke-idx
                                        (zipmap closed-over-idens
                                                (mapv iden->invoke-idx closed-over-idens)))
        ;; here we decide which iden will be installed in which index in the enclosed array
        closed-over-cnt (count closed-over-idens)
        iden->enclosed-idx (zipmap closed-over-idens (range closed-over-cnt))
        iden->enclosed-idx (if fn-name
                             (assoc iden->enclosed-idx fn-id closed-over-cnt)
                             iden->enclosed-idx)
        enclosed-array-fn
        (if (or self-ref? (seq closed-over-iden->binding-idx))
          (let [enclosed-array-cnt (cond-> closed-over-cnt
                                     fn-name (inc))
                ^objects binding->enclosed
                (into-array (keep (fn [iden]
                                    ;; for fn-id usage there is no outer binding idx
                                    (when-let [binding-idx (get iden->invoke-idx iden)]
                                      (let [enclosed-idx (get iden->enclosed-idx iden)]
                                        ;; (prn :copying binding-idx '-> enclosed-idx)
                                        (doto (object-array 2)
                                          (aset 0 binding-idx)
                                          (aset 1 enclosed-idx)))))
                                  closed-over-idens))]
            (fn [^objects bindings]
              (areduce binding->enclosed idx ret (object-array enclosed-array-cnt)
                       (let [^objects idxs (aget binding->enclosed idx)
                             binding-idx (aget idxs 0)
                             binding-val (aget bindings binding-idx)
                             enclosed-idx (aget idxs 1)]
                         (aset ret enclosed-idx binding-val)
                         ret))))
          (constantly nil))
        bodies (:bodies analyzed-bodies)
        bodies (mapv (fn [body]
                       (let [iden->invocation-idx (:iden->invoke-idx body)
                             invocation-self-idx (:self-ref-idx body)
                             enclosed->invocation
                             (into-array (keep (fn [iden]
                                                 (when-let [invocation-idx (iden->invocation-idx iden)]
                                                   (doto (object-array 2)
                                                     (aset 0 (iden->enclosed-idx iden))
                                                     (aset 1 invocation-idx))))
                                               closed-over-idens))
                             invoc-size (count iden->invocation-idx)
                             copy-enclosed->invocation
                             (when (pos? (alength ^objects enclosed->invocation))
                               (fn [^objects enclosed-array ^objects invoc-array]
                                 (areduce ^objects enclosed->invocation idx ret invoc-array
                                          (let [^objects idxs (aget ^objects enclosed->invocation idx)
                                                enclosed-idx (aget ^objects  idxs 0)
                                                enclosed-val (aget ^objects enclosed-array enclosed-idx)
                                                invoc-idx (aget idxs 1)]
                                            (aset ^objects ret invoc-idx enclosed-val)
                                            ret))))]
                         (assoc body
                                :invoc-size invoc-size
                                :invocation-self-idx invocation-self-idx
                                :copy-enclosed->invocation copy-enclosed->invocation)))
                     bodies)
        arglists (:arglists analyzed-bodies)
        fn-meta (dissoc (meta fn-expr) :line :column)
        ana-fn-meta (when (seq fn-meta) (analyze ctx fn-meta))
        struct #:sci.impl{:fn-bodies bodies
                          :fn-name fn-name
                          :self-ref? self-ref?
                          :arglists arglists
                          :fn true
                          :fn-meta ana-fn-meta
                          :bindings-fn enclosed-array-fn}]
    struct))

(defn fn-ctx-fn [_ctx struct fn-meta]
  (let [fn-name (:sci.impl/fn-name struct)
        fn-bodies (:sci.impl/fn-bodies struct)
        macro? (:sci/macro struct)
        single-arity (when (= 1 (count fn-bodies))
                       (first fn-bodies))
        bindings-fn (:sci.impl/bindings-fn struct)
        self-ref? (:sci.impl/self-ref? struct)]
    (if fn-meta
      (fn [ctx bindings]
        (let [fn-meta (eval/eval ctx bindings fn-meta)
              f (fns/eval-fn ctx bindings fn-name fn-bodies macro? single-arity self-ref? bindings-fn)]
          (vary-meta f merge fn-meta)))
      (fn [ctx bindings]
        (fns/eval-fn ctx bindings fn-name fn-bodies macro? single-arity self-ref? bindings-fn)))))

(defn analyze-fn [ctx fn-expr macro?]
  (let [struct (analyze-fn* ctx fn-expr macro?)
        fn-meta (:sci.impl/fn-meta struct)
        ctxfn (fn-ctx-fn ctx struct fn-meta)]
    (ctx-fn ctxfn
            struct
            fn-expr
            nil)))

(defn update-parents
  ":syms = closed over values"
  [ctx closure-bindings ob]
  (let [parents (:parents ctx)
        new-cb (vswap! closure-bindings
                       (fn [cb]
                         (update-in cb (conj parents :syms)
                                    (fn [iden->invoke-idx]
                                      (if (contains? iden->invoke-idx ob)
                                        iden->invoke-idx
                                        (assoc iden->invoke-idx ob (+ #_arity (count iden->invoke-idx))))))))
        closure-idx (get-in new-cb (conj parents :syms ob))]
    closure-idx))

(defn analyze-let*
  [ctx expr destructured-let-bindings exprs]
  (let [rt (recur-target ctx)
        ctx (without-recur-target ctx)
        [ctx new-let-bindings idens]
        (reduce
         (fn [[ctx new-let-bindings idens] [binding-name binding-value]]
           (let [m (meta binding-value)
                 t (when m (:tag m))
                 binding-name (if t (vary-meta binding-name
                                               assoc :tag t)
                                  binding-name)
                 v (analyze ctx binding-value)
                 new-iden (gensym)
                 cb (:closure-bindings ctx)
                 idx (update-parents ctx cb new-iden)
                 iden->invoke-idx (:iden->invoke-idx ctx)
                 iden->invoke-idx (assoc iden->invoke-idx new-iden idx)
                 ctx (assoc ctx :iden->invoke-idx iden->invoke-idx)]
             [(update ctx :bindings assoc binding-name new-iden)
              (conj new-let-bindings binding-name v)
              (conj idens new-iden)]))
         [ctx [] []]
         (partition 2 destructured-let-bindings))
        body (return-do (with-recur-target ctx rt) expr exprs)
        iden->invoke-idx (:iden->invoke-idx ctx)
        idxs (mapv iden->invoke-idx idens)]
    ;; (prn :params params :idens idens :idxs idxs)
    (ctx-fn
     (fn [ctx bindings]
       (eval/eval-let ctx bindings new-let-bindings body idxs))
     expr)))

(defn analyze-let
  "The let macro from clojure.core"
  [ctx [_let let-bindings & exprs :as expr]]
  (let [let-bindings (destructure let-bindings)]
    (analyze-let* ctx expr let-bindings exprs)))

(declare expand-declare)

(defn analyze-def
  [ctx expr]
  (let [ctx (without-recur-target ctx)
        [_def var-name ?docstring ?init] expr]
    (expand-declare ctx [nil var-name])
    (when-not (simple-symbol? var-name)
      (throw-error-with-location "Var name should be simple symbol." expr))
    (let [arg-count (count expr)
          docstring (when (and (= 4 arg-count)
                               (string? ?docstring))
                      ?docstring)
          expected-arg-count (if docstring 4 3)]
      (when-not (<= arg-count expected-arg-count)
        (throw (new #?(:clj  IllegalArgumentException
                       :cljs js/Error)
                    "Too many arguments to def")))
      (let [init (if docstring ?init ?docstring)
            init (if (= 2 arg-count)
                   :sci.impl/var.unbound
                   (analyze ctx init))
            m (meta var-name)
            m-cnt (count m)
            m-needs-eval? (not= 2 m-cnt)
            m (assoc m :ns @vars/current-ns)
            m (if docstring (assoc m :doc docstring) m)
            m (if m-needs-eval?
                (analyze ctx m)
                (ctx-fn (fn [_ctx _bindings]
                          m)
                        m))]
        (ctx-fn
         (fn [ctx bindings]
           (eval/eval-def ctx bindings var-name init m))
         expr)))))

(defn analyze-defn [ctx [op fn-name & body :as expr]]
  ;; TODO: re-use analyze-def
  (when-not (simple-symbol? fn-name)
    (throw-error-with-location "Var name should be simple symbol." expr))
  (expand-declare ctx [nil fn-name])
  (let [macro? (= "defmacro" (name op))
        [pre-body body] (split-with (comp not sequential?) body)
        _ (when (empty? body)
            (throw-error-with-location "Parameter declaration missing." expr))
        docstring (when-let [ds (first pre-body)]
                    (when (string? ds) ds))
        meta-map (when-let [m (last pre-body)]
                   (when (map? m) m))
        [meta-map2 body] (if (seq? (first body))
                           (let [lb (last body)]
                             (if (map? lb)
                               [lb (butlast body)]
                               [nil body]))
                           [nil body])
        meta-map (merge (meta fn-name) (meta expr) meta-map)
        meta-map (if meta-map2 (merge meta-map meta-map2)
                     meta-map)
        fn-body (with-meta (cons 'fn body)
                  (meta expr))
        f (analyze-fn* ctx fn-body macro?)
        arglists (list 'quote (seq (:sci.impl/arglists f)))
        meta-map (assoc meta-map
                        :ns @vars/current-ns
                        :arglists arglists)
        meta-map (cond-> meta-map
                   docstring (assoc :doc docstring)
                   macro? (assoc :macro true))
        f (assoc f
                 :sci/macro macro?
                 :sci.impl/fn-name fn-name
                 :sci.impl/defn true)
        fn-meta (:sci.impl/fn-meta f)
        ctxfn (fn-ctx-fn ctx f fn-meta)
        f (ctx-fn ctxfn f f)
        meta-map (analyze ctx meta-map)]
    (ctx-fn
     (fn [ctx bindings]
       (eval/eval-def ctx bindings fn-name f meta-map))
     expr)))

(defn analyze-loop
  [ctx expr]
  (let [bv (second expr)
        arg-names (take-nth 2 bv)
        init-vals (take-nth 2 (rest bv))
        [bv syms] (if (every? symbol? arg-names)
                    [bv arg-names]
                    (let [syms (repeatedly (count arg-names) gensym)
                          bv1 (map vector syms init-vals)
                          bv2  (map vector arg-names syms)]
                      [(into [] cat (interleave bv1 bv2)) syms]))
        body (nnext expr)
        expansion (list 'let bv
                        (list* `(fn ~(vec arg-names) ~@body)
                               syms))]
    (analyze ctx expansion)))

(defn analyze-lazy-seq
  [ctx expr]
  (let [body (rest expr)
        ctx (with-recur-target ctx true) ;; body is analyzed in context of implicit no-arg fn
        ana (return-do ctx expr body)]
    (ctx-fn (fn [ctx bindings]
              (lazy-seq (eval/eval ctx bindings ana)))
            expr)))

(defn return-if
  [ctx expr]
  (let [exprs (rest expr)
        children (analyze-children ctx exprs)]
    (case (count children)
      (0 1) (throw-error-with-location "Too few arguments to if" expr)
      2 (let [condition (nth children 0)
              then (nth children 1)]
          (cond (not condition) nil
                (constant? condition) then
                :else (ctx-fn
                       (fn [ctx bindings]
                         (when (eval/eval ctx bindings condition)
                           (eval/eval ctx bindings then)))
                       ;; backward compatibility with stacktrace
                       nil
                       expr
                       nil)))
      3 (let [condition (nth children 0)
              then (nth children 1)
              else (nth children 2)]
          (cond (not condition) else
                (constant? condition) then
                :else (ctx-fn
                       (fn [ctx bindings]
                         (if (eval/eval ctx bindings condition)
                           (eval/eval ctx bindings then)
                           (eval/eval ctx bindings else)))
                       ;; backward compatibility with stacktrace
                       nil
                       expr
                       nil)))
      (throw-error-with-location "Too many arguments to if" expr))))

(defn analyze-case
  [ctx expr]
  (let [ctx-wo-rt (without-recur-target ctx)
        case-val (analyze ctx-wo-rt (second expr))
        clauses (nnext expr)
        match-clauses (take-nth 2 clauses)
        result-clauses (analyze-children ctx (take-nth 2 (rest clauses)))
        [default? case-default] (when (odd? (count clauses))
                                  [true (analyze ctx (last clauses))])
        cases (interleave match-clauses result-clauses)
        assoc-new (fn [m k v]
                    (if-not (contains? m k)
                      (assoc m k v)
                      (throw-error-with-location (str "Duplicate case test constant " k)
                                                 expr)))
        case-map (loop [cases (seq cases)
                        ret-map {}]
                   (if cases
                     (let [[k v & cases] cases]
                       (if (seq? k)
                         (recur
                          cases
                          (reduce (fn [acc k]
                                    (assoc-new acc k v))
                                  ret-map
                                  k))
                         (recur
                          cases
                          (assoc-new ret-map k v))))
                     ret-map))
        f (if default?
            (fn [ctx bindings]
              (eval/eval-case ctx bindings case-map case-val case-default))
            (fn [ctx bindings]
              (eval/eval-case ctx bindings case-map case-val)))]
    (ctx-fn
     f
     nil
     expr
     nil)))

(defn analyze-try
  [ctx expr]
  (let [ctx (without-recur-target ctx)
        body (next expr)
        [body-exprs
         catches
         finally]
        (loop [exprs body
               body-exprs []
               catch-exprs []
               finally-expr nil]
          (if exprs
            (let [expr (first exprs)
                  exprs (next exprs)]
              (cond (and (seq? expr) (= 'catch (first expr)))
                    (recur exprs body-exprs (conj catch-exprs expr) finally-expr)
                    (and (not exprs) (and (seq? expr) (= 'finally (first expr))))
                    [body-exprs catch-exprs expr]
                    :else
                    ;; TODO: cannot add body expression when catch is not empty
                    ;; TODO: can't have finally as non-last expression
                    (recur exprs (conj body-exprs expr) catch-exprs finally-expr)))
            [body-exprs catch-exprs finally-expr]))
        body (analyze ctx (cons 'do body-exprs))
        catches (mapv (fn [c]
                        (let [[_ ex binding & body] c]
                          (if-let [clazz #?(:clj (interop/resolve-class ctx ex)
                                            :cljs (case ex
                                                    js/Error js/Error
                                                    js/Object js/Object
                                                    :default :default
                                                    (analyze ctx ex)))]
                            (let [ex-iden (gensym)
                                  closure-bindings (:closure-bindings ctx)
                                  ex-idx (update-parents ctx closure-bindings ex-iden)
                                  ctx (-> ctx
                                          (assoc-in [:bindings binding] ex-iden)
                                          (assoc-in [:iden->invoke-idx ex-iden] ex-idx))
                                  analyzed-body (analyze ctx
                                                         (cons 'do body))]
                              {:class clazz
                               :ex-idx ex-idx
                               :body analyzed-body})
                            (throw-error-with-location (str "Unable to resolve classname: " ex) ex))))
                      catches)
        finally (when finally
                  (analyze ctx (cons 'do (rest finally))))]
    (ctx-fn (fn [ctx bindings]
              (eval/eval-try ctx bindings body catches finally))
            expr)))

(defn analyze-throw [ctx [_throw ex :as expr]]
  (when-not (= 2 (count expr))
    (throw-error-with-location
     #?(:clj "Too many arguments to throw, throw expects a single Throwable instance"
        :cljs "Too many arguments to throw")
     expr))
  (let [ctx (without-recur-target ctx)
        ana (analyze ctx ex)]
    (ctx-fn (fn [ctx bindings]
              (throw (eval/eval ctx bindings ana)))
            ;; legacy structure for error reporting
            expr
            nil
            (assoc (meta expr)
                   :ns @vars/current-ns
                   :file @vars/current-file
                   :special true))))

(defn expand-declare [ctx [_declare & names :as expr]]
  (let [cnn (vars/current-ns-name)
        env (:env ctx)
        the-current-ns (get-in @env [:namespaces cnn])
        refers (:refers the-current-ns)
        the-current-ns (reduce (fn [acc name]
                                 (if-let [x (and refers (.get ^java.util.Map refers name))]
                                   (throw-error-with-location
                                    (str name " already refers to "
                                         x " in namespace "
                                         cnn)
                                    expr)
                                   (if-not #?(:clj (.containsKey ^java.util.Map the-current-ns name)
                                              :cljs (get the-current-ns name))
                                     (assoc acc name
                                            (doto (vars/->SciVar nil (symbol (str cnn)
                                                                             (str name))
                                                                 (assoc (meta name)
                                                                        :name name
                                                                        :ns @vars/current-ns
                                                                        :file @vars/current-file)
                                                                 false)
                                              (vars/unbind)))
                                     the-current-ns)))
                               the-current-ns
                               names)]
    (swap! env
           (fn [env]
             (update env :namespaces assoc cnn the-current-ns))))
  nil)

;;;; Interop

(defn analyze-dot [ctx [_dot instance-expr method-expr & args :as expr]]
  (let [ctx (without-recur-target ctx)
        [method-expr & args] (if (seq? method-expr) method-expr
                                 (cons method-expr args))
        instance-expr (analyze ctx instance-expr)
        #?@(:clj [instance-expr (utils/vary-meta*
                                 instance-expr
                                 (fn [m]
                                   (if-let [t (:tag m)]
                                     (let [clazz (or (interop/resolve-class ctx t)
                                                     (records/resolve-record-class ctx t)
                                                     (throw-error-with-location
                                                      (str "Unable to resolve classname: " t) t))]
                                       (assoc m :tag-class clazz))
                                     m)))])
        method-name (name method-expr)
        args (when args (analyze-children ctx args))
        res
        (let [field-access (str/starts-with? method-name "-")
              meth-name (if field-access
                          (subs method-name 1)
                          method-name)]
          #?(:clj (if (class? instance-expr)
                    (if (nil? args)
                      (if field-access
                        (ctx-fn
                         (fn [_ctx _bindings]
                           (interop/get-static-field [instance-expr (subs method-name 1)]))
                         nil expr nil)
                        ;; https://clojure.org/reference/java_interop
                        ;; If the second operand is a symbol and no args are
                        ;; supplied it is taken to be a field access - the
                        ;; name of the field is the name of the symbol, and
                        ;; the value of the expression is the value of the
                        ;; field, unless there is a no argument public method
                        ;; of the same name, in which case it resolves to a
                        ;; call to the method.
                        (if-let [_
                                 (try (Reflector/getStaticField ^Class instance-expr ^String method-name)
                                      (catch IllegalArgumentException _ nil))]
                          (ctx-fn
                           (fn [_ctx _bindings]
                             (interop/get-static-field [instance-expr method-name]))
                           nil expr nil)
                          (ctx-fn
                           (fn [ctx bindings]
                             (eval/eval-static-method-invocation ctx bindings (cons [instance-expr method-name] args)))
                           nil
                           expr
                           (assoc (meta expr)
                                  :ns @vars/current-ns
                                  :file @vars/current-file))))
                      (ctx-fn
                       (fn [ctx bindings]
                         (eval/eval-static-method-invocation ctx bindings (cons [instance-expr method-name] args)))
                       nil expr
                       (assoc (meta expr)
                              :ns @vars/current-ns
                              :file @vars/current-file)))
                    (ctx-fn (fn [ctx bindings]
                              (eval/eval-instance-method-invocation ctx bindings instance-expr meth-name field-access args))
                            ;; this info is used by set!
                            {::instance-expr instance-expr
                             ::method-name method-name}
                            expr
                            (assoc (meta expr)
                                   :ns @vars/current-ns
                                   :file @vars/current-file)))
             :cljs (ctx-fn
                    (let [allowed? (identical? method-expr utils/allowed-append)]
                      (fn [ctx bindings]
                        (eval/eval-instance-method-invocation ctx bindings instance-expr meth-name field-access args allowed?)))
                    ;; this info is used by set!
                    {::instance-expr instance-expr
                     ::method-name method-name}
                    expr
                    (assoc (meta expr)
                           :ns @vars/current-ns
                           :file @vars/current-file))))]
    res))

(defn expand-dot**
  "Expands (. x method)"
  [ctx expr]
  (when (< (count expr) 3)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (analyze-dot ctx expr))

(defn expand-dot*
  "Expands (.foo x)"
  [ctx [method-name obj & args :as expr]]
  (when (< (count expr) 2)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (analyze-dot ctx (list '. obj (cons (symbol (subs (name method-name) 1)) args))))

(defn analyze-new [ctx [_new class-sym & args :as expr]]
  (let [ctx (without-recur-target ctx)]
    (if-let [class #?(:clj (:class (interop/resolve-class-opts ctx class-sym))
                      :cljs (or (when-let [clazz (when-let [opts (interop/resolve-class-opts ctx class-sym)]
                                                   (or
                                                    ;; TODO: deprecate
                                                    (:constructor opts)
                                                    (:class opts)))]
                                  clazz)
                                (resolve/resolve-symbol ctx class-sym false)))]
      (let [#?@(:cljs [var? (vars/var? class)
                       maybe-var (when var? class)
                       maybe-record (cond
                                      var?
                                      (deref maybe-var)
                                      ;; symbol = already deref-ed record coming in via :import
                                      (symbol? class)
                                      class)
                       maybe-record-constructor
                       (when maybe-record
                         (-> maybe-record
                             meta :sci.impl.record/constructor))])
            args (analyze-children ctx args)] ;; analyze args!
        #?(:clj
           (ctx-fn
            (fn [ctx bindings]
              (interop/invoke-constructor class (mapv #(eval/eval ctx bindings %) args)))
            expr)
           :cljs
           (cond maybe-record-constructor
                 (return-call ctx
                              ;; for backwards compatibility with error reporting
                              expr ;; (list* (:sci.impl.record/constructor (meta record)) args)
                              maybe-record-constructor
                              args
                              (assoc (meta expr)
                                     :ns @vars/current-ns
                                     :file @vars/current-file)
                              nil)
                 var?
                 (ctx-fn
                  (fn [ctx bindings]
                    (interop/invoke-constructor (deref maybe-var)
                                                (mapv #(eval/eval ctx bindings %) args)))
                  expr)
                 (instance? sci.impl.types/EvalFn class)
                 (ctx-fn
                  (fn [ctx bindings]
                    (interop/invoke-constructor (eval/eval ctx bindings class)
                                                (mapv #(eval/eval ctx bindings %) args)))
                  expr)
                 :else
                 (ctx-fn
                  (fn [ctx bindings]
                    (interop/invoke-constructor class ;; no eval needed
                                                (mapv #(eval/eval ctx bindings %) args)))
                  expr))))
      (if-let [record (records/resolve-record-class ctx class-sym)]
        (let [args (analyze-children ctx args)]
          ;; _ctx expr f analyzed-children stack
          (return-call ctx
                       ;; for backwards compatibility with error reporting
                       expr ;; (list* (:sci.impl.record/constructor (meta record)) args)
                       (:sci.impl.record/constructor (meta record))
                       args
                       (assoc (meta expr)
                              :ns @vars/current-ns
                              :file @vars/current-file
                              )
                       nil))
        (throw-error-with-location (str "Unable to resolve classname: " class-sym) class-sym)))))

(defn expand-constructor [ctx [constructor-sym & args]]
  (let [constructor-name (name constructor-sym)
        class-sym (with-meta (symbol (subs constructor-name 0
                                           (dec (count constructor-name))))
                    (meta constructor-sym))]
    (analyze-new ctx (with-meta (list* 'new class-sym args)
                       (meta constructor-sym)))))

;;;; End interop

;;;; Namespaces

(defn return-ns-op [_ctx f expr analyzed-args]
  (ctx-fn (fn [ctx _bindings]
            (apply f ctx analyzed-args))
          expr
          nil
          (assoc (meta expr)
                 :file @vars/current-file
                 :ns @vars/current-ns)))

(defn analyze-ns-form [ctx [_ns ns-name & exprs :as expr]]
  (when-not (symbol? ns-name)
    (throw (new #?(:clj IllegalArgumentException
                   :cljs js/Error)
                (str "Namespace name must be symbol, got: " (pr-str ns-name)))))
  (let [[docstring exprs]
        (let [fexpr (first exprs)]
          (if (string? fexpr)
            [fexpr (next exprs)]
            [nil exprs]))
        ;; skip attr-map
        [attr-map exprs]
        (let [m (first exprs)]
          (if (map? m)
            [m (next exprs)]
            [nil exprs]))
        attr-map (if docstring
                   (assoc attr-map :doc docstring)
                   attr-map)]
    (set-namespace! ctx ns-name attr-map)
    (loop [exprs exprs
           ret []]
      (if exprs
        (let [[k & args :as expr] (first exprs)]
          (case k
            (:require :use :import :refer-clojure)
            (recur (next exprs)
                   (conj ret
                         (return-ns-op
                          ctx (case k
                                :require load/eval-require
                                :use load/eval-use
                                :import eval/eval-import
                                :refer-clojure (fn [ctx & args]
                                                 (apply load/eval-refer ctx 'clojure.core args)))
                          expr args)))
            :gen-class ;; ignore
            (recur (next exprs) ret)))
        (return-do
         ctx
         expr
         (conj ret
               (ctx-fn
                (fn [ctx _bindings]
                  (load/add-loaded-lib (:env ctx) ns-name)
                  nil)
                nil)))))))

;;;; End namespaces


;;;; Vars

(defn analyze-var [ctx [_ var-name]]
  (resolve/resolve-symbol ctx var-name))

(defn analyze-set! [ctx [_ obj v :as expr]]
  (cond (symbol? obj) ;; assume dynamic var
        (let [obj (resolve/resolve-symbol ctx obj)
              _ (when-not (vars/var? obj)
                  (throw-error-with-location "Invalid assignment target" expr))
              v (analyze ctx v)]
          (ctx-fn (fn [ctx bindings]
                    (let [v (eval/eval ctx bindings v)]
                      (types/setVal obj v)))
                  expr))
        #?@(:cljs [(seq? obj)
                   (let [obj (analyze ctx obj)
                         v (analyze ctx v)
                         obj (types/info obj)
                         k (subs (::method-name obj) 1)
                         obj (::instance-expr obj)]
                     (ctx-fn (fn [ctx bindings]
                               (let [obj (eval/eval ctx bindings obj)
                                     v (eval/eval ctx bindings v)]
                                 (gobj/set obj k v)))
                             expr))])
        :else (throw-error-with-location "Invalid assignment target" expr)))

;;;; End vars

(defmacro gen-return-binding-call
  "Creates returning-binding-call function, optimizes calling a local
  binding as function."
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 20))]
    `(defn ~'return-binding-call
       ~'[_ctx expr idx f analyzed-children stack]
       (ctx-fn
        (case (count ~'analyzed-children)
          ~@(concat
             (mapcat (fn [[i binds]]
                       [i `(let ~binds
                             (fn [~'ctx ~'bindings]
                               ((aget ~(with-meta 'bindings
                                         {:tag 'objects}) ~'idx)
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                       (range i)))))])
                     let-bindings)
             `[(fn [~'ctx ~'bindings]
                 (eval/fn-call ~'ctx ~'bindings (aget ~(with-meta 'bindings
                                                         {:tag 'objects}) ~'idx) ~'analyzed-children))]))
        nil
        ~'expr
        ~'stack))))

(declare return-binding-call) ;; for clj-kondo
(gen-return-binding-call)

(defmacro gen-return-needs-ctx-call
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 20))]
    `(defn ~'return-needs-ctx-call
       ~'[_ctx expr f analyzed-children]
       (ctx-fn
        (case (count ~'analyzed-children)
          ~@(concat
             (mapcat (fn [[i binds]]
                       [i `(let ~binds
                             (fn [~'ctx ~'bindings]
                               (~'f ~'ctx
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                       (range i)))))])
                     let-bindings)
             `[(fn [~'ctx ~'bindings]
                 (eval/fn-call ~'ctx ~'bindings ~'f (cons ~'ctx ~'analyzed-children)))]))
        ~'expr))))

(declare return-needs-ctx-call) ;; for clj-kondo
(gen-return-needs-ctx-call)

;; NOTE: there is a small perf win (about 3%) when checking if all
;; analyzed-children are EvalFn and then using those fns directly. See
;; inline-evals branch.

(defmacro gen-return-call
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 20))]
    `(defn ~'return-call
       ~'[_ctx expr f analyzed-children stack wrap]
       (ctx-fn
        (case (count ~'analyzed-children)
          ~@(concat
             (mapcat (fn [[i binds]]
                       [i `(let ~binds
                             (if ~'wrap
                               (fn [~'ctx ~'bindings]
                                 ((~'wrap ~'bindings ~'f)
                                  ~@(map (fn [j]
                                           `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                         (range i))))
                               (fn [~'ctx ~'bindings]
                                 (~'f
                                  ~@(map (fn [j]
                                           `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" j))))
                                         (range i))))))])
                     let-bindings)
             `[(if ~'wrap
                 (fn [~'ctx ~'bindings]
                   (eval/fn-call ~'ctx ~'bindings (~'wrap ~'bindings ~'f) ~'analyzed-children))
                 (fn [~'ctx ~'bindings]
                   (eval/fn-call ~'ctx ~'bindings ~'f ~'analyzed-children)))]))
        nil
        ~'expr
        ~'stack))))

(declare return-call) ;; for clj-kondo
(gen-return-call)

(defn analyze-quote [_ctx expr]
  (when-not (= 2 (count expr))
    (throw-error-with-location "Wrong number of args (0) passed to quote" expr))
  (let [snd (second expr)]
    (ctx-fn (fn [_ctx _bindings] snd) expr)))

(defn analyze-in-ns [ctx expr]
  (let [ns-expr (analyze ctx (second expr))]
    (ctx-fn (fn [ctx bindings]
              (let [ns-sym (eval/eval ctx bindings ns-expr)]
                (set-namespace! ctx ns-sym nil)
                nil))
            expr)))

;; f m expr stack
(defn analyze-import [_ctx expr]
  (let [args (rest expr)]
    (ctx-fn (fn [ctx _bindings]
              (apply eval/eval-import ctx args))
            nil
            expr
            (assoc (meta expr)
                   :ns @vars/current-ns
                   :file @vars/current-file))))

(defn analyze-call [ctx expr m top-level?]
  (let [eval-file (:clojure.core/eval-file m)]
    (when eval-file
      (vars/push-thread-bindings {vars/current-file eval-file}))
    (try
      (let [f (first expr)]
        (cond (symbol? f)
              (let [fsym f
                    ;; in call position Clojure prioritizes special symbols over
                    ;; bindings
                    special-sym (get special-syms f)
                    _ (when (and special-sym
                                 (:check-permissions ctx))
                        (resolve/check-permission! ctx f [special-sym nil]))
                    f (or special-sym
                          (resolve/resolve-symbol ctx f true))
                    f-meta (meta f)
                    eval? (and f-meta (:sci.impl/op f-meta))]
                (cond (and f-meta (::static-access f-meta))
                      #?(:clj (expand-dot** ctx (with-meta (list* '. (first f) (second f) (rest expr))
                                                  m))
                         :cljs
                         (let [[class method-name] f
                               method-name (str method-name)
                               len (.-length method-name)
                               idx (str/last-index-of method-name ".")
                               f (if ;; this is not js/Error.
                                     (and idx (not= (dec len) idx))
                                   ;; this is to support calls like js/Promise.all
                                   ;; and js/process.argv.slice
                                   [(gobj/getValueByKeys class (into-array (.split (subs method-name 0 idx) ".")))
                                    (subs method-name (inc idx))]
                                   f)
                               children (analyze-children ctx (rest expr))]
                           (ctx-fn (fn [ctx bindings]
                                     (eval/eval-static-method-invocation ctx bindings (cons f children)))
                                   expr)))
                      (and (not eval?) ;; the symbol is not a binding
                           (symbol? f)
                           (or
                            special-sym
                            (contains? ana-macros f)))
                      (case f
                        ;; we treat every subexpression of a top-level do as a separate
                        ;; analysis/interpretation unit so we hand this over to the
                        ;; interpreter again, which will invoke analysis + evaluation on
                        ;; every sub expression
                        do (return-do ctx expr (rest expr))
                        let (analyze-let ctx expr)
                        (fn fn*) (analyze-fn ctx expr false)
                        def (analyze-def ctx expr)
                        ;; NOTE: defn / defmacro aren't implemented as normal macros yet
                        (defn defmacro) (let [ret (analyze-defn ctx expr)]
                                          ret)
                        ;; TODO: implement as normal macro in namespaces.cljc
                        loop (analyze-loop ctx expr)
                        lazy-seq (analyze-lazy-seq ctx expr)
                        if (return-if ctx expr)
                        case (analyze-case ctx expr)
                        try (analyze-try ctx expr)
                        throw (analyze-throw ctx expr)
                        declare (expand-declare ctx expr)
                        expand-dot* (expand-dot* ctx expr)
                        . (expand-dot** ctx expr)
                        expand-constructor (expand-constructor ctx expr)
                        new (analyze-new ctx expr)
                        ns (analyze-ns-form ctx expr)
                        var (analyze-var ctx expr)
                        set! (analyze-set! ctx expr)
                        quote (analyze-quote ctx expr)
                        import (analyze-import ctx expr)
                        or (return-or ctx expr (rest expr))
                        and (return-and ctx expr (rest expr))
                        recur (return-recur ctx expr (analyze-children (without-recur-target ctx) (rest expr)))
                        in-ns (analyze-in-ns ctx expr))
                      :else
                      (try
                        (if (macro? f)
                          (let [needs-ctx? (identical? utils/needs-ctx
                                                       (:sci.impl/op (meta f)))
                                ;; Fix for #603
                                #?@(:cljs [f (if (vars/var? f) @f f)])
                                v (if needs-ctx?
                                    (apply f expr
                                           (:bindings ctx)
                                           ctx
                                           (rest expr))
                                    (apply f expr
                                           (:bindings ctx) (rest expr)))
                                expanded (cond (:sci.impl/macroexpanding ctx) v
                                               (and top-level? (seq? v) (= 'do (first v)))
                                               ;; hand back control to eval-form for
                                               ;; interleaved analysis and eval
                                               (types/->EvalForm v)
                                               :else (let [v (if m (if #?(:clj (instance? clojure.lang.IObj v)
                                                                          :cljs (implements? IWithMeta v))
                                                                     (with-meta v (merge m (meta v)))
                                                                     v)
                                                                 v)]
                                                       (analyze ctx v top-level?)))]
                            expanded)
                          (if-let [f (:sci.impl/inlined f-meta)]
                            (return-call ctx
                                         expr
                                         f (analyze-children ctx (rest expr))
                                         (assoc m
                                                :ns @vars/current-ns
                                                :file @vars/current-file
                                                :sci.impl/f-meta f-meta)
                                         nil)
                            (if-let [op (:sci.impl/op (meta f))]
                              (case op
                                needs-ctx
                                (if (identical? utils/needs-ctx op)
                                  (return-needs-ctx-call ctx
                                                         expr
                                                         f (analyze-children ctx (rest expr)))
                                  (let [children (analyze-children ctx (rest expr))]
                                    (return-call ctx
                                                 expr
                                                 f children
                                                 (assoc m
                                                        :ns @vars/current-ns
                                                        :file @vars/current-file
                                                        :sci.impl/f-meta f-meta)
                                                 nil)))
                                :resolve-sym
                                (return-binding-call ctx
                                                     expr
                                                     (:sci.impl/idx (meta f))
                                                     f (analyze-children ctx (rest expr))
                                                     (assoc m
                                                            :ns @vars/current-ns
                                                            :file @vars/current-file
                                                            :sci.impl/f-meta f-meta))
                                (let [children (analyze-children ctx (rest expr))]
                                  (return-call ctx
                                               expr
                                               f children (assoc m
                                                                 :ns @vars/current-ns
                                                                 :file @vars/current-file
                                                                 :sci.impl/f-meta f-meta)
                                               nil)))
                              (let [self-ref? (:self-ref? ctx)]
                                (if (and self-ref? (self-ref? f))
                                  (let [children (analyze-children ctx (rest expr))]
                                    (return-call ctx
                                                 expr
                                                 f children (assoc m
                                                                   :ns @vars/current-ns
                                                                   :file @vars/current-file
                                                                   :sci.impl/f-meta f-meta)
                                                 (fn [bindings _]
                                                   (deref
                                                    (eval/resolve-symbol bindings fsym)))))
                                  (let [children (analyze-children ctx (rest expr))]
                                    (return-call ctx
                                                 expr
                                                 f children (assoc m
                                                                   :ns @vars/current-ns
                                                                   :file @vars/current-file
                                                                   :sci.impl/f-meta f-meta)
                                                 #?(:cljs (when (vars/var? f) (fn [_ v]
                                                                                (deref v))) :clj nil))))))))
                        (catch #?(:clj Exception :cljs js/Error) e
                          ;; we pass a ctx-fn because the rethrow function calls
                          ;; stack on it, the only interesting bit it the map
                          ;; with :ns and :file
                          (rethrow-with-location-of-node ctx e
                                                         (ctx-fn
                                                          nil
                                                          nil
                                                          expr
                                                          (assoc m
                                                                 :ns @vars/current-ns
                                                                 :file @vars/current-file
                                                                 :sci.impl/f-meta f-meta)))))))
              (keyword? f)
              (let [children (analyze-children ctx (rest expr))
                    ccount (count children)]
                (case ccount
                  1 (let [arg (nth children 0)]
                      (ctx-fn
                       (fn [ctx bindings]
                         (f (eval/eval ctx bindings arg)))
                       expr))
                  2 (let [arg0 (nth children 0)
                          arg1 (nth children 1)]
                      (ctx-fn (fn [ctx bindings]
                                (f (eval/eval ctx bindings arg0)
                                   (eval/eval ctx bindings arg1)))
                              expr))
                  (throw-error-with-location (str "Wrong number of args (" ccount ") passed to: " f) expr)))
              :else
              (let [f (analyze ctx f)
                    children (analyze-children ctx (rest expr))]
                (ctx-fn (fn [ctx bindings]
                          (let [f (eval/eval ctx bindings f)]
                            (if (ifn? f)
                              (eval/fn-call ctx bindings f children)
                              (throw (new #?(:clj Exception :cljs js/Error)
                                          (str "Cannot call " (pr-str f) " as a function."))))))
                        nil
                        expr
                        (assoc m
                               :ns @vars/current-ns
                               :file @vars/current-file)))))
      (finally
        (when eval-file
          (vars/pop-thread-bindings))))))

(def ^:const constant-colls true) ;; see GH #452

(defn return-map [ctx the-map]
  (let [children (into [] cat the-map)
        analyzed-children (analyze-children ctx children)]
    (if (<= (count analyzed-children) 16)
      (return-call ctx the-map array-map analyzed-children nil nil)
      (return-call ctx the-map hash-map analyzed-children nil nil))))

(defn analyze-map
  [ctx expr m]
  (let [ctx (without-recur-target ctx)
        ks (keys expr)
        vs (vals expr)
        constant-map? (and constant-colls
                           (every? constant? ks)
                           (every? constant? vs))
        analyzed-map (cond constant-map?
                           expr
                           :else
                           (return-map ctx expr))
        analyzed-meta (when m (analyze ctx m))
        ret (if analyzed-meta
              (ctx-fn
               (fn [ctx bindings]
                 (let [coll (eval/eval ctx bindings analyzed-map)
                       md (eval/eval ctx bindings analyzed-meta)]
                   (with-meta coll md)))
               expr)
              analyzed-map)]
    ret))

(defn analyze-vec-or-set
  "Returns analyzed vector or set"
  [ctx _f1 f2 expr m]
  (let [ctx (without-recur-target ctx)
        constant-coll?
        (and constant-colls
             (every? constant? expr))
        analyzed-meta (when m (analyze ctx #_(assoc ctx :meta true) m))
        must-eval (or (not constant-coll?)
                      (not (identical? m analyzed-meta)))
        analyzed-coll (if (not must-eval)
                        expr
                        (if m
                          ;; can we transform this into return-call?
                          (let [ef (return-call ctx expr f2 (analyze-children ctx expr) nil nil)]
                            (ctx-fn
                             (fn [ctx bindings]
                               (let [coll (eval/eval ctx bindings ef)
                                     md (eval/eval ctx bindings analyzed-meta)]
                                 (with-meta coll md)))
                             expr))
                          (return-call ctx expr f2 (analyze-children ctx expr) nil nil)))]
    analyzed-coll))

#?(:cljs
   (defn analyze-js-obj [ctx js-val]
     (let [v (.-val js-val)]
       (if (map? v)
         (let [ks (keys v)
               ks (map name ks)
               vs (vals v)
               vs (analyze-children ctx vs)]
           (ctx-fn (fn [ctx bindings]
                     (apply js-obj (interleave ks (map #(eval/eval ctx bindings %) vs))))
                   js-val))
         (let [vs (analyze-children ctx v)]
           (ctx-fn (fn [ctx bindings]
                     (let [arr (array)]
                       (doseq [x vs]
                         (.push arr (eval/eval ctx bindings x)))
                       arr))
                   js-val))))))

(defn analyze
  ([ctx expr]
   (analyze ctx expr false))
  ([ctx expr top-level?]
   ;; (prn :ana expr (:meta ctx))
   (let [m (meta expr)]
     (cond
       (constant? expr) expr ;; constants do not carry metadata
       (symbol? expr) (let [v (resolve/resolve-symbol ctx expr false (:tag m))
                            mv (meta v)]
                        (cond (constant? v) v
                              (identical? utils/needs-ctx (:sci.impl/op mv))
                              (partial v ctx)
                              (vars/var? v)
                              (if (:const mv)
                                @v
                                (if (vars/isMacro v)
                                  (throw (new #?(:clj IllegalStateException :cljs js/Error)
                                              (str "Can't take value of a macro: " v "")))
                                  (ctx-fn (fn [_ctx _bindings]
                                            (faster/deref-1 v))
                                          v)))
                              :else v))
       ;; don't evaluate records, this check needs to go before map?
       ;; since a record is also a map
       (record? expr) expr
       (map? expr) (analyze-map ctx expr m)
       #?@(:cljs [(instance?  JSValue expr) (analyze-js-obj ctx expr)])
       (vector? expr) (analyze-vec-or-set ctx
                                          ;; relying on analyze-children to
                                          ;; return a vector
                                          identity
                                          vector expr m)
       (set? expr) (analyze-vec-or-set ctx set hash-set expr m)
       (seq? expr) (if (seq expr)
                     (analyze-call ctx expr m top-level?)
                     ;; the empty list
                     expr)
       :else expr))))

;;;; Scratch

(comment
  ;; _ctx expr f analyzed-children
  )
