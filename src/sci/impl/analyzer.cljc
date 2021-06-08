(ns sci.impl.analyzer
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   #?(:clj [clojure.string :as str])
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.doseq-macro :refer [expand-doseq]]
   [sci.impl.evaluator :as eval]
   [sci.impl.fns :as fns]
   [sci.impl.for-macro :refer [expand-for]]
   [sci.impl.interop :as interop]
   [sci.impl.load :as load]
   [sci.impl.records :as records]
   [sci.impl.resolve :as resolve]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer
    [mark-eval mark-eval-call constant?
     rethrow-with-location-of-node
     merge-meta set-namespace!
     macro? ana-macros kw-identical? ctx-fn
     maybe-destructured]]
   [sci.impl.vars :as vars]
   #?(:cljs [goog.object :as gobject]))
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

#?(:clj (set! *warn-on-reflection* true))

;; derived from (keys (. clojure.lang.Compiler specials))
;; (& monitor-exit case* try reify* finally loop* do letfn* if clojure.core/import* new deftype* let* fn* recur set! . var quote catch throw monitor-enter def)
(def special-syms '#{try finally do if new recur quote catch throw def . var set!})

(defn- throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(declare analyze analyze-call return-call return-map)

;;;; Macros

(defn macroexpand-1 [ctx expr]
  (let [original-expr expr]
    (if (seq? expr)
      (let [op (first expr)]
        (if (symbol? op)
          (cond (get special-syms op) expr
                (contains? #{'for} op) (analyze (assoc ctx :sci.impl/macroexpanding true)
                                                expr)
                :else
                (let [f (try (resolve/resolve-symbol ctx op true)
                             (catch #?(:clj Exception :cljs :default)
                                 _ ::unresolved))]
                  (if (kw-identical? ::unresolved f)
                    expr
                    (let [f (if (and (vars/var? f)
                                     (vars/isMacro f))
                              @f f)]
                      (if (macro? f)
                        (let [f (if (identical? utils/needs-ctx (some-> f meta :sci.impl/op))
                                  (partial f ctx)
                                  f)]
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

(defmacro gen-return-do
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 2 4))]
    `(defn ~'return-do
       ~'[expr analyzed-children]
       (case (count ~'analyzed-children)
         ~@(concat
            [0 nil]
            [1 `(nth ~'analyzed-children 0)]
            (mapcat (fn [[i binds]]
                      [i `(let ~binds
                            (ctx-fn
                             (fn [~'ctx]
                               ~@(map (fn [j]
                                        `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                      (range i)))
                             ~'expr))])
                    let-bindings)
            `[(ctx-fn
               (fn [~'ctx]
                 (eval/eval-do ~'ctx ~'analyzed-children))
               ~'expr)])))))

;; (require '[clojure.pprint :refer [pprint]])
;; (pprint (clojure.core/macroexpand '(gen-return-do)))

(declare return-do) ;; for clj-kondo
(gen-return-do)

(defmacro gen-return-or
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 2 20))]
    `(defn ~'return-or
       ~'[expr analyzed-children]
       (case (count ~'analyzed-children)
         ~@(concat
            [0 nil]
            [1 `(nth ~'analyzed-children 0)]
            (mapcat (fn [[i binds]]
                      [i `(let ~binds
                            (ctx-fn
                             (fn [~'ctx]
                               (or
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                       (range i))))
                             ~'expr))])
                    let-bindings)
            `[(ctx-fn
               (fn [~'ctx]
                 (eval/eval-or ~'ctx ~'analyzed-children))
               ~'expr)])))))

(declare return-or) ;; for clj-kondo
(gen-return-or)

(defmacro gen-return-and
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 2 20))]
    `(defn ~'return-and
       ~'[expr analyzed-children]
       (case (count ~'analyzed-children)
         ~@(concat
            [0 nil]
            [1 `(nth ~'analyzed-children 0)]
            (mapcat (fn [[i binds]]
                      [i `(let ~binds
                            (ctx-fn
                             (fn [~'ctx]
                               (and
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                       (range i))))
                             ~'expr))])
                    let-bindings)
            `[(ctx-fn
               (fn [~'ctx]
                 (eval/eval-and ~'ctx ~'analyzed-children))
               ~'expr)])))))

(declare return-and) ;; for clj-kondo
(gen-return-and)

(def ^:const recur-0 (fns/->Recur []))

(defmacro gen-return-recur
  []
  (let [let-bindings (map (fn [i]
                            [i (vec (mapcat (fn [j]
                                              [(symbol (str "arg" j))
                                               `(nth ~'analyzed-children ~j)])
                                            (range i)))])
                          (range 1 20))]
    `(defn ~'return-recur
       ~'[expr analyzed-children]
       (case (count ~'analyzed-children)
         ~@(concat
            [0 `(ctx-fn
                 (fn [~'_]
                   recur-0)
                 ~'expr)]
            (mapcat (fn [[i binds]]
                      [i `(let ~binds
                            (ctx-fn
                             (fn [~'ctx]
                               (and
                                (fns/->Recur
                                 [~@(map (fn [j]
                                           `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                         (range i))])))
                             ~'expr))])
                    let-bindings)
            `[(ctx-fn
               (fn [~'ctx]
                 (eval/fn-call ~'ctx (comp fns/->Recur vector) ~'analyzed-children))
               ~'expr)])))))

;; (require 'clojure.pprint)
;; (clojure.pprint/pprint
;;  (clojure.core/macroexpand '(gen-return-recur)))

(declare return-recur) ;; for clj-kondo
(gen-return-recur)

(defn analyze-children [ctx children]
  (mapv #(analyze ctx %) children))

(defrecord FnBody [params body fixed-arity var-arg-name])

(defn expand-fn-args+body [{:keys [:fn-expr] :as ctx} [binding-vector & body-exprs] macro?]
  (when-not binding-vector
    (throw-error-with-location "Parameter declaration missing." fn-expr))
  (when-not (vector? binding-vector)
    (throw-error-with-location "Parameter declaration should be a vector" fn-expr))
  (let [binding-vector (if macro? (into ['&form '&env] binding-vector)
                           binding-vector)
        fixed-args (take-while #(not= '& %) binding-vector)
        fixed-arity (count fixed-args)
        var-arg-name (second (drop-while #(not= '& %) binding-vector))
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
        ctx (update ctx :bindings merge (zipmap params
                                                (repeat nil)))
        body (return-do fn-expr (analyze-children ctx body))]
    (->FnBody params body fixed-arity var-arg-name)))

(defn analyzed-fn-meta [ctx m]
  (let [;; seq expr has location info with 2 keys
        meta-needs-eval? (> (count m) 2)
        ;; TODO: users might have parsed using :end-line still
        m (if meta-needs-eval? (mark-eval
                                (analyze (assoc ctx :meta true) m))
              m)]
    m))


(defn expand-fn* [ctx [_fn name? & body :as fn-expr] macro?]
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
        ctx (if fn-name (assoc-in ctx [:bindings fn-name] nil)
                ctx)
        analyzed-bodies (reduce
                         (fn [{:keys [:max-fixed :min-varargs] :as acc} body]
                           (let [arglist (first body)
                                 body (expand-fn-args+body ctx body macro?)
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
                                 (update :bodies conj body)
                                 (update :arglists conj arglist))))
                         {:bodies []
                          :arglists []
                          :min-var-args nil
                          :max-fixed -1} bodies)
        arities (:bodies analyzed-bodies)
        arglists (:arglists analyzed-bodies)
        fn-meta (meta fn-expr)
        ana-fn-meta (analyzed-fn-meta ctx fn-meta)
        fn-meta (when-not (identical? fn-meta ana-fn-meta)
                  ;; fn-meta contains more than only location info
                  (-> ana-fn-meta (dissoc :line :end-line :column :end-column)))
        struct #:sci.impl{:fn-bodies arities
                          :fn-name fn-name
                          :arglists arglists
                          :fn true
                          :fn-meta fn-meta}
        struct (with-meta struct
                 {:sci.impl/op :fn})]
    struct))

(defn fn-ctx-fn [_ctx struct fn-meta]
  (let [fn-name (:sci.impl/fn-name struct)
        fn-bodies (:sci.impl/fn-bodies struct)
        defn? (:sci.impl/defn struct)
        macro? (:sci/macro struct)
        self-ref? (and fn-name (not defn?))
        single-arity (when (= 1 (count fn-bodies))
                       (first fn-bodies))]
    (if fn-meta
      (fn [ctx]
        (let [fn-meta (eval/handle-meta ctx fn-meta)
              f (fns/eval-fn ctx fn-name fn-bodies macro? single-arity self-ref?)]
          (vary-meta f merge fn-meta)))
      (fn [ctx]
        (fns/eval-fn ctx fn-name fn-bodies macro? single-arity self-ref?)))))

(defn expand-fn [ctx fn-expr macro?]
  (let [struct (expand-fn* ctx fn-expr macro?)
        fn-meta (:sci.impl/fn-meta struct)
        ctxfn (fn-ctx-fn ctx struct fn-meta)]
    (ctx-fn ctxfn
            struct
            struct)))

(defn expand-let*
  [ctx destructured-let-bindings exprs]
  (let [[ctx new-let-bindings]
        (reduce
         (fn [[ctx new-let-bindings] [binding-name binding-value]]
           (let [m (meta binding-value)
                 t (when m (:tag m))
                 binding-name (if t (vary-meta binding-name
                                               assoc :tag t)
                                  binding-name)
                 v (analyze ctx binding-value)]
             [(update ctx :bindings assoc binding-name v)
              (conj new-let-bindings binding-name v)]))
         [ctx []]
         (partition 2 destructured-let-bindings))
        body (analyze-children ctx exprs)]
    (ctx-fn
     (fn [ctx]
       (eval/eval-let ctx new-let-bindings body))
     nil)))

(defn expand-let
  "The let macro from clojure.core"
  [ctx [_let let-bindings  & exprs]]
  (let [let-bindings (destructure let-bindings)]
    (expand-let* ctx let-bindings exprs)))

(declare expand-declare)

(defn expand-def
  [ctx expr]
  (let [[_def var-name ?docstring ?init] expr]
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
            m (analyze (assoc ctx :meta true) m)
            m (assoc m :ns @vars/current-ns)
            m (if docstring (assoc m :doc docstring) m)
            var-name (with-meta var-name m)]
        (ctx-fn
         (fn [ctx]
           (eval/eval-def ctx var-name init))
         expr)))))

(defn expand-defn [ctx [op fn-name & body :as expr]]
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
        meta-map (analyze (assoc ctx :meta true) meta-map)
        fn-body (with-meta (cons 'fn body)
                  (meta expr))
        f (expand-fn* ctx fn-body macro?)
        arglists (seq (:sci.impl/arglists f))
        meta-map (assoc meta-map
                        :ns @vars/current-ns
                        :arglists arglists)
        fn-name (with-meta fn-name
                  (cond-> meta-map
                    docstring (assoc :doc docstring)
                    macro? (assoc :macro true)))
        f (assoc f
                 :sci/macro macro?
                 :sci.impl/fn-name fn-name
                 :sci.impl/defn true)
        fn-meta (:sci.impl/fn-meta f)
        ctxfn (fn-ctx-fn ctx f fn-meta)
        f (ctx-fn ctxfn f f)]
    (ctx-fn
     (fn [ctx]
       (eval/eval-def ctx fn-name f))
     expr)))

(defn expand-loop
  [ctx expr]
  (let [bv (second expr)
        arg-names (take-nth 2 bv)
        init-vals (take-nth 2 (rest bv))
        [bv syms] (if (every? symbol? arg-names)
                    [bv arg-names]
                    (let [syms (repeatedly (count arg-names) #(gensym))
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
        ana (analyze ctx (cons 'do body))]
    (ctx-fn (fn [ctx]
              (lazy-seq (eval/eval ctx ana)))
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
                       (fn [ctx]
                         (when (eval/eval ctx condition)
                           (eval/eval ctx then)))
                       ;; backward compatibility with stacktrace
                       (with-meta expr {:sci.impl/op :call}))))
      3 (let [condition (nth children 0)
              then (nth children 1)
              else (nth children 2)]
          (cond (not condition) else
                (constant? condition) then
                :else (ctx-fn
                       (fn [ctx]
                         (if (eval/eval ctx condition)
                           (eval/eval ctx then)
                           (eval/eval ctx else)))
                       ;; backward compatibility with stacktrace
                       (with-meta expr {:sci.impl/op :call}))))
      (throw-error-with-location "Too many arguments to if" expr))))

(defn analyze-case
  [ctx expr]
  (let [case-val (analyze ctx (second expr))
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
                       (if (list? k)
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
            (fn [ctx]
              (eval/eval-case ctx case-map case-val case-default))
            (fn [ctx]
              (eval/eval-case ctx case-map case-val)))]
    (ctx-fn
     f
     ;; legacy structure for error reporting
     (mark-eval-call (list 'case)))))

(defn analyze-try
  [ctx expr]
  (let [body (next expr)
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
                          (if-let [clazz (interop/resolve-class ctx ex)]
                            {:class clazz
                             :binding binding
                             :body (analyze (assoc-in ctx [:bindings binding] nil)
                                            (cons 'do body))}
                            (throw-error-with-location (str "Unable to resolve classname: " ex) ex))))
                      catches)
        finally (when finally
                  (analyze ctx (cons 'do (rest finally))))]
    (ctx-fn (fn [ctx]
              (eval/eval-try ctx body catches finally))
            expr)))

(defn analyze-throw [ctx [_throw ex :as expr]]
  (when-not (= 2 (count expr))
    (throw-error-with-location
     #?(:clj "Too many arguments to throw, throw expects a single Throwable instance"
        :cljs "Too many arguments to throw")
     expr))
  (let [ana (analyze ctx ex)]
    (ctx-fn (fn [ctx]
              (throw (eval/eval ctx ana)))
            ;; legacy structure for error reporting
            (mark-eval-call (list 'throw)))))

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
                                                                 {:name name
                                                                  :ns @vars/current-ns
                                                                  :file @vars/current-file}
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

(defn expand-dot [ctx [_dot instance-expr method-expr & args :as _expr]]
  (let [[method-expr & args] (if (seq? method-expr) method-expr
                                 (cons method-expr args))
        instance-expr (analyze ctx instance-expr)
        instance-expr (utils/vary-meta*
                       instance-expr
                       (fn [m]
                         (if-let [t (:tag m)]
                           (let [clazz (or (interop/resolve-class ctx t)
                                           (records/resolve-record-class ctx t)
                                           (throw-error-with-location
                                            (str "Unable to resolve classname: " t) t))]
                             (assoc m :tag-class clazz))
                           m)))
        method-expr (name method-expr)
        args (when args (analyze-children ctx args))
        res #?(:clj (if (class? instance-expr)
                      (if (nil? args)
                        (if (str/starts-with? method-expr "-")
                          (ctx-fn
                           (fn [_ctx]
                             (interop/get-static-field [instance-expr (subs method-expr 1)]))
                           (with-meta [instance-expr (subs method-expr 1)]
                             {:sci.impl/op :static-access}))
                          ;; https://clojure.org/reference/java_interop
                          ;; If the second operand is a symbol and no args are
                          ;; supplied it is taken to be a field access - the
                          ;; name of the field is the name of the symbol, and
                          ;; the value of the expression is the value of the
                          ;; field, unless there is a no argument public method
                          ;; of the same name, in which case it resolves to a
                          ;; call to the method.
                          (if-let [_
                                   (try (Reflector/getStaticField ^Class instance-expr ^String method-expr)
                                        (catch IllegalArgumentException _ nil))]
                            (ctx-fn
                             (fn [_ctx]
                               (interop/get-static-field [instance-expr method-expr]))
                             (with-meta [instance-expr (subs method-expr 1)]
                               {:sci.impl/op :static-access})) #_(with-meta [instance-expr method-expr]
                              {:sci.impl/op :static-access})
                            (ctx-fn
                             (fn [ctx]
                               (eval/eval-static-method-invocation ctx (cons [instance-expr method-expr] args)))
                             (mark-eval-call (cons (with-meta [instance-expr method-expr]
                                                     {:sci.impl/op :static-access})
                                                   args)))))
                        (ctx-fn
                         (fn [ctx]
                           (eval/eval-static-method-invocation ctx (cons [instance-expr method-expr] args)))
                         (mark-eval-call (cons (with-meta [instance-expr method-expr]
                                                 {:sci.impl/op :static-access})
                                               args))))
                      (ctx-fn (fn [ctx]
                                (eval/eval-instance-method-invocation ctx instance-expr method-expr args))
                              ;; this info is used by set!
                              {::instance-expr instance-expr
                               ::method-expr method-expr}
                              ;; legacy error reporting for (.foo 1)
                              (mark-eval-call _expr)
                              ))
               :cljs (ctx-fn (fn [ctx]
                               (eval/eval-instance-method-invocation ctx instance-expr method-expr args))
                             ;; this info is used by set!
                             {::instance-expr instance-expr
                              ::method-expr method-expr}
                             ;; legacy error reporting for (.foo 1)
                             (mark-eval-call _expr)))]
    res))

(defn expand-dot**
  "Expands (. x method)"
  [ctx expr]
  (when (< (count expr) 3)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (expand-dot ctx expr))

(defn expand-dot*
  "Expands (.foo x)"
  [ctx [method-name obj & args :as expr]]
  (when (< (count expr) 2)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (expand-dot ctx (list '. obj (cons (symbol (subs (name method-name) 1)) args))))

(defn analyze-new [ctx [_new class-sym & args :as expr]]
  (if-let [#?(:clj {:keys [:class] :as _opts}
              :cljs {:keys [:constructor] :as _opts}) (interop/resolve-class-opts ctx class-sym)]
    (let [args (analyze-children ctx args)] ;; analyze args!
      (ctx-fn
       (fn [ctx]
         (interop/invoke-constructor #?(:clj class :cljs constructor)
                                     (mapv #(eval/eval ctx %) args)))
       expr))
    (if-let [record (records/resolve-record-class ctx class-sym)]
      (let [args (analyze-children ctx args)]
        (return-call ctx
                     ;; for backwards compatibility with error reporting
                     (mark-eval-call (list* (:sci.impl.record/constructor (meta record)) args))
                     (:sci.impl.record/constructor (meta record))
                     args))
      (throw-error-with-location (str "Unable to resolve classname: " class-sym) class-sym))))

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
  (ctx-fn (fn [ctx]
            (apply f ctx analyzed-args))
          expr))

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
         expr
         (conj ret
               (ctx-fn
                (fn [ctx]
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
          (ctx-fn (fn [ctx]
                    (let [v (eval/eval ctx v)]
                      (types/setVal obj v)))
                  expr))
        #?@(:cljs [(seq? obj)
                   (let [obj (analyze ctx obj)
                         v (analyze ctx v)
                         obj (types/info obj)
                         k (subs (::method-expr obj) 1)
                         obj (::instance-expr obj)]
                     (ctx-fn (fn [ctx]
                               (let [obj (eval/eval ctx obj)
                                     v (eval/eval ctx v)]
                                 (gobject/set obj k v)))
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
       ~'[_ctx expr f analyzed-children]
       (ctx-fn
        (case (count ~'analyzed-children)
          ~@(concat
             (mapcat (fn [[i binds]]
                       [i `(let ~binds
                             (fn [~'ctx]
                               ((eval/resolve-symbol ~'ctx ~'f)
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                       (range i)))))])
                     let-bindings)
             `[(fn [~'ctx]
                 (eval/fn-call ~'ctx (eval/resolve-symbol ~'ctx ~'f) ~'analyzed-children))]))
        ~'expr))))

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
                             (fn [~'ctx]
                               (~'f ~'ctx
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                       (range i)))))])
                     let-bindings)
             `[(fn [~'ctx]
                 (eval/fn-call ~'ctx ~'f (cons ~'ctx ~'analyzed-children)))]))
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
       ~'[_ctx expr f analyzed-children]
       (ctx-fn
        (case (count ~'analyzed-children)
          ~@(concat
             (mapcat (fn [[i binds]]
                       [i `(let ~binds
                             (fn [~'ctx]
                               (~'f
                                ~@(map (fn [j]
                                         `(eval/eval ~'ctx ~(symbol (str "arg" j))))
                                       (range i)))))])
                     let-bindings)
             `[(fn [~'ctx]
                 (eval/fn-call ~'ctx ~'f ~'analyzed-children))]))
        ~'expr))))

(declare return-call) ;; for clj-kondo
(gen-return-call)

(defn analyze-quote [_ctx expr]
  (when-not (= 2 (count expr))
    (throw-error-with-location "Wrong number of args (0) passed to quote" expr))
  (let [snd (second expr)]
    (ctx-fn (fn [_ctx] snd) expr)))

(defn analyze-in-ns [ctx expr]
  (let [ns-expr (analyze ctx (second expr))]
    (ctx-fn (fn [ctx]
              (let [ns-sym (eval/eval ctx ns-expr)]
                (set-namespace! ctx ns-sym nil)
                nil))
            expr)))

(defn analyze-import [_ctx expr]
  (let [args (rest expr)]
    (ctx-fn (fn [ctx]
              (apply eval/eval-import ctx args))
            (mark-eval-call expr))))

(defn analyze-call [ctx expr top-level?]
  (let [f (first expr)]
    (cond (symbol? f)
          (let [;; in call position Clojure prioritizes special symbols over
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
                  #?(:clj (expand-dot** ctx (list* '. (first f) (second f) (rest expr)))
                     :cljs
                     (let [children (analyze-children ctx (rest expr))]
                       (ctx-fn (fn [ctx]
                                 (eval/eval-static-method-invocation ctx (cons f children)))
                               expr)))
                  (and (not eval?) ;; the symbol is not a binding
                       (or
                        special-sym
                        (contains? ana-macros f)))
                  (case f
                    ;; we treat every subexpression of a top-level do as a separate
                    ;; analysis/interpretation unit so we hand this over to the
                    ;; interpreter again, which will invoke analysis + evaluation on
                    ;; every sub expression
                    do (return-do expr (analyze-children ctx (rest expr)))
                    let (expand-let ctx expr)
                    (fn fn*) (expand-fn ctx expr false)
                    def (expand-def ctx expr)
                    ;; NOTE: defn / defmacro aren't implemented as normal macros yet
                    (defn defmacro) (let [ret (expand-defn ctx expr)]
                                      ret)
                    ;; TODO: implement as normal macro in namespaces.cljc
                    loop (expand-loop ctx expr)
                    lazy-seq (analyze-lazy-seq ctx expr)
                    for (let [res (expand-for ctx expr)]
                          (if (:sci.impl/macroexpanding ctx)
                            res
                            (analyze ctx res)))
                    doseq (analyze ctx (expand-doseq ctx expr))
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
                    ;; TODO: analyze if recur occurs in tail position, see #498
                    ;; recur (mark-eval-call (cons f (analyze-children ctx (rest expr))))
                    ;; else
                    or (return-or expr (analyze-children ctx (rest expr)))
                    and (return-and expr (analyze-children ctx (rest expr)))
                    recur (return-recur expr (analyze-children ctx (rest expr)))
                    in-ns (analyze-in-ns ctx expr)
                    (mark-eval-call (cons f (analyze-children ctx (rest expr)))))
                  :else
                  (try
                    (if (macro? f)
                      (let [needs-ctx? (identical? utils/needs-ctx
                                                   (:sci.impl/op (meta f)))
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
                                           :else (analyze ctx v))]
                        expanded)
                      (if-let [f (:sci.impl/inlined f-meta)]
                        (return-call ctx
                                     ;; for backwards compatibility with error reporting
                                     (mark-eval-call (cons f (rest expr))
                                                     :sci.impl/f-meta f-meta)
                                     f (analyze-children ctx (rest expr)))
                        (if-let [op (:sci.impl/op (meta f))]
                          (cond
                            (identical? utils/needs-ctx op)
                            (return-needs-ctx-call ctx
                                                   ;; no need to pass metadata for backwards compatibility
                                                   ;; since we weren't reporting needs-ctx-fns anyway
                                                   expr
                                                   f (analyze-children ctx (rest expr)))
                            (kw-identical? :resolve-sym op)
                            (return-binding-call ctx
                                                 ;; for backwards compatibility with error reporting
                                                 (mark-eval-call (cons f (rest expr))
                                                                 :sci.impl/f-meta f-meta)
                                                 f (analyze-children ctx (rest expr)))
                            :else
                            (mark-eval-call (cons f (analyze-children ctx (rest expr)))))
                          (let [children (analyze-children ctx (rest expr))]
                            (return-call ctx
                                         ;; for backwards compatibility with error reporting
                                         (mark-eval-call (cons f children)
                                                         :sci.impl/f-meta f-meta)
                                         f children)))))
                    (catch #?(:clj Exception :cljs js/Error) e
                      (rethrow-with-location-of-node ctx e
                                                     ;; adding metadata for error reporting
                                                     (mark-eval-call
                                                      (with-meta (cons f (rest expr))
                                                        (meta expr))))))))
          (keyword? f)
          (let [children (analyze-children ctx (rest expr))
                ccount (count children)]
            (case ccount
              1 (let [arg (nth children 0)]
                  (ctx-fn
                   (fn [ctx]
                     (f (eval/eval ctx arg)))
                   expr))
              2 (let [arg0 (nth children 0)
                      arg1 (nth children 1)]
                  (ctx-fn (fn [ctx]
                            (f (eval/eval ctx arg0)
                               (eval/eval ctx arg1)))
                          expr))
              (throw-error-with-location (str "Wrong number of args (" ccount ") passed to: " f) expr)))
          :else
          (let [f (analyze ctx f)
                children (analyze-children ctx (rest expr))]
            (ctx-fn (fn [ctx]
                      (let [f (eval/eval ctx f)]
                        (if (ifn? f)
                          (eval/fn-call ctx f children)
                          (throw (new #?(:clj Exception :cljs js/Error)
                                      (str "Cannot call " (pr-str f) " as a function."))))))
                    (mark-eval-call (cons f children)))))))

(def ^:const constant-colls true) ;; see GH #452

(defn return-map [ctx the-map]
  (let [children (into [] cat the-map)
        analyzed-children (analyze-children ctx children)]
    (if (<= (count analyzed-children) 16)
      (return-call ctx the-map array-map analyzed-children)
      (return-call ctx the-map hash-map analyzed-children))))

(defn analyze-map
  [ctx expr m]
  (let [ks (keys expr)
        vs (vals expr)
        constant-map? (and constant-colls
                           (every? constant? ks)
                           (every? constant? vs))
        analyzed-map (cond constant-map?
                           expr
                           ;; potential place for optimization
                           (not (:meta ctx))
                           (return-map ctx expr)
                           :else
                           (zipmap (analyze-children ctx ks)
                                   (analyze-children ctx vs)))
        analyzed-meta (when m (analyze (assoc ctx :meta true) m))
        analyzed-meta (if (and constant-map?
                               ;; meta was also a constant-map
                               (identical? m analyzed-meta))
                        analyzed-meta
                        (assoc analyzed-meta :sci.impl/op :eval))
        ret (if analyzed-meta
              (if (instance? #?(:clj sci.impl.types.EvalFn
                                :cljs sci.impl.types/EvalFn)
                             analyzed-map)
                (ctx-fn
                 (fn [ctx]
                   (let [md (eval/handle-meta ctx analyzed-meta)
                         coll (eval/eval ctx analyzed-map)]
                     (with-meta coll md)))
                 expr)
                (with-meta analyzed-map analyzed-meta))
              analyzed-map)]
    ret))

(defn analyze-vec-or-set
  "Returns analyzed vector or set"
  [ctx _f1 f2 expr m]
  (let [constant-coll?
        (and constant-colls
             (every? constant? expr))
        analyzed-meta (when m (analyze ctx #_(assoc ctx :meta true) m))
        must-eval (or (not constant-coll?)
                      (not (identical? m analyzed-meta)))
        analyzed-coll (if (not must-eval)
                        expr
                        (if m
                          ;; can we transform this into return-call?
                          (let [ef (return-call ctx expr f2 (analyze-children ctx expr))]
                            (ctx-fn
                             (fn [ctx]
                               (let [md (eval/eval ctx analyzed-meta)
                                     coll (eval/eval ctx ef)]
                                 (with-meta coll md)))
                             expr))
                          (return-call ctx expr f2 (analyze-children ctx expr))))]
    analyzed-coll))

(defn analyze
  ([ctx expr]
   (analyze ctx expr false))
  ([ctx expr top-level?]
   ;; (prn :ana expr (:meta ctx))
   (let [m (meta expr)]
     (cond
       (constant? expr) expr ;; constants do not carry metadata
       (symbol? expr) (let [v (resolve/resolve-symbol ctx expr false)]
                        (cond (constant? v) v
                              (vars/var? v)
                              (if (:const (meta v))
                                @v
                                (if (vars/isMacro v)
                                  (throw (new #?(:clj IllegalStateException :cljs js/Error)
                                              (str "Can't take value of a macro: " v "")))
                                  (types/->EvalVar v)))
                              :else (merge-meta v m)))
       ;; don't evaluate records, this check needs to go before map?
       ;; since a record is also a map
       (record? expr) expr
       (map? expr) (analyze-map ctx expr m)
       (vector? expr) (analyze-vec-or-set ctx
                                          ;; relying on analyze-children to
                                          ;; return a vector
                                          identity
                                          vector expr m)
       (set? expr) (analyze-vec-or-set ctx set hash-set expr m)
       (seq? expr) (if (seq expr)
                     (merge-meta (analyze-call ctx expr top-level?) m)
                     ;; the empty list
                     expr)
       :else
       expr))))

;;;; Scratch

(comment
  ;; _ctx expr f analyzed-children
  )
