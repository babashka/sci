(ns sci.impl.analyzer
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   [clojure.string :as str]
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.doseq-macro :refer [expand-doseq]]
   [sci.impl.for-macro :refer [expand-for]]
   [sci.impl.interop :as interop]
   [sci.impl.vars :as vars]
   [sci.impl.utils :as utils :refer
    [eval? gensym* mark-resolve-sym mark-eval mark-eval-call constant?
     rethrow-with-location-of-node throw-error-with-location
     merge-meta kw-identical? strip-core-ns]]))

;; derived from (keys (. clojure.lang.Compiler specials))
;; (& monitor-exit case* try reify* finally loop* do letfn* if clojure.core/import* new deftype* let* fn* recur set! . var quote catch throw monitor-enter def)
(def special-syms '#{try finally do if new recur quote catch throw def . var set!})

;; Built-in macros.
(def macros '#{do if when and or -> ->> as-> quote quote* syntax-quote let fn
               fn* def defn comment loop lazy-seq for doseq require cond case
               try defmacro declare expand-dot* expand-constructor new . import
               in-ns ns var set!})

(defn check-permission! [{:keys [:allow :deny]} check-sym sym]
  (when-not (kw-identical? :allow (-> sym meta :row))
    (let [check-sym (strip-core-ns check-sym)]
      (when-not (if allow (contains? allow check-sym)
                    true)
        (throw-error-with-location (str sym " is not allowed!") sym))
      (when (if deny (contains? deny check-sym)
                false)
        (throw-error-with-location (str sym " is not allowed!") sym)))))

(defn lookup* [{:keys [:env] :as ctx} sym]
  (let [sym-ns (some-> (namespace sym) symbol)
        sym-name (symbol (name sym))
        env @env
        current-ns (:current-ns env)
        the-current-ns (-> env :namespaces current-ns)]
    (or (find the-current-ns sym) ;; env can contain foo/bar symbols from bindings
        (cond
          (and sym-ns (or (= sym-ns 'clojure.core) (= sym-ns 'cljs.core))) ;; or cljs.core?
          (or (some-> env :namespaces (get 'clojure.core) (find sym-name))
              (when-let [v (get macros sym-name)]
                [sym v]))
          sym-ns
          (or (some-> env :namespaces sym-ns (find sym-name))
              (when-let [aliased (-> the-current-ns :aliases sym-ns)]
                (when-let [v (some-> env :namespaces aliased (get sym-name))]
                  [(symbol (str aliased) (str sym-name)) v]))
              (when-let [clazz (interop/resolve-class ctx sym-ns)]
                [sym (mark-eval ^:sci.impl/static-access [clazz sym-name])]))
          :else
          ;; no sym-ns, this could be a symbol from clojure.core
          (or
           (some-> env :namespaces (get 'clojure.core) (find sym-name))
           (when (get macros sym)
             [sym sym])
           (when-let [c (interop/resolve-class ctx sym)]
             [sym c]))))))

(defn lookup [{:keys [:bindings] :as ctx} sym]
  (let [[k v :as kv]
        (or
         ;; bindings are not checked for permissions
         (when-let [[k _v]
                    (find bindings sym)]
           ;; never inline a binding at macro time!
           [k (mark-resolve-sym k)])
         (when-let
             [[k _ :as kv]
              (or
               (lookup* ctx sym)
               #_(when (= 'recur sym)
                   [sym sym]))]
           (check-permission! ctx k sym)
           kv))]
    ;; (prn 'lookup sym '-> res)
    (if-let [m (meta k)]
      (if (:sci/deref! m)
        ;; the evaluation of this expression has been delayed by
        ;; the caller and now is the time to deref it
        [k @v] kv)
      kv)))

(defn resolve-symbol
  ([ctx sym] (resolve-symbol ctx sym false))
  ([ctx sym call?]
   (let [sym sym ;; (strip-core-ns sym)
         res (second
              (or
               (lookup ctx sym)
               ;; TODO: check if symbol is in macros and then emit an error: cannot take
               ;; the value of a macro
               (let [n (name sym)]
                 (cond
                   (and call?
                        (str/starts-with? n ".")
                        (> (count n) 1))
                   [sym 'expand-dot*] ;; method invocation
                   (and call?
                        (str/ends-with? n ".")
                        (> (count n) 1))
                   [sym 'expand-constructor]
                   (str/starts-with? n "'") ;; TODO: deprecated?
                   (let [v (symbol (subs n 1))]
                     [v v])
                   :else (throw-error-with-location
                          (str "Could not resolve symbol: " (str sym))
                          sym)))))]
     ;; (prn 'resolve sym '-> res)
     res)))

(declare analyze)

(defn analyze-children [ctx children]
  (mapv #(analyze ctx %) children))

(defn expand-fn-args+body [{:keys [:fn-expr] :as ctx} fn-name [binding-vector & body-exprs] macro?]
  (when-not binding-vector
    (throw-error-with-location "Parameter declaration missing." fn-expr))
  (when-not (vector? binding-vector)
    (throw-error-with-location "Parameter declaration should be a vector" fn-expr))
  (let [binding-vector (if macro? (into ['&form '&env] binding-vector)
                           binding-vector)
        fixed-args (take-while #(not= '& %) binding-vector)
        var-arg (second (drop-while #(not= '& %) binding-vector))
        fixed-arity (count fixed-args)
        fixed-names (vec (repeatedly fixed-arity gensym*))
        destructure-vec (vec (interleave binding-vector fixed-names))
        var-arg-name (when var-arg (gensym*))
        destructure-vec (if var-arg
                          (conj destructure-vec var-arg var-arg-name)
                          destructure-vec)
        destructured-vec (destructure destructure-vec)
        ;; all user-provided bindings are extracted by the destructure macro and
        ;; now we add them to bindings and continue the macroexpansion of the
        ;; body
        ctx (update ctx :bindings merge (zipmap (take-nth 2 destructured-vec)
                                                (repeat nil)))
        body-form (mark-eval-call
                   `(~'let ~destructured-vec
                     ;; we analyze the body expressions only once with the
                     ;; bindings in scope
                     ~@(analyze-children ctx body-exprs)))
        arg-list (if var-arg
                   (conj fixed-names '& var-arg-name)
                   fixed-names)]
    #:sci.impl{:arg-list arg-list
               :body [body-form]
               :fixed-arity fixed-arity
               :destructure-vec destructure-vec
               :fixed-names fixed-names
               :fixed-args fixed-args
               :var-arg-name var-arg-name
               :fn-name fn-name}))

(defn expand-fn [ctx [_fn name? & body :as fn-expr] macro?]
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
        arities (mapv #(expand-fn-args+body ctx fn-name % macro?) bodies)]
    (mark-eval #:sci.impl{:fn-bodies arities
                          :fn-name fn-name
                          :fn true})))

(defn expand-fn-literal-body [ctx expr]
  (let [fn-body (get-in expr [:sci.impl/fn-bodies 0])
        fixed-names (:sci.impl/fixed-names fn-body)
        var-arg-name (:sci.impl/var-arg-name fn-body)
        bindings (if var-arg-name
                   (conj fixed-names var-arg-name)
                   fixed-names)
        bindings (zipmap bindings (repeat nil))
        ctx (update ctx :bindings merge bindings)]
    ;; expr
    (-> (update-in expr [:sci.impl/fn-bodies 0 :sci.impl/body 0]
                   (fn [expr]
                     (analyze ctx expr)))
        (assoc :sci.impl/fn true)
        mark-eval)))

(defn expand-let*
  [ctx destructured-let-bindings exprs]
  (let [[ctx new-let-bindings]
        (reduce
         (fn [[ctx new-let-bindings] [binding-name binding-value]]
           (let [v (analyze ctx binding-value)]
             [(update ctx :bindings assoc binding-name v)
              (conj new-let-bindings binding-name v)]))
         [ctx []]
         (partition 2 destructured-let-bindings))]
    (mark-eval-call `(~'let ~new-let-bindings ~@(analyze-children ctx exprs)))))

(defn expand-let
  "The let macro from clojure.core"
  [ctx [_let let-bindings  & exprs]]
  (let [let-bindings (destructure let-bindings)]
    (expand-let* ctx let-bindings exprs)))

(defn expand->
  "The -> macro from clojure.core."
  [ctx [x & forms]]
  (let [expanded
        (loop [x x, forms forms]
          (if forms
            (let [form (first forms)
                  threaded (if (seq? form)
                             (with-meta (concat (list (first form) x)
                                                (next form))
                               (meta form))
                             (list form x))]
              (recur threaded (next forms))) x))]
    (analyze ctx expanded)))

(defn expand->>
  "The ->> macro from clojure.core."
  [ctx [x & forms]]
  (let [expanded
        (loop [x x, forms forms]
          (if forms
            (let [form (first forms)
                  threaded (if (seq? form)
                             (with-meta
                               (concat (cons (first form) (next form))
                                       (list x))
                               (meta form))
                             (list form x))]
              (recur threaded (next forms))) x))]
    (analyze ctx expanded)))

(defn expand-as->
  "The ->> macro from clojure.core."
  [ctx [_as expr name & forms]]
  (let [[let-bindings & body] `([~name ~expr
                                 ~@(interleave (repeat name) (butlast forms))]
                                ~(if (empty? forms)
                                   name
                                   (last forms)))]
    (expand-let* ctx let-bindings body)))

(declare expand-declare)

(defn expand-def
  [ctx [_def var-name ?docstring ?init :as expr]]
  (when-not (simple-symbol? var-name)
    (throw-error-with-location "Var name should be simple symbol." expr))
  (let [docstring (when ?init ?docstring)
        init (if docstring ?init ?docstring)
        init (analyze ctx init)
        m (meta var-name)
        m (if docstring (assoc m :sci/doc docstring) m)
        var-name (with-meta var-name m)]
    (expand-declare ctx [nil var-name])
    (mark-eval-call (list 'def var-name init))))

(defn expand-defn [ctx [op fn-name docstring? & body :as expr]]
  (when-not (simple-symbol? fn-name)
    (throw-error-with-location "Var name should be simple symbol." expr))
  (expand-declare ctx [nil fn-name])
  (let [macro? (= 'defmacro op)
        docstring (when (string? docstring?) docstring?)
        body (if docstring body
                 (if docstring?
                   (cons docstring? body)
                   (throw-error-with-location "Parameter declaration missing." expr)))
        fn-body (with-meta (list* 'fn body)
                  (meta expr))
        f (expand-fn ctx fn-body macro?)
        f (assoc f :sci/macro macro?
                 :sci.impl/fn-name fn-name)]
    (mark-eval-call (list 'def fn-name f))))

(defn expand-comment
  "The comment macro from clojure.core."
  [_ctx & _body])

(defn expand-loop
  [ctx expr]
  (let [bv (second expr)
        arg-names (take-nth 2 bv)
        init-vals (take-nth 2 (rest bv))
        body (nnext expr)]
    (analyze ctx (apply list (list 'fn (vec arg-names)
                                   ;; expand-fn will take care of the analysis of the body
                                   (cons 'do body))
                        init-vals))))

(defn expand-lazy-seq
  [ctx expr]
  (let [body (rest expr)]
    (mark-eval-call
     (list 'lazy-seq
           (analyze ctx
                    ;; expand-fn will take care of the analysis of the body
                    (list 'fn [] (cons 'do body)))))))

(defn expand-cond*
  "The cond macro from clojure.core"
  [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (new #?(:clj IllegalArgumentException
                           :cljs js/Error)
                        "cond requires an even number of forms")))
          (apply expand-cond* (next (next clauses))))))

(defn expand-cond
  [ctx expr]
  (let [clauses (rest expr)]
    (analyze ctx (apply expand-cond* clauses))))

(defn expand-case
  [ctx expr]
  (let [v (analyze ctx (second expr))
        clauses (nnext expr)
        match-clauses (take-nth 2 clauses)
        result-clauses (analyze-children ctx (take-nth 2 (rest clauses)))
        default (when (odd? (count clauses))
                  [:val (analyze ctx (last clauses))])
        case-map (zipmap match-clauses result-clauses)
        ret (mark-eval-call (list 'case
                                  {:case-map case-map
                                   :case-val v
                                   :case-default default}
                                  default))]
    (mark-eval-call ret)))

(defn expand-try
  [ctx [_try & body]]
  (let [[body-exprs
         catches
         finally]
        (loop [[expr & exprs] body
               body-exprs []
               catch-exprs []
               finally-expr nil]
          (if expr
            (cond (and (seq? expr) (= 'catch (first expr)))
                  (recur exprs body-exprs (conj catch-exprs expr) finally-expr)
                  (and (empty? exprs) (and (seq? expr) (= 'finally (first expr))))
                  [body-exprs catch-exprs expr]
                  :else
                  ;; TODO: cannot add body expression when catch is not empty
                  ;; TODO: can't have finally as non-last expression
                  (recur exprs (conj body-exprs expr) catch-exprs finally-expr))
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
    (mark-eval
     {:sci.impl/try
      {:body body
       :catches catches
       :finally finally}})))

(defn expand-syntax-quote [ctx expr]
  (let [ret (utils/prewalk
             (fn [x]
               (if (seq? x)
                 (case (first x)
                   unquote (analyze ctx (second x))
                   unquote-splicing (vary-meta
                                     (analyze ctx (second x))
                                     (fn [m]
                                       (assoc m :sci.impl/unquote-splicing true)))
                   x)
                 x))
             (second expr))]
    (mark-eval-call (list 'syntax-quote ret))))

(defn expand-declare [ctx [_declare & names :as _expr]]
  (swap! (:env ctx)
         (fn [env]
           (let [current-ns (:current-ns env)]
             (update-in env [:namespaces current-ns]
                        (fn [current-ns]
                          (reduce (fn [acc name]
                                    (if (contains? acc name)
                                      ;; declare does not override an existing
                                      ;; var
                                      acc
                                      (assoc acc name (vary-meta (mark-eval name)
                                                                 #(assoc % :sci.impl/var.declared true)))))
                                  current-ns
                                  names))))))
  nil)

(defn do-import [{:keys [:env] :as ctx} [_ & import-symbols-or-lists :as expr]]
  (let [specs (map #(if (and (seq? %) (= 'quote (first %))) (second %) %)
                   import-symbols-or-lists)]
    (doseq [spec (reduce (fn [v spec]
                           (if (symbol? spec)
                             (conj v (name spec))
                             (let [p (first spec) cs (rest spec)]
                               (into v (map #(str p "." %) cs)))))
                         [] specs)]
      (let [fq-class-name (symbol spec)]
        (when-not (interop/resolve-class ctx fq-class-name)
          (throw-error-with-location (str "Unable to resolve classname: " fq-class-name) expr))
        (let [last-dot (str/last-index-of spec ".")
              class-name (subs spec (inc last-dot) (count spec))]
          (swap! env assoc-in [:imports (symbol class-name)] fq-class-name))))))

;;;; Interop

(defn expand-dot [ctx [_dot instance-expr [method-expr & args]]]
  (let [instance-expr (analyze ctx instance-expr)
        method-expr (str method-expr)
        args (analyze-children ctx args)
        res `(~'. ~instance-expr ~method-expr ~args)]
    (mark-eval-call res)))

(defn expand-dot* [ctx [method-name obj & args]]
  (expand-dot ctx (list '. obj (cons (symbol (subs (name method-name) 1)) args))))


(defn expand-new [ctx [_new class-sym & args]]
  (if-let [clazz (interop/resolve-class ctx class-sym)]
    (let [args (analyze-children ctx args)] ;; analyze args!
      (mark-eval-call (list 'new clazz args)))
    (throw-error-with-location (str "Unable to resolve classname: " class-sym) class-sym)))

(defn expand-constructor [ctx [constructor-sym & args]]
  (let [;; TODO:
        ;; here is strips the namespace, which is correct in the case of
        ;; js/Error. but not in clj
        constructor-name (name constructor-sym)
        class-sym (with-meta (symbol (subs constructor-name 0
                                           (dec (count constructor-name))))
                    (meta constructor-sym))]
    (expand-new ctx (with-meta (list* 'new class-sym args)
                      (meta constructor-sym)))))

;;;; End interop

;;;; Namespaces

(defn analyze-ns-form [ctx [_ns ns-name & exprs]]
  (let [;; skip docstring
        exprs (if (string? (first exprs))
                (rest exprs)
                exprs)
        ;; skip attr-map
        exprs (if (map? (first exprs))
                (rest exprs)
                exprs)]
    (swap! (:env ctx) assoc :current-ns ns-name)
    (loop [exprs exprs
           ret [(mark-eval-call (list 'in-ns ns-name))]]
      (if exprs
        (let [[k & args] (first exprs)]
          (case k
            :require (recur (next exprs) (conj ret
                                               (mark-eval-call `(~'require ~@args))))
            :import (do
                      ;; imports are processed analysis time
                      (do-import ctx `(~'import ~@args))
                      (recur (next exprs) ret))))
        (mark-eval-call (list* 'do ret))))))

;;;; End namespaces


;;;; Vars

(defn analyze-var [ctx [_ var-name]]
  (let [v (resolve-symbol ctx var-name)]
    (when (vars/var? v)
      (vary-meta v #(assoc % :sci.impl/eval false))
      ;; TODO: exception?
      )))

(defn analyze-set! [ctx [_ obj v]]
  (let [obj (analyze ctx obj)
        v (analyze ctx v)]
    (mark-eval-call (list 'set! obj v))))

;;;;

(defn macro? [f]
  (when-let [m (meta f)]
    (:sci/macro m)))

(defn analyze-call [ctx expr]
  (let [f (first expr)]
    (if (symbol? f)
      (let [;; in call position Clojure prioritizes special symbols over
            ;; bindings
            special-sym (get special-syms f)
            _ (when special-sym (check-permission! ctx special-sym f))
            f (or special-sym
                  (resolve-symbol ctx f true))]
        (if (and (not (eval? f)) ;; the symbol is not a binding
                 (or
                  special-sym
                  (contains? macros f)))
          (case f
            ;; we treat every subexpression of a top-level do as a separate
            ;; analysis/interpretation unit so we hand this over to the
            ;; interpreter again, which will invoke analysis + evaluation on
            ;; every sub expression
            do (mark-eval-call (cons 'do
                                     (analyze-children ctx (rest expr))))
            let (expand-let ctx expr)
            (fn fn*) (expand-fn ctx expr false)
            def (expand-def ctx expr)
            (defn defmacro) (let [ret (expand-defn ctx expr)]
                              ret)
            -> (expand-> ctx (rest expr))
            ->> (expand->> ctx (rest expr))
            as-> (expand-as-> ctx expr)
            quote (do nil (second expr))
            syntax-quote (expand-syntax-quote ctx expr)
            comment (expand-comment ctx expr)
            loop (expand-loop ctx expr)
            lazy-seq (expand-lazy-seq ctx expr)
            for (analyze ctx (expand-for ctx expr))
            doseq (analyze ctx (expand-doseq ctx expr))
            require (mark-eval-call
                     (cons 'require (analyze-children ctx (rest expr))))
            cond (expand-cond ctx expr)
            case (expand-case ctx expr)
            try (expand-try ctx expr)
            declare (expand-declare ctx expr)
            expand-dot* (expand-dot* ctx expr)
            . (expand-dot ctx expr)
            expand-constructor (expand-constructor ctx expr)
            new (expand-new ctx expr)
            import (do-import ctx expr)
            ns (analyze-ns-form ctx expr)
            var (analyze-var ctx expr)
            set! (analyze-set! ctx expr)
            ;; else:
            (mark-eval-call (cons f (analyze-children ctx (rest expr)))))
          (try
            (if (macro? f)
              (let [v (apply f expr
                             (:bindings ctx) (rest expr))
                    expanded (analyze ctx v)]
                expanded)
              (mark-eval-call (analyze-children ctx expr)))
            (catch #?(:clj Exception :cljs js/Error) e
              (rethrow-with-location-of-node ctx e expr)))))
      (let [ret (mark-eval-call (analyze-children ctx expr))]
        ret))))

(defn analyze
  [ctx expr]
  (let [ret (cond (constant? expr) expr ;; constants do not carry metadata
                  (symbol? expr) (let [v (resolve-symbol ctx expr)]
                                   (cond (constant? v) v
                                         (fn? v) (merge-meta v {:sci.impl/eval false})
                                         (vars/var? v) (with-meta v (assoc (meta v) :sci.impl/eval true))
                                         :else (merge-meta v (meta expr))))
                  :else
                  (merge-meta
                   (cond
                     ;; reader result still needs analysis
                     (:sci.impl/fn-literal expr) (expand-fn-literal-body ctx expr)
                     (map? expr)
                     (-> (zipmap (analyze-children ctx (keys expr))
                                 (analyze-children ctx (vals expr)))
                         mark-eval)
                     (or (vector? expr) (set? expr))
                     (-> (into (empty expr) (analyze-children ctx expr))
                         mark-eval)
                     (and (seq? expr) (seq expr))
                     (analyze-call ctx expr)
                     :else expr)
                   (select-keys (meta expr)
                                [:row :col])))]
    ret))

;;;; Scratch

(comment
  )
