(ns sci.impl.macros
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   [clojure.string :as str]
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.doseq-macro :refer [expand-doseq]]
   [sci.impl.for-macro :refer [expand-for]]
   [sci.impl.namespaces :as namespaces]
   [sci.impl.utils :refer
    [gensym* mark-resolve-sym mark-eval mark-eval-call constant? throw-error-with-location
     merge-meta kw-identical?]]))

(def macros '#{do if when and or -> ->> as-> quote quote*
               let fn fn* def defn comment loop lazy-seq for doseq
               require cond case try})

(defn check-permission! [{:keys [:allow :deny]} sym]
  (when-not (if allow (contains? allow sym)
                true)
    (throw-error-with-location (str sym " is not allowed!") sym))
  (when (if deny (contains? deny sym)
            false)
    (throw-error-with-location (str sym " is not allowed!") sym)))

(defn lookup-env [env sym sym-ns sym-name]
  (let [env @env]
    (or (find env sym)
        (when sym-ns
          (or (some-> env :namespaces sym-ns (find sym-name))
              (when-let [aliased (some-> env :aliases sym-ns)]
                (some-> env :namespaces aliased (find sym-name))))))))

(defn lookup [{:keys [:env :bindings] :as ctx} sym]
  (let [sym-ns (some-> (namespace sym) symbol)
        sym-name (symbol (name sym))
        [k v :as kv]
        (or (when-let [[k v]
                       (find bindings sym)]
              (if (:sci/macro (meta v))
                [k v]
                ;; never inline a binding at macro time!
                [k (mark-resolve-sym k)]))
            (lookup-env env sym sym-ns sym-name)
            (when-let [v (get macros sym)]
              (do (check-permission! ctx sym)
                  [v v]))
            (when-let [v (find namespaces/clojure-core sym)]
              (do (check-permission! ctx sym)
                  v))
            (when sym-ns
              (when (or (= 'clojure.core sym-ns)
                        (= 'cljs.core sym-ns))
                (let [unqualified-sym (symbol (name sym))]
                  (when-let [v (find namespaces/clojure-core unqualified-sym)]
                    (do (check-permission! ctx unqualified-sym)
                        v)))))
            (when (= 'recur sym)
              (check-permission! ctx sym)
              [sym (mark-resolve-sym sym)]))]
    ;; (prn 'lookup sym '-> res)
    (if-let [m (meta k)]
      (if (:sci/deref! m)
        ;; the evaluation of this expression has been delayed by
        ;; the caller and now is the time to deref it
        [k @v] kv)
      kv)))

(defn resolve-symbol [ctx expr]
  (let [res (second
             (or
              (lookup ctx expr)
              ;; TODO: check if symbol is in macros and then emit an error: cannot take
              ;; the value of a macro
              (let [n (name expr)]
                (if (str/starts-with? n "'")
                  (let [v (symbol (subs n 1))]
                    [v v])
                  (throw-error-with-location
                   (str "Could not resolve symbol: " (str expr))
                   expr)))))]
    ;; (prn 'resolve expr '-> res)
    res))

(declare macroexpand)

(defn expand-fn-args+body [ctx fn-name [binding-vector & body-exprs]]
  (let [fixed-args (take-while #(not= '& %) binding-vector)
        var-arg (second (drop-while #(not= '& %) binding-vector))
        fixed-arity (count fixed-args)
        fixed-names (vec (repeatedly fixed-arity gensym*))
        destructure-vec (vec (interleave binding-vector fixed-names))
        var-arg-name (when var-arg (gensym*))
        destructure-vec (if var-arg
                          (conj destructure-vec var-arg var-arg-name)
                          destructure-vec)
        arg-bindings (apply hash-map (interleave fixed-names (repeat nil)))
        ctx (cond-> (update ctx :bindings merge arg-bindings)
              var-arg
              (assoc-in [:bindings var-arg-name] nil))
        destructured-vec (destructure destructure-vec)
        ctx (update ctx :bindings merge (zipmap (take-nth 2 destructured-vec)
                                                (repeat nil)))
        body-form (mark-eval-call
                   `(~'let ~destructured-vec
                     ~@(doall (map #(macroexpand ctx %) body-exprs))))
        arg-list (if var-arg
                   (conj fixed-names '& var-arg-name)
                   fixed-names)]
    {:sci/arg-list arg-list
     :sci/body [body-form]
     :sci/fixed-arity fixed-arity
     :sci/destructure-vec destructure-vec
     :sci/fixed-names fixed-names
     :sci/fixed-args fixed-args
     :sci/var-arg-name var-arg-name
     :sci/fn-name fn-name}))

(defn expand-fn [ctx [_fn name? & body]]
  (let [fn-name (if (symbol? name?)
                  name?
                  nil)
        body (if fn-name
               body
               (cons name? body))
        fn-name (or fn-name (gensym* "fn"))
        bodies (if (seq? (first body))
                 body
                 [body])
        ctx (assoc-in ctx [:bindings fn-name] nil)
        arities (doall (map #(expand-fn-args+body ctx fn-name %) bodies))]
    (mark-eval
     {:sci/fn-bodies arities
      :sci/fn-name fn-name
      :sci/fn true})))

(defn expand-fn-literal-body [ctx expr]
  (let [fn-body (get-in expr [:sci/fn-bodies 0])
        fixed-names (:sci/fixed-names fn-body)
        var-arg-name (:sci/var-arg-name fn-body)
        bindings (if var-arg-name
                   (conj fixed-names var-arg-name)
                   fixed-names)
        bindings (zipmap bindings (repeat nil))
        ctx (update ctx :bindings merge bindings)]
    ;; expr
    (-> (update-in expr [:sci/fn-bodies 0 :sci/body 0]
                   (fn [expr]
                     (macroexpand ctx expr)))
        mark-eval)))

(defn expand-let*
  [ctx destructured-let-bindings exprs]
  (let [[ctx new-let-bindings]
        (reduce
         (fn [[ctx new-let-bindings] [binding-name binding-value]]
           (let [v (macroexpand ctx binding-value)]
             [(update ctx :bindings assoc binding-name v)
              (conj new-let-bindings binding-name v)]))
         [ctx []]
         (partition 2 destructured-let-bindings))]
    (mark-eval-call `(~'let ~new-let-bindings ~@(doall (map #(macroexpand ctx %) exprs))))))

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
    (macroexpand ctx expanded)))

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
    (macroexpand ctx expanded)))

(defn expand-as->
  "The ->> macro from clojure.core."
  [ctx [_as expr name & forms]]
  (let [[let-bindings & body] `([~name ~expr
                                 ~@(interleave (repeat name) (butlast forms))]
                                ~(if (empty? forms)
                                   name
                                   (last forms)))]
    (expand-let* ctx let-bindings body)))

(defn expand-def
  [ctx [_def var-name ?docstring ?init]]
  (let [docstring (when ?init ?docstring)
        init (if docstring ?init ?docstring)
        init (macroexpand ctx init)
        m (if docstring {:sci/doc docstring} {})
        var-name (with-meta var-name m)]
    (swap! (:env ctx) assoc var-name :sci/var.unbound)
    (mark-eval-call (list 'def var-name init))))

(defn expand-defn [ctx [_defn fn-name docstring? & body]]
  (let [docstring (when (string? docstring?) docstring?)
        body (if docstring body (cons docstring? body))
        fn-body (list* 'fn fn-name body)
        f (expand-fn ctx fn-body)]
    (swap! (:env ctx) assoc fn-name :sci/var.unbound)
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
    (macroexpand ctx (apply list (list 'fn (vec arg-names)
                                       (cons 'do body))
                            init-vals))))

(defn expand-lazy-seq
  [ctx expr]
  (let [body (rest expr)]
    (mark-eval-call
     (list 'lazy-seq
           (macroexpand ctx (list 'fn [] (cons 'do body)))))))

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
    (macroexpand ctx (apply expand-cond* clauses))))

(defn expand-case
  [ctx expr]
  (let [v (macroexpand ctx (second expr))
        clauses (nnext expr)
        match-clauses (take-nth 2 clauses)
        result-clauses (map #(macroexpand ctx %) (take-nth 2 (rest clauses)))
        default (when (odd? (count clauses))
                  [:val (macroexpand ctx (last clauses))])
        case-map (zipmap match-clauses result-clauses)
        ret (mark-eval-call (list 'case
                                  {:case-map case-map
                                   :case-val v
                                   :case-default default}
                                  default))]
    (mark-eval-call ret)))

(defn expand-try
  [ctx expr]
  (let [catches (filter #(and (seq? %) (= 'catch (first %))) expr)
        catches (map (fn [c]
                       (let [[_ ex binding & body] c]
                         {:class #?(:clj (java.lang.Class/forName (str ex)) :cljs ex)
                          :binding binding
                          :body (macroexpand (assoc-in ctx [:bindings binding] nil)
                                             (cons 'do body))}))
                     catches)
        finally (let [l (last expr)]
                  (when (= 'finally (first l))
                    (macroexpand ctx (cons 'do (rest l)))))]
    (mark-eval
     {:sci.impl/try
      {:body (macroexpand ctx (second expr))
       :catches catches
       :finally finally}})))

(defn macroexpand-call [ctx expr]
  (if (empty? expr) expr
      (let [f (first expr)]
        (if (symbol? f)
          (let [f (if-let [ns (namespace f)]
                    (if (or (= "clojure.core" ns)
                            (= "cljs.core" ns))
                      (symbol (name f))
                      f)
                    f)]
            (if (contains? macros f)
              (do (check-permission! ctx f)
                  (case f
                    do (mark-eval-call expr) ;; do will call macroexpand on every
                    ;; subsequent expression
                    let (expand-let ctx expr)
                    (fn fn*) (expand-fn ctx expr)
                    def (expand-def ctx expr)
                    defn (expand-defn ctx expr)
                    -> (expand-> ctx (rest expr))
                    ->> (expand->> ctx (rest expr))
                    as-> (expand-as-> ctx expr)
                    quote (do nil #_(prn "quote" expr) (second expr))
                    comment (expand-comment ctx expr)
                    loop (expand-loop ctx expr)
                    lazy-seq (expand-lazy-seq ctx expr)
                    for (macroexpand ctx (expand-for ctx expr))
                    doseq (macroexpand ctx (expand-doseq ctx expr))
                    require (mark-eval-call
                             (cons 'require (map #(macroexpand ctx %)
                                                 (rest expr))))
                    cond (expand-cond ctx expr)
                    case (expand-case ctx expr)
                    try (expand-try ctx expr)
                    ;; else:
                    (mark-eval-call (doall (map #(macroexpand ctx %) expr)))))
              (if-let [vf (resolve-symbol ctx f)]
                (if (:sci/macro (meta vf))
                  (macroexpand ctx (apply vf (rest expr)))
                  (mark-eval-call (doall (map #(macroexpand ctx %) expr))))
                (mark-eval-call (doall (map #(macroexpand ctx %) expr))))))
          (mark-eval-call (doall (map #(macroexpand ctx %) expr)))))))

(defn macroexpand
  [ctx expr]
  (cond (constant? expr) expr ;; constants do not carry metadata
        (symbol? expr) (let [v (resolve-symbol ctx expr)]
                         (cond (kw-identical? :sci/var.unbound v) nil
                               (constant? v) v
                               (fn? v) (merge-meta v {:sci.impl/eval false})
                               :else (merge-meta v (meta expr))))
        :else
        (merge-meta
         (cond
           ;; already expanded by reader
           (:sci/fn expr) (expand-fn-literal-body ctx expr)

           (map? expr)
           (-> (zipmap (map #(macroexpand ctx %) (keys expr))
                       (map #(macroexpand ctx %) (vals expr)))
               mark-eval)
           (or (vector? expr) (set? expr))
           (-> (into (empty expr) (map #(macroexpand ctx %) expr))
               mark-eval)
           (seq? expr) (macroexpand-call ctx expr)
           :else expr)
         (meta expr))))

;;;; Scratch

(comment
  )
