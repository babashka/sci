(ns sci.impl.macros
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.functions :as f]
   [sci.impl.utils :refer
    [gensym* mark-resolve-sym mark-eval mark-eval-call constant? throw-error-with-location
     merge-meta]]
   [clojure.string :as str]))

(def macros '#{do if when and or -> ->> as-> quote quote* let fn fn* def defn comment})

(defn allow?! [{:keys [:allow]} sym]
  (let [allowed? (if allow (contains? allow sym)
                     true)]
    (when-not allowed?
      (throw-error-with-location (str sym " is not allowed!") sym))))

(defn lookup [{:keys [:env :bindings] :as ctx} sym]
  (let [res (or (when-let [v (get macros sym)]
                  (do (allow?! ctx sym)
                      [v v]))
                (when-let [[k _v]
                           (find bindings sym)]
                  ;; never inline a binding at macro time!
                  [k (mark-resolve-sym k)])
                (find @env sym)
                (when-let [v (find f/functions sym)]
                  (do (allow?! ctx sym)
                      v))
                (when-let [ns (namespace sym)]
                  ;; (prn "NS" ns)
                  (when (or (= "clojure.core" ns)
                            (= "cljs.core" ns))
                    (let [unqualified-sym (symbol (name sym))]
                      (when-let [v (find f/functions unqualified-sym)]
                        (do (allow?! ctx unqualified-sym)
                            v))))))]
    ;; (prn 'lookup sym '-> res)
    res))

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
          (let [x (macroexpand ctx x)]
            (if forms
              (let [form (first forms)
                    threaded (if (seq? form)
                               (with-meta (concat (list (first form) x)
                                                  (next form))
                                 (meta form))
                               (list form x))]
                (recur threaded (next forms))) x)))]
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
[ctx & body])


(defn macroexpand-call [ctx expr]
  (if-let [f (first expr)]
    (if (symbol? f)
      (let [f (if-let [ns (namespace f)]
                (if (or (= "clojure.core" ns)
                        (= "cljs.core" ns))
                  (symbol (name f))
                  f)
                f)]
        (if (contains? macros f)
          (do (allow?! ctx f)
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
                ;; else:
                (mark-eval-call (doall (map #(macroexpand ctx %) expr)))))
          (mark-eval-call (doall (map #(macroexpand ctx %) expr)))))
      (mark-eval-call (doall (map #(macroexpand ctx %) expr))))
    expr))

(defn macroexpand
  [ctx expr]
  (merge-meta
   (cond
     (constant? expr) expr
     ;; already expanded by reader
     (:sci/fn expr) (expand-fn-literal-body ctx expr)
     (symbol? expr) (let [v (resolve-symbol ctx expr)]
                      (when-not (#?(:clj identical? :cljs keyword-identical?)
                                 :sci/var.unbound v)
                        v))
     (map? expr)
     (-> (zipmap (map #(macroexpand ctx %) (keys expr))
                 (map #(macroexpand ctx %) (vals expr)))
         mark-eval)
     (or (vector? expr) (set? expr))
     (-> (into (empty expr) (map #(macroexpand ctx %) expr))
         mark-eval)
     (seq? expr) (macroexpand-call ctx expr)
     :else expr)
   (meta expr)))

;;;; Scratch

(comment
  (macroexpand {:env (atom {}) :bindings f/functions} '(defn f [x] x))
  (macroexpand {:env (atom {}) :bindings f/functions} '((fn foo [x] (if (< x 3) (foo 1 (inc x)) x))))
  (macroexpand {:env (atom {}) :allow '#{int?}} '(int? 1))
  (macroexpand {:env (atom {}) :allow '#{int?}} '(boolean? 1))
  (macroexpand {:env (atom {})
                :allow '#{def}} '(defn f []))
  )
