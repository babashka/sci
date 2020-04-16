(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-1])
  (:require
   [clojure.tools.reader.reader-types :as r]
   [sci.impl.analyzer :as ana]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.macros :as macros]
   [sci.impl.max-or-throw :refer [max-or-throw]]
   [sci.impl.opts :as opts]
   [sci.impl.parser :as p]
   [sci.impl.types :as t]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     rethrow-with-location-of-node
                                     set-namespace!
                                     kw-identical?]]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros [sci.impl.interpreter :refer [def-fn-call]])))

(declare interpret fn-call)

#?(:clj (set! *warn-on-reflection* true))

(def #?(:clj ^:const macros :cljs macros)
  '#{do if and or quote let fn def defn
     lazy-seq try syntax-quote case . in-ns set!
     macroexpand-1 macroexpand require})

;;;; Evaluation

(defn eval-and
  "The and macro from clojure.core."
  [ctx args]
  (let [args (seq args)]
    (loop [args args]
      (if args
        (let [x (first args)
              xs (next args)
              v (interpret ctx x)]
          (if v
            (if xs
              (recur xs) v) v))
        true))))

(defn eval-or
  "The or macro from clojure.core."
  [ctx args]
  (let [args (seq args)]
    (loop [args args]
      (when args
        (let [x (first args)
              xs (next args)
              v (interpret ctx x)]
          (if v v
              (if xs (recur xs)
                  v)))))))

(defn eval-let
  "The let macro from clojure.core"
  [ctx let-bindings & exprs]
  (let [ctx (loop [ctx ctx
                   let-bindings let-bindings]
              (let [let-name (first let-bindings)
                    let-bindings (rest let-bindings)
                    let-val (first let-bindings)
                    rest-let-bindings (next let-bindings)
                    val-tag (when-let [m (meta let-val)]
                              (:tag m))
                    let-name (if val-tag
                               (vary-meta let-name update :tag (fn [t]
                                                                 (if t t val-tag)))
                               let-name)
                    v (interpret ctx let-val)
                    ctx (assoc-in ctx [:bindings let-name] v)]
                (if-not rest-let-bindings
                  ctx
                  (recur ctx
                         rest-let-bindings))))]
    (when exprs
      (loop [exprs exprs]
        (let [e (first exprs)
              ret (interpret ctx e)
              nexprs (next exprs)]
          (if nexprs (recur nexprs)
              ret))))))

(defn eval-if
  [ctx expr]
  ;; NOTE: not using destructuring for small perf gain
  (let [cond (first expr)
        expr (rest expr)
        then (first expr)
        expr (rest expr)
        else (first expr)]
    (if (interpret ctx cond)
      (interpret ctx then)
      (interpret ctx else))))

(defn eval-def
  [ctx [_def var-name ?docstring ?init]]
  (let [docstring (when ?init ?docstring)
        init (if docstring ?init ?docstring)
        init (interpret ctx init)
        m (meta var-name)
        m (interpret ctx m)
        cnn (vars/getName (:ns m))
        assoc-in-env
        (fn [env]
          (let [the-current-ns (get-in env [:namespaces cnn])
                prev (get the-current-ns var-name)
                v (if (kw-identical? :sci.impl/var.unbound init)
                    (doto prev
                      (alter-meta! merge m))
                    (do (vars/bindRoot prev init)
                        (alter-meta! prev merge m)
                        prev))
                the-current-ns (assoc the-current-ns var-name v)]
            (assoc-in env [:namespaces cnn] the-current-ns)))
        env (swap! (:env ctx) assoc-in-env)]
    ;; return var instead of init-val
    (get-in env [:namespaces cnn var-name])))

(defn resolve-symbol [ctx sym]
  (let [^java.util.Map bindings (.get ^java.util.Map ctx :bindings)]
    (#?@(:clj [if (.containsKey bindings sym) (.get bindings sym)]
         :cljs [if-let [v (find bindings sym)] (second v)])
     ;; TODO: check if symbol is in macros and then emit an error: cannot take
     ;; the value of a macro
     (throw-error-with-location
      (str "Could not resolve symbol: " sym "\nks:" (keys (:bindings ctx)))
      sym))))

(defn parse-libspec [libspec]
  (cond
    (sequential? libspec)
    (let [[lib-name & opts] libspec]
      (loop [ret {:lib-name lib-name}
             [opt-name fst-opt & rst-opts] opts]
        (if-not opt-name ret
                (case opt-name
                  :as (recur (assoc ret :as fst-opt)
                             rst-opts)
                  (:reload :reload-all :verbose) (recur
                                                  (assoc ret :reload true)
                                                  (cons fst-opt rst-opts))
                  (:refer :rename :exclude :only) (recur (assoc ret opt-name fst-opt)
                                                         rst-opts)))))
    (symbol? libspec) {:lib-name libspec}
    :else (throw (new #?(:clj Exception :cljs js/Error)
                      (str "Invalid libspec: " libspec)))))

(declare eval-string*)

(defn handle-refer-all [the-current-ns the-loaded-ns include-sym? rename-sym only]
  (let [only (when only (set only))]
    (reduce (fn [ns [k v]]
              (if (and (symbol? k) (include-sym? k)
                       (or (not only)
                           (contains? only k)))
                (assoc ns (rename-sym k) v)
                ns))
            the-current-ns
            the-loaded-ns)))

(defn handle-require-libspec-env
  [env use? current-ns the-loaded-ns lib-name
   {:keys [:as :refer :rename :exclude :only] :as _parsed-libspec}]
  (let [the-current-ns (get-in env [:namespaces current-ns]) ;; = ns-data?
        the-current-ns (if as (assoc-in the-current-ns [:aliases as] lib-name)
                           the-current-ns)
        rename-sym (if rename (fn [sym] (or (rename sym) sym))
                       identity)
        include-sym? (if exclude
                       (let [excludes (set exclude)]
                         (fn [sym]
                           (not (contains? excludes sym))))
                       (constantly true))
        the-current-ns
        (cond refer
              (cond (or (kw-identical? :all refer)
                        use?)
                    (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym nil)
                    (sequential? refer)
                    (reduce (fn [ns sym]
                              (if (include-sym? sym)
                                (assoc ns (rename-sym sym)
                                       (if-let [[_k v] (find the-loaded-ns sym)]
                                         v
                                         (throw (new #?(:clj Exception :cljs js/Error)
                                                     (str sym " does not exist")))))
                                ns))
                            the-current-ns
                            refer)
                    :else (throw (new #?(:clj Exception :cljs js/Error)
                                      (str ":refer value must be a sequential collection of symbols"))))
              use? (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym only)
              :else the-current-ns)
        env (assoc-in env [:namespaces current-ns] the-current-ns)]
    env))

(defn handle-require-libspec
  [ctx libspec]
  (let [{:keys [:lib-name :reload] :as parsed-libspec} (parse-libspec libspec)
        env* (:env ctx)
        env @env* ;; NOTE: loading namespaces is not (yet) thread-safe
        cnn (vars/current-ns-name)
        namespaces (get env :namespaces)
        use? (:sci.impl/use ctx)]
    (if-let [the-loaded-ns (when-not reload (get namespaces lib-name))]
      (reset! env* (handle-require-libspec-env env use? cnn the-loaded-ns lib-name parsed-libspec))
      (if-let [load-fn (:load-fn ctx)]
        (if-let [{:keys [:file :source]} (load-fn {:namespace lib-name})]
          (do
            (try (vars/with-bindings {vars/current-file file}
                   (eval-string* (assoc ctx :bindings {}) source))
                 (catch #?(:clj Exception :cljs js/Error) e
                   (swap! env* update :namespaces dissoc lib-name)
                   (throw e))
                 ;; TODO: fix ns metadata
                 (finally (set-namespace! ctx cnn nil)))
            (swap! env* (fn [env]
                          (let [namespaces (get env :namespaces)
                                the-loaded-ns (get namespaces lib-name)]
                            (handle-require-libspec-env env use? cnn
                                                        the-loaded-ns
                                                        lib-name parsed-libspec)))))
          (or (when reload
                (when-let [the-loaded-ns (get namespaces lib-name)]
                  (reset! env* (handle-require-libspec-env env use? cnn the-loaded-ns lib-name parsed-libspec))))
              (throw (new #?(:clj Exception :cljs js/Error)
                          (str "Could not require " lib-name ".")))))
        (throw (new #?(:clj Exception :cljs js/Error)
                    (str "Could not require " lib-name ".")))))))

(defn eval-require
  [ctx & args]
  (loop [libspecs []
         current-libspec nil
         args args]
    (if args
      (let [ret (interpret ctx (first args))]
        (cond
          (symbol? ret)
          (recur (cond-> libspecs
                   current-libspec (conj current-libspec))
                 [ret]
                 (next args))
          (keyword? ret)
          (recur (conj libspecs (conj current-libspec ret))
                 nil
                 (next args))
          :else
          (recur (cond-> libspecs
                   current-libspec (conj current-libspec))
                 ret
                 (next args))))
      (let [libspecs (cond-> libspecs
                       current-libspec (conj current-libspec))]
        (run! #(handle-require-libspec ctx %) libspecs)))))

(vreset! utils/eval-require-state eval-require)

(defn eval-use
  [ctx & args]
  (apply eval-require (assoc ctx :sci.impl/use true) args))

(vreset! utils/eval-use-state eval-use)

(defn eval-case
  [ctx [_case {:keys [:case-map :case-val :case-default]}]]
  (let [v (interpret ctx case-val)]
    (if-let [[_ found] (find case-map v)]
      (interpret ctx found)
      (if (vector? case-default)
        (interpret ctx (second case-default))
        (throw (new #?(:clj Exception :cljs js/Error)
                    (str "No matching clause: " v)))))))

(defn eval-try
  [ctx expr]
  (let [{:keys [:body :catches :finally]} (:sci.impl/try expr)]
    (try
      (binding [utils/*in-try* true]
        (interpret ctx body))
      (catch #?(:clj Throwable :cljs js/Error) e
        (if-let
            [[_ r]
             (reduce (fn [_ c]
                       (let [clazz (:class c)]
                         (when (instance? clazz e)
                           (reduced
                            [::try-result
                             (interpret (assoc-in ctx [:bindings (:binding c)]
                                                  e)
                                        (:body c))]))))
                     nil
                     catches)]
          r
          (rethrow-with-location-of-node ctx e body)))
      (finally
        (interpret ctx finally)))))

(defn eval-throw [ctx [_throw ex]]
  (let [ex (interpret ctx ex)]
    (throw ex)))

;;;; Interop

(defn eval-static-method-invocation [ctx expr]
  (interop/invoke-static-method (first expr)
                                ;; eval args!
                                (map #(interpret ctx %) (rest expr))))

(defn eval-constructor-invocation [ctx [_new #?(:clj class :cljs constructor) args]]
  (let [args (map #(interpret ctx %) args)] ;; eval args!
    (interop/invoke-constructor #?(:clj class :cljs constructor) args)))

#?(:clj
   (defn super-symbols [clazz]
     ;; (prn clazz '-> (map #(symbol (.getName ^Class %)) (supers clazz)))
     (map #(symbol (.getName ^Class %)) (supers clazz))))

(defn eval-instance-method-invocation [{:keys [:class->opts] :as ctx} [_dot instance-expr method-str args]]
  (let [instance-meta (meta instance-expr)
        tag-class (:tag-class instance-meta)
        instance-expr* (interpret ctx instance-expr)
        instance-class (or tag-class (#?(:clj class :cljs type) instance-expr*))
        instance-class-name #?(:clj (.getName ^Class instance-class)
                               :cljs (.-name instance-class))
        instance-class-symbol (symbol instance-class-name)
        allowed? (or
                  (get class->opts :allow)
                  (get class->opts instance-class-symbol))
        ^Class target-class (if allowed? instance-class
                                (when-let [f (:public-class ctx)]
                                  (f instance-expr*)))]
    ;; we have to check options at run time, since we don't know what the class
    ;; of instance-expr is at analysis time
    (when-not target-class
      (throw-error-with-location (str "Method " method-str " on " instance-class " not allowed!") instance-expr))
    (let [args (map #(interpret ctx %) args)] ;; eval args!
      (interop/invoke-instance-method instance-expr* target-class method-str args))))

;;;; End interop

;;;; Namespaces

(defn eval-in-ns [ctx [_in-ns ns-expr]]
  (let [ns-sym (interpret ctx ns-expr)]
    (set-namespace! ctx ns-sym nil)
    nil))

(defn eval-refer [ctx [_ ns-sym & exprs]]
  (let [ns-sym (interpret ctx ns-sym)]
    (loop [exprs exprs]
      (when exprs
        (let [[k v] exprs]
          (case k
            :exclude
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (vars/current-ns-name)]
                       (update-in env [:namespaces cnn :refer ns-sym :exclude]
                                  (fnil into #{}) v)))))
          (recur (nnext exprs)))))))

(declare eval-form)

(defn eval-resolve [ctx sym]
  (let [sym (interpret ctx sym)]
    (second (ana/lookup ctx sym false))))

(vreset! utils/eval-resolve-state eval-resolve)

;;;; End namespaces

;;;; Macros

(defn macroexpand-1 [ctx expr]
  (let [original-expr expr]
    (if (seq? expr)
      (let [op (first expr)]
        (if (symbol? op)
          (cond (get ana/special-syms op) expr
                (contains? #{'for} op) (ana/analyze (assoc ctx :sci.impl/macroexpanding true)
                                                    expr)
                :else
                (let [f (ana/resolve-symbol ctx op true)
                      f (if (and (vars/var? f)
                                 (vars/isMacro f))
                          @f f)]
                  (if (ana/macro? f)
                    (apply f original-expr (:bindings ctx) (rest expr))
                    expr)))
          expr))
      expr)))

(defn macroexpand
  [ctx form]
  (let [ex (macroexpand-1 ctx form)]
    (if (identical? ex form)
      form
      (macroexpand ctx ex))))

;;;; End macros

(defn eval-set! [ctx [_ obj v]]
  (let [obj (interpret ctx obj)
        v (interpret ctx v)]
    (if (vars/var? obj)
      (t/setVal obj v)
      (throw (ex-info (str "Cannot set " obj " to " v) {:obj obj :v v})))))

(declare eval-string)

(defn eval-do*
  [ctx exprs]
  (loop [[expr & exprs] exprs]
    (let [ret (try (interpret ctx expr)
                   (catch #?(:clj Throwable :cljs js/Error) e
                     (rethrow-with-location-of-node ctx e expr)))]
      (if-let [exprs (seq exprs)]
        (recur exprs)
        ret))))

(defn eval-do
  [ctx expr]
  (when-let [exprs (next expr)]
    (eval-do* ctx exprs)))

(macros/deftime
  ;; This macro generates a function of the following form for 20 arities:
  #_(defn fn-call [ctx f args]
      (case (count args)
        0 (f)
        1 (let [arg (interpret ctx (first args))]
            (f arg))
        2 (let [arg1 (interpret ctx (first args))
                args (rest args)
                arg2 (interpret ctx (first args))]
            (f arg1 arg2))
        ,,,
        (let [args (mapv #(interpret ctx %) args)]
          (apply f args))))
  (defmacro def-fn-call []
    (let [cases
          (mapcat (fn [i]
                    [i (let [arg-syms (map (fn [_] (gensym "arg")) (range i))
                             args-sym 'args ;; (gensym "args")
                             let-syms (interleave arg-syms (repeat args-sym))
                             let-vals (interleave (repeat `(interpret ~'ctx (first ~args-sym)))
                                                  (repeat `(rest ~args-sym)))
                             let-bindings (vec (interleave let-syms let-vals))]
                         `(let ~let-bindings
                            (~'f ~@arg-syms)))]) (range 20))
          cases (concat cases ['(let [args (mapv #(interpret ctx %) args)]
                                  (apply f args))])]
      `(defn ~'fn-call ~'[ctx f args]
         (case ~'(count args)
           ~@cases)))))

(def-fn-call)

(defn eval-special-call [ctx f-sym expr]
  (case (utils/strip-core-ns f-sym)
    do (eval-do ctx expr)
    if (eval-if ctx (rest expr))
    and (eval-and ctx (rest expr))
    or (eval-or ctx (rest expr))
    let (apply eval-let ctx (rest expr))
    def (eval-def ctx expr)
    lazy-seq (new #?(:clj clojure.lang.LazySeq
                     :cljs cljs.core/LazySeq)
                  #?@(:clj []
                      :cljs [nil])
                  (interpret ctx (second expr))
                  #?@(:clj []
                      :cljs [nil nil]))
    recur (fn-call ctx (comp fns/->Recur vector) (rest expr))
    case (eval-case ctx expr)
    try (eval-try ctx expr)
    ;; interop
    new (when-not (.get ^java.util.Map ctx :dry-run)
          (eval-constructor-invocation ctx expr))
    . (when-not (.get ^java.util.Map ctx :dry-run)
        (eval-instance-method-invocation ctx expr))
    throw (eval-throw ctx expr)
    in-ns (eval-in-ns ctx expr)
    set! (eval-set! ctx expr)
    refer (eval-refer ctx expr)
    require (apply eval-require ctx (rest expr))
    use (apply eval-use ctx (rest expr))
    resolve (eval-resolve ctx (second expr))
    macroexpand-1 (macroexpand-1 ctx (interpret ctx (second expr)))
    macroexpand (macroexpand ctx (interpret ctx (second expr)))))

(defn eval-call [ctx expr]
  (try (let [f (first expr)
             m (meta f)
             op (when m (.get ^java.util.Map m :sci.impl/op))]
         ;; (prn "call first op" (type f) op)
         (cond
           (and (symbol? f) (not op))
           (eval-special-call ctx f expr)
           (kw-identical? op :static-access)
           (when-not (.get ^java.util.Map ctx :dry-run)
             (eval-static-method-invocation ctx expr))
           :else
           (let [f (if op (interpret ctx f)
                       f)]
             (if (ifn? f)
               (when-not (.get ^java.util.Map ctx :dry-run)
                 (fn-call ctx f (rest expr)))
               (throw (new #?(:clj Exception :cljs js/Error)
                           (str "Cannot call " (pr-str f) " as a function.")))))))
       (catch #?(:clj Throwable :cljs js/Error) e
         (rethrow-with-location-of-node ctx e expr))))

(defn fix-meta [v old-meta]
  ;; TODO: find out why the special case for vars is needed. When I remove it,
  ;; spartan.spec does not work.
  (if (and (meta v) (and (not (vars/var? v))
                         (not (vars/namespace? v))))
    (vary-meta v (fn [m]
                   (-> m
                       (dissoc :sci.impl/op)
                       (assoc :line (:line old-meta)))))
    v))

(defn interpret
  [ctx expr]
  (if (instance? sci.impl.types.EvalVar expr)
    (let [v (t/getVal expr)]
      (if-not (vars/isMacro v)
        (deref v)
        (throw (new #?(:clj IllegalStateException :cljs js/Error)
                    (str "Can't take value of a macro: " v "")))))
    (let [m (meta expr)
          op (when m (.get ^java.util.Map m :sci.impl/op))
          ret
          (if
              (not op) expr
              ;; TODO: moving this up increased performance for #246. We can
              ;; probably optimize it further by not using separate keywords for
              ;; one :sci.impl/op keyword on which we can use a case expression
              (case op
                :call (eval-call ctx expr)
                :try (eval-try ctx expr)
                :fn (fns/eval-fn ctx interpret eval-do* expr)
                :static-access (interop/get-static-field expr)
                :var-value (nth expr 0)
                :deref! (let [v (first expr)
                              v (if (vars/var? v) @v v)
                              v (force v)]
                          v)
                :resolve-sym (resolve-symbol ctx expr)
                :needs-ctx (partial expr ctx)
                (cond (map? expr) (zipmap (map #(interpret ctx %) (keys expr))
                                          (map #(interpret ctx %) (vals expr)))
                      (or (vector? expr) (set? expr)) (into (empty expr)
                                                            (map #(interpret ctx %)
                                                                 expr))
                      :else (throw (new #?(:clj Exception :cljs js/Error)
                                        (str "unexpected: " expr ", type: " (type expr), ", meta:" (meta expr)))))))
          ret (if m (fix-meta ret m)
                  ret)]
      ;; for debugging:
      ;; (prn expr (meta expr) '-> ret)
      (if-let [n (.get ^java.util.Map ctx :realize-max)]
        (max-or-throw ret (assoc ctx
                                 :expression expr)
                      n)
        ret))))

(defn do? [expr]
  (and (list? expr)
       (= 'do (first expr))))

(defn eval-form [ctx form]
  (if (do? form) (loop [exprs (rest form)
                        ret nil]
                   (if (seq exprs)
                     (recur
                      (rest exprs)
                      (eval-form ctx (first exprs)))
                     ret))
      (let [analyzed (ana/analyze ctx form)
            ret (interpret ctx analyzed)]
        ret)))

(vreset! utils/eval-form-state eval-form)

(defn eval-string* [ctx s]
  (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
    (loop [ret nil]
      (let [expr (p/parse-next ctx reader)]
        (if (utils/kw-identical? :edamame.impl.parser/eof expr) ret
            (let [ret (eval-form ctx expr)]
              (recur ret)))))))

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s opts]
   (let [init-ctx (opts/init opts)
         ret (vars/with-bindings
               (when-not @vars/current-ns
                 {vars/current-ns opts/user-ns})
               (eval-string* init-ctx s))]
     ret)))

;;;; Scratch

(comment
  (eval-string "((fn f [x] (if (< x 3) (recur (inc x)) x)) 0)")
  )
