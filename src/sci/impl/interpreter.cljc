(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand])
  (:require
   [clojure.string :as str]
   [sci.impl.analyzer :as ana]
   [sci.impl.exceptions :refer [exception-bindings]]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.max-or-throw :refer [max-or-throw]]
   [sci.impl.namespaces :as namespaces]
   [sci.impl.parser :as p]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     rethrow-with-location-of-node
                                     strip-core-ns]]))

(declare interpret)
#?(:clj (set! *warn-on-reflection* true))

(def macros
  '#{do if when and or -> ->> as-> quote let fn def defn
     lazy-seq require try syntax-quote case .})

;;;; Evaluation

(defn eval-and
  "The and macro from clojure.core."
  [ctx args]
  (if (empty? args) true
      (let [[x & xs] args
            v (interpret ctx x)]
        (if v
          (if (empty? xs) v
              (eval-and ctx xs))
          v))))

(defn eval-or
  "The or macro from clojure.core."
  [ctx args]
  (if (empty? args) nil
      (let [[x & xs] args
            v (interpret ctx x)]
        (if v v
            (if (empty? xs) v
                (eval-or ctx xs))))))

(defn eval-let
  "The let macro from clojure.core"
  [ctx let-bindings & exprs]
  (let [ctx (loop [ctx ctx
                   [let-name let-val & rest-let-bindings] let-bindings]
              (let [v (interpret ctx let-val)
                    ctx (assoc-in ctx [:bindings let-name] v)]
                (if (empty? rest-let-bindings)
                  ctx
                  (recur ctx
                         rest-let-bindings))))]
    (last (map #(interpret ctx %) exprs))))

(defn eval-if
  [ctx expr]
  (let [[_if cond then else] expr]
    (if (interpret ctx cond)
      (interpret ctx then)
      (interpret ctx else))))

(defn eval-when
  [ctx expr]
  (let [[_when cond & body] expr]
    (when (interpret ctx cond)
      (last (map #(interpret ctx %) body)))))

(defn eval-def
  [ctx [_def var-name ?docstring ?init]]
  (let [docstring (when ?init ?docstring)
        init (if docstring ?init ?docstring)
        init (interpret ctx init)
        m (if docstring {:sci/doc docstring} {})
        var-name (with-meta var-name m)]
    (swap! (:env ctx) assoc var-name init)
    init))

(defn lookup [{:keys [:bindings :env] :as ctx} sym]
  (let [env @env]
    (or
     (find bindings sym)
     (when (some-> sym meta :sci.impl/var.declared)
       (find env sym))
     (when-let [c (interop/resolve-class ctx sym)]
       [sym c]) ;; TODO, don't we resolve classes in the analyzer??
     (when (get macros sym)
       [sym sym]))))

(defn resolve-symbol [ctx expr]
  (second
   (or
    (lookup ctx expr)
    ;; TODO: check if symbol is in macros and then emit an error: cannot take
    ;; the value of a macro
    (throw-error-with-location
     (str "Could not resolve symbol: " (str expr) "\nks:" (keys (:bindings ctx)))
     expr))))

(defn parse-libspec-opts [opts]
  (loop [opts-map {}
         [opt-name fst-opt & rst-opts] opts]
    (if-not opt-name opts-map
            (case opt-name
              :as (recur (assoc opts-map :as fst-opt)
                         rst-opts)
              (:reload :reload-all :verbose) (recur opts-map (cons fst-opt rst-opts))
              :refer (recur (assoc opts-map :refer fst-opt)
                            rst-opts)))))

(defn handle-require-libspec
  [ctx [lib-name & opts]]
  (let [{:keys [:as :refer]} (parse-libspec-opts opts)]
    (swap! (:env ctx)
           (fn [env]
             (if-let [ns-data (get (:namespaces env) lib-name)]
               (let [env (if as (assoc-in env [:aliases as] lib-name)
                             env)
                     env (if refer
                           (do
                             (when-not (sequential? refer)
                               (throw (new #?(:clj Exception :cljs js/Error)
                                           (str ":refer value must be a sequential collection of symbols"))))
                             (reduce (fn [env sym]
                                       (assoc env sym
                                              (if-let [[_k v] (find ns-data sym)]
                                                v
                                                (throw (new #?(:clj Exception :cljs js/Error)
                                                            (str sym " does not exist"))))))
                                     env
                                     refer))
                           env)]
                 env)
               (throw (new #?(:clj Exception :cljs js/Error)
                           (str "Could not require " lib-name "."))))))))

(defn eval-require
  [ctx expr]
  (let [args (rest expr)]
    (run! #(handle-require-libspec ctx %) args)))

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
      (interpret (assoc ctx :sci.impl/in-try true) body)
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

;;;; syntax-quote

(declare eval-syntax-quote walk-syntax-quote)

(defn unquote-splicing? [x]
  (and (seq? x) (= 'unquote-splicing (first x))))

(defn process-seq [ctx form]
  (let [ret (loop [ret []
                   xs form]
              (if (seq xs)
                (let [x (first xs)
                      uq? (some-> x meta :sci.impl/unquote-splicing)
                      x' (walk-syntax-quote ctx x)
                      ret (if uq?
                            (into ret x')
                            (conj ret x'))]
                  (recur ret (rest xs)))
                (seq ret)))]
    ret))

(defn process-symbol [{:keys [:gensyms] :as ctx} sym]
  (let [m (meta sym)]
    (if (:sci.impl/eval m)
      (interpret ctx sym)
      (if (namespace sym)
        sym
        (let [n (name sym)]
          (if (str/ends-with? n "#")
            (if-let [generated (get @gensyms sym)]
              (interpret ctx generated)
              (let [n (subs n 0 (dec (count n)))
                    generated (gensym (str n "__"))
                    generated (symbol (str (name generated) "__auto__"))]
                (swap! gensyms assoc sym generated)
                generated))
            sym))))))

(defn walk-syntax-quote
  [ctx form]
  (cond
    (list? form) (let [f (first form)]
                   (if (= 'syntax-quote f)
                     (eval-syntax-quote ctx form)
                     (apply list (process-seq ctx form))))
    (seq? form) (process-seq ctx form)
    (coll? form) (into (empty form) (process-seq ctx form))
    (symbol? form) (process-symbol ctx form)
    :else (interpret ctx form)))

(defn eval-syntax-quote
  [ctx expr]
  (let [gensyms (atom {})
        ctx (assoc ctx :gensyms gensyms)]
    (walk-syntax-quote ctx (second expr))))

;;;; End syntax-quote

;;;; Interop

(defn eval-static-method-invocation [ctx expr]
  (interop/invoke-static-method ctx
                                (cons (first expr)
                                      ;; eval args!
                                      (map #(interpret ctx %) (rest expr)))))

(defn eval-constructor-invocation [ctx [_new class args]]
  (let [args (map #(interpret ctx %) args)] ;; eval args!
    (interop/invoke-constructor ctx class args)))

(defn eval-instance-method-invocation [{:keys [:class->opts] :as ctx} [_dot instance-expr method-str args]]
  (let [instance-expr* (interpret ctx instance-expr)
        clazz (#?(:clj class :cljs type) instance-expr*)
        class-name (#?(:clj .getName :cljs str) clazz)
        class-symbol (symbol class-name)
        opts (get class->opts class-symbol)]
    ;; we have to check options at run time, since we don't know what the class
    ;; of instance-expr is at analysis time
    (when-not opts
      (throw-error-with-location (str "Method " method-str " on " clazz " not allowed!") instance-expr))
    (let [args (map #(interpret ctx %) args)] ;; eval args!
      (interop/invoke-instance-method ctx instance-expr* method-str args))))

;;;; End interop

(declare eval-string)

(defn eval-do
  [{:keys [:top-level?] :as ctx} expr]
  (let [analyzed? (not top-level?)]
    (loop [exprs (rest expr)]
      (when-let [expr (first exprs)]
        (let [expr (if analyzed? expr (ana/analyze ctx expr))
              ret (try (interpret ctx expr)
                       (catch #?(:clj Exception :cljs js/Error) e
                         (rethrow-with-location-of-node ctx e expr)))]
          (if-let [n (next exprs)]
            (recur n)
            ret))))))

(defn eval-call [ctx expr]
  (try (let [ctx* ctx
             ctx (assoc ctx :top-level? false)
             f (first expr)
             m (meta f)
             eval? (:sci.impl/eval m)]
         (cond (:sci.impl/static-access m)
               (eval-static-method-invocation ctx expr)
               (or eval? (not (symbol? f)))
               (let [f (interpret ctx f)]
                 (if (ifn? f) (apply f (map #(interpret ctx %) (rest expr)))
                     (throw (new #?(:clj Exception :cljs js/Error)
                                 (str "Cannot call " (pr-str f) " as a function.")))))
               :else ;; if f is a symbol that we should not interpret anymore, it must be one of these:
               (case f
                 do (eval-do (assoc ctx :top-level? (:top-level? ctx*)) expr)
                 if (eval-if ctx expr)
                 when (eval-when ctx expr)
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
                 recur (with-meta (map #(interpret ctx %) (rest expr))
                         {:sci.impl/recur true})
                 require (eval-require ctx expr)
                 case (eval-case ctx expr)
                 try (eval-try ctx expr)
                 syntax-quote (eval-syntax-quote ctx expr)
                 ;; interop
                 new (eval-constructor-invocation ctx expr)
                 . (eval-instance-method-invocation ctx expr)
                 throw (eval-throw ctx expr))))
       (catch #?(:clj Exception :cljs js/Error) e
         (rethrow-with-location-of-node ctx e expr))))

(defn interpret
  [ctx expr]
  (let [m (meta expr)
        eval? (:sci.impl/eval m)
        ret
        (cond
          (not eval?) (do nil expr)
          (:sci.impl/try expr) (eval-try ctx expr)
          (:sci.impl/fn expr) (fns/eval-fn ctx interpret expr)
          (:sci.impl/eval-call m) (eval-call ctx expr)
          (:sci.impl/static-access m) (interop/get-static-field ctx expr)
          (symbol? expr) (resolve-symbol ctx expr)
          (map? expr) (zipmap (map #(interpret ctx %) (keys expr))
                              (map #(interpret ctx %) (vals expr)))
          (or (vector? expr) (set? expr)) (into (empty expr)
                                                (map #(interpret ctx %)
                                                     expr))
          :else (throw (new #?(:clj Exception :cljs js/Error) (str "unexpected: " expr))))]
    ;; for debugging:
    ;; (prn expr (meta expr) '-> ret)
    (if-let [n (:realize-max ctx)]
      (max-or-throw ret (assoc ctx
                               :expression expr)
                    n)
      ret)))


;;;; Initialization

(defn init-env! [env bindings aliases namespaces imports]
  (swap! env (fn [env]
               (let [env-val (merge env bindings)
                     namespaces (merge-with merge namespaces/namespaces (:namespaces env) namespaces)
                     aliases (merge namespaces/aliases (:aliases env) aliases)]
                 (assoc env-val
                        :namespaces namespaces
                        :aliases aliases
                        :imports imports)))))

(def presets
  {:termination-safe
   {:deny '[loop recur trampoline]
    :realize-max 100}})

(defn process-permissions [& permissions]
  (not-empty (into #{} (comp cat (map strip-core-ns)) permissions)))

(def default-classes
  #?(:clj {'java.lang.Exception {:class Exception}
           'clojure.lang.ExceptionInfo clojure.lang.ExceptionInfo
           'java.lang.String {:class String}
           'java.lang.Integer Integer
           'java.lang.Double Double
           'java.lang.ArithmeticException ArithmeticException}
     :cljs []))

(def default-imports
  #?(:clj '{Exception java.lang.Exception
            String java.lang.String
            ArithmeticException java.lang.ArithmeticException
            Integer java.lang.Integer
            Double java.lang.Double}
     :cljs {}))

(defn normalize-classes [classes]
  (loop [sym->class (transient {})
         class->opts (transient {})
         kvs classes]
    (if-let [[sym class-opts] (first kvs)]
      (let [[class class-opts] (if (map? class-opts)
                                 [(:class class-opts) class-opts]
                                 [class-opts {}])]
        (recur (assoc! sym->class sym class)
               ;; storing the physical class as key didn't work well with
               ;; GraalVM
               (assoc! class->opts sym class-opts)
               (rest kvs)))
      {:sym->class (persistent! sym->class)
       :class->opts (persistent! class->opts)})))

(defn opts->ctx [{:keys [:bindings :env
                         :allow :deny
                         :realize-max
                         :preset ;; used by malli
                         :aliases
                         :namespaces
                         :classes
                         :imports]}]
  (let [preset (get presets preset)
        env (or env (atom {}))
        imports (merge default-imports imports)
        bindings (merge exception-bindings bindings)
        _ (init-env! env bindings aliases namespaces imports)
        ctx (merge {:env env
                    :bindings {}
                    :allow (process-permissions (:allow preset) allow)
                    :deny (process-permissions (:deny preset) deny)
                    :realize-max (or realize-max (:realize-max preset))}
                   (normalize-classes (merge default-classes classes)))]
    ctx))

(defn eval-edn-vals [ctx edn-vals]
  (eval-do (assoc ctx :top-level? true) (cons 'do edn-vals)))

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s opts]
   (let [init-ctx (opts->ctx opts)
         features (:features opts)
         edn-vals (p/parse-string-all s features)
         ret (eval-edn-vals init-ctx edn-vals)]
     ret)))

;;;; Scratch

(comment
  (eval-string "((fn f [x] (if (< x 3) (recur (inc x)) x)) 0)")
  )
