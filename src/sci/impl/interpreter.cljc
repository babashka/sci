(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand])
  (:require
   [clojure.string :as str]
   [clojure.tools.reader.reader-types :as r]
   [sci.impl.analyzer :as ana]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.max-or-throw :refer [max-or-throw]]
   [sci.impl.namespaces :as namespaces]
   [sci.impl.parser :as p]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     rethrow-with-location-of-node
                                     strip-core-ns]]
   [sci.impl.var]))

(declare interpret)
#?(:clj (set! *warn-on-reflection* true))

(def macros
  '#{do if when and or -> ->> as-> quote let fn def defn
     lazy-seq require try syntax-quote case . in-ns})

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
        varify? (:sci/var (meta var-name))
        init (if varify? (sci.impl.var.SciVar. (fn [] init) var-name (meta var-name))
                 init)]
    (swap! (:env ctx) (fn [env]
                        (let [current-ns (:current-ns env)]
                          (update-in env [:namespaces current-ns] assoc var-name init))))
    init))

(defn lookup [{:keys [:bindings :env] :as ctx} sym]
  (let [env @env]
    (or
     (find bindings sym)
     (when (some-> sym meta :sci.impl/var.declared)
       (let [current-ns (:current-ns env)
             current-ns-map (-> env :namespaces current-ns)]
         (when-let [[k v] (find current-ns-map sym)]
           (if (some-> v meta :sci.impl/var.declared)
             [k nil]
             [k v]))))
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
             (let [namespaces (get env :namespaces)]
               (if-let [ns-data (get namespaces lib-name)]
                 (let [current-ns (:current-ns env)
                       the-current-ns (get-in env [:namespaces current-ns])
                       the-current-ns (if as (assoc-in the-current-ns [:aliases as] lib-name)
                                          the-current-ns)
                       the-current-ns
                       (if refer
                         (do
                           (when-not (sequential? refer)
                             (throw (new #?(:clj Exception :cljs js/Error)
                                         (str ":refer value must be a sequential collection of symbols"))))
                           (reduce (fn [ns sym]
                                     (assoc ns sym
                                            (if-let [[_k v] (find ns-data sym)]
                                              v
                                              (throw (new #?(:clj Exception :cljs js/Error)
                                                          (str sym " does not exist"))))))
                                   the-current-ns
                                   refer))
                         the-current-ns)
                       env (assoc-in env [:namespaces current-ns] the-current-ns)]
                   env)
                 (throw (new #?(:clj Exception :cljs js/Error)
                             (str "Could not require " lib-name ".")))))))))

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
    (:sci.impl/eval (meta form)) (interpret ctx form)
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

;;;; Namespaces

(defn eval-in-ns [ctx [_in-ns ns-expr]]
  (let [ns-sym (interpret ctx ns-expr)]
    (swap! (:env ctx) assoc :current-ns ns-sym)
    nil))

;;;; End namespaces

(declare eval-string)

(defn eval-do
  [ctx expr]
  (when-let [exprs (next expr)]
    (loop [[expr & exprs] exprs]
      (let [ret (try (interpret ctx expr)
                     (catch #?(:clj Throwable :cljs js/Error) e
                       (rethrow-with-location-of-node ctx e expr)))]
        (if-let [exprs (seq exprs)]
          (recur exprs)
          ret)))))

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
                 throw (eval-throw ctx expr)
                 in-ns (eval-in-ns ctx expr))))
       (catch #?(:clj Exception :cljs js/Error) e
         (rethrow-with-location-of-node ctx e expr))))

(defn interpret
  [ctx expr]
  (let [m (meta expr)
        ctx (assoc ctx :top-level? false)
        ctx (if m
              (let [{:keys [:row :col]} m]
                (if (and row col)
                  (assoc ctx :row row :col col)
                  ctx))
              ctx)
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
               (let [namespaces (merge-with merge {'user bindings} namespaces/namespaces (:namespaces env) namespaces)
                     aliases (merge namespaces/aliases (:aliases env) aliases)
                     namespaces (update namespaces 'user assoc :aliases aliases)]
                 (assoc env
                        :namespaces namespaces
                        :imports imports
                        :current-ns 'user)))))

(def presets
  {:termination-safe
   {:deny '[loop recur trampoline]
    :realize-max 100}})

(defn process-permissions [& permissions]
  (not-empty (into #{} (comp cat (map strip-core-ns)) permissions)))

(def default-classes
  #?(:clj {'java.lang.AssertionError AssertionError
           'java.lang.Exception {:class Exception}
           'clojure.lang.ExceptionInfo clojure.lang.ExceptionInfo
           'java.lang.String {:class String}
           'java.lang.Integer Integer
           'java.lang.Double Double
           'java.lang.ArithmeticException ArithmeticException}
     :cljs {'Error js/Error}))

(def default-imports
  #?(:clj '{AssertionError java.lang.AssertionError
            Exception java.lang.Exception
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
                         :imports
                         :features]}]
  (let [preset (get presets preset)
        env (or env (atom {}))
        imports (merge default-imports imports)
        bindings bindings
        _ (init-env! env bindings aliases namespaces imports)
        ctx (merge {:env env
                    :bindings {}
                    :allow (process-permissions (:allow preset) allow)
                    :deny (process-permissions (:deny preset) deny)
                    :realize-max (or realize-max (:realize-max preset))
                    :features features}
                   (normalize-classes (merge default-classes classes)))]
    ctx))

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

(defn eval-string* [ctx s]
  (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))
        features (:features ctx)
        env (:env ctx)]
    (loop [queue []
           ret nil]
      (let [env-val @env
            current-ns (:current-ns env-val)
            the-current-ns (get-in env-val [:namespaces current-ns])
            aliases (:aliases the-current-ns)
            expr (or (first queue)
                     (p/parse-next reader features
                                   (assoc aliases
                                          :current current-ns)))]
        (if (utils/kw-identical? :edamame.impl.parser/eof expr) ret
            (let [ret (eval-form ctx expr)]
              (if (seq queue) (recur (rest queue) ret)
                  (recur [] ret))))))))

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s opts]
   (let [init-ctx (opts->ctx opts)
         ret (eval-string* init-ctx s)]
     ret)))

;;;; Scratch

(comment
  (eval-string "((fn f [x] (if (< x 3) (recur (inc x)) x)) 0)")
  )
