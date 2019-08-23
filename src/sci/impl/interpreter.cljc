(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand])
  (:require
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   [clojure.string :as str]
   [sci.impl.fns :as fns]
   [sci.impl.functions :as f]
   [sci.impl.macros :as macros]
   [sci.impl.max-or-throw :refer [max-or-throw]]
   [clojure.walk :refer [postwalk]]))

;;;; Readers

(declare interpret)

(defn read-fn [ctx expr]
  (macros/expand-fn-literal ctx expr))

(defn read-regex [form]
  (re-pattern form))

(defn read-quote [form]
  (list 'quote form))

(defn read-edn [ctx s]
  (edn/read-string
   {:readers {'sci/fn (fn [s] (read-fn ctx s))
              'sci/regex read-regex
              'sci/quote read-quote}}
   s))

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

(defn eval-do
  [ctx expr]
  (loop [exprs (rest expr)]
    (when-let [e (first exprs)]
      (let [e (macros/macroexpand ctx e)
            e (interpret ctx e)]
        (if-let [n (next exprs)]
          (recur n)
          e)))))

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

(defn lookup [{:keys [:env :bindings]} sym]
  (when-let [[k v :as kv]
             (or (find bindings sym)
                 (find @env sym)
                 (find f/functions sym)
                 (when-let [ns (namespace sym)]
                   ;; (prn "NS>" ns)
                   (when (or (= "clojure.core" ns)
                             (= "cljs.core" ns))
                     (find f/functions (symbol (name sym))))))]
    (if-let [m (meta k)]
      (if (:sci/deref! m)
        ;; the evaluation of this expression has been delayed by
        ;; the caller and now is the time to deref it
        [k @v] kv)
      kv)))

(def macros '#{do if when and or -> ->> as-> quote let fn def defn})

(defn resolve-symbol [ctx expr]
  (second
   (or
    (lookup ctx expr)
    ;; TODO: check if symbol is in macros and then emit an error: cannot take
    ;; the value of a macro
    (let [n (name expr)]
      (if (str/starts-with? n "'")
        (let [v (symbol (subs n 1))]
          [v v])
        (throw (new #?(:clj Exception
                       :cljs js/Error)
                    (str "Could not resolve symbol: " (str expr)))))))))

(defn apply-fn [ctx f args]
  (let [args (map #(interpret ctx %) args)]
    ;; (prn "ARGS" args)
    (apply f args)))

(defn mark-evaled [x]
  (if #?(:clj (instance? clojure.lang.IObj x)
         :cljs (satisfies? IWithMeta x))
    (try (with-meta x {:sci/evaled true})
         (catch #?(:clj Exception :cljs js/Error) _ x))
    x))

(defn eval-call [ctx expr]
  (if-let [f (first expr)]
    (let [f (or (get macros f)
                (interpret ctx f))]
      (case f
        do
        (eval-do ctx expr)
        if
        (eval-if ctx expr)
        when
        (eval-when ctx expr)
        quote (do
                ;; (prn "QUOTE!" expr)
                (postwalk mark-evaled
                          (second expr)))
        and
        (eval-and ctx (rest expr))
        or
        (eval-or ctx (rest expr))
        let
        (apply eval-let ctx (rest expr))
        def (eval-def ctx expr)
        ;; else
        (if (ifn? f)
          (apply-fn ctx f (rest expr))
          (throw (new #?(:clj Exception :cljs js/Error)
                      (str "Cannot call " (pr-str f) " as a function."))))))
    expr))

(def constant? (some-fn fn? number? string? keyword?))

(defn interpret
  [ctx expr]
  ;; (prn "to eval expr" expr)
  (let [ret
        (cond
          (-> meta :sci/evaled) expr
          (constant? expr) expr
          (symbol? expr) (do #_(prn "sym" expr '-> (if (-> expr meta :sci.impl/unresolved)
                                                     (resolve-symbol ctx expr)
                                                     expr))
                             (if (-> expr meta :sci.impl/unresolved)
                               (resolve-symbol ctx expr)
                               expr))
          (:sci/fn expr) (fns/eval-fn ctx interpret expr)
          ;; we might eventually switch to rewrite-clj for parsing code,
          ;; then we can differentiate between was has been evaled and what
          ;; has not
          ;; (-> (meta expr) :sci/evaled) expr
          (map? expr) (zipmap (map #(interpret ctx %) (keys expr))
                              (map #(interpret ctx %) (vals expr)))
          (or (vector? expr) (set? expr)) (into (empty expr)
                                                (map #(interpret ctx %)
                                                     expr))
          (-> expr meta :sci.impl/eval-call) (eval-call ctx expr)
          :else expr)
        ret (mark-evaled ret)]
    ;; for debugging:
    ;; (prn expr) (prn '-> ret)
    (if-let [n (:realize-max ctx)]
      (max-or-throw ret (assoc ctx
                               :expression expr)
                    n)
      ret)))

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s {:keys [:bindings :env :allow :realize-max]}]
   (let [env (or env (atom {}))
         ctx {:env env
              :bindings bindings
              :allow (when allow (set allow))
              :realize-max realize-max
              :start-expression s}
         edn (read-edn ctx (-> s
                               ;; we might eventually switch to rewrite-clj for
                               ;; parsing code, for now this hack works
                               (str/replace "#(" "#sci/fn(")
                               (str/replace "#\"" "#sci/regex\"")
                               (str/replace #"'([{(#\[])"
                                            (fn [m]
                                              (str "#sci/quote " (second m))))))

         ;; _ (def e edn)
         expr (macros/macroexpand ctx edn)]
     ;; (prn "expanded:" expr)
     (interpret ctx expr))))

;;;; Scratch

(comment
  (interpret '(and *in* 3) 1)
  (interpret '(and *in* 3 false) 1)
  (interpret '(or *in* 3) nil)
  (ifn? 'foo)

  (eval-string "'foo")
  (str/replace "'foo" #"'(\S.*)" (fn [m]
                                   (str "#sci/quote "(second m))))
  (eval-string "((fn [] 1))")
  (eval-string "((fn [x] x) 1)")
  (eval-string "((fn [x] x) 1)")
  (eval-string "((fn [x y] [x y]) 1 2)")
  (def f (eval-string "#(< %1 10)"))
  (def f (eval-string "#(< x 10)")) ;; ERROR
  (f 1000)
  (eval-string "(quote (1 2 x))" {:bindings {:x 1}})
  (eval-string "((fn foo [x] (if (< x 3) (foo 1 (inc x)) x)) 0)")
  (eval-string "(apply (fn [x & xs] xs) 1 2 [3 4])")
  (eval-string "(map #(+ 1 %) [0 1 2])")
  (eval-string "list")
  (eval-string "(list (list \"foo\") (list \"bar\"))")
  (eval-string "(map (fn [x] x) (list (list \"foo\") (list \"bar\")))")
  (eval-string "(mapv (fn [x] x) (list (list \"foo\") (list \"bar\")))")
  )
