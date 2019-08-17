(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand])
  (:require
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   [clojure.string :as str]
   [sci.impl.fns :as fns]
   [sci.impl.functions :as f]
   [sci.impl.macros :as macros]))

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
  [i expr]
  (let [[_do & body] expr]
    (last (map i body))))

(defn eval-if
  [i expr]
  (let [[_if cond then else] expr]
    (if (i cond)
      (i then)
      (i else))))

(defn eval-when
  [i expr]
  (let [[_when cond & body] expr]
    (when (i cond )
      (last (map i body)))))

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
                 (find f/functions sym))]
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

(defn apply-fn [f i args]
  (let [args (mapv i args)]
    (apply f args)))

(def constant? (some-fn fn? number? string? keyword?))

(defn interpret
  [ctx expr]
  ;; (prn "EXPR" expr)
  (cond (constant? expr) expr
        (symbol? expr) (resolve-symbol ctx expr)
        (:sci/fn expr) (fns/eval-fn ctx interpret expr)
        :else
        (let [i #(interpret ctx %)
              r (cond
                  (map? expr)
                  (zipmap (map i (keys expr))
                          (map i (vals expr)))
                  (or (vector? expr) (set? expr))
                  (into (empty expr) (map i expr))
                  (seq? expr)
                  (if-let [f (first expr)]
                    (let [;;_ (prn "FST" f)
                          f (or (get macros f)
                                (i f))
                          ;;_ (prn ">" f)
                          ]
                      (case f
                        do
                        (eval-do i expr)
                        if
                        (eval-if i expr)
                        when
                        (eval-when i expr)
                        quote (second expr)
                        and
                        (eval-and ctx (rest expr))
                        or
                        (eval-or ctx (rest expr))
                        let
                        (apply eval-let ctx (rest expr))
                        def (eval-def ctx expr)
                        ;; else
                        (if (ifn? f)
                          (apply-fn f i (rest expr))
                          (throw #?(:clj (Exception. (format "Cannot call %s as a function." (pr-str f)))
                                    :cljs (js/Error. (str "Cannot call " (pr-str f) " as a function.")))))))
                    expr)
                  :else expr)]
          ;; for debugging:
          ;; (prn expr '-> r)
          r)))

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s {:keys [:bindings]}]
   (let [env (atom {})
         ctx {:env env
              :bindings bindings}
         edn (read-edn ctx (-> s
                               (str/replace "#(" "#sci/fn(")
                               (str/replace "#\"" "#sci/regex\"")
                               (str/replace #"'([{(#\[])"
                                            (fn [m]
                                              (str "#sci/quote " (second m))))))

         ;; _ (def e edn)
         expr (macros/macroexpand ctx edn)
         ]
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
  )

