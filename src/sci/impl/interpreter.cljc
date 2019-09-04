(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand])
  (:require
   [clojure.string :as str]
   [sci.impl.fns :as fns]
   [sci.impl.functions :as f]
   [sci.impl.macros :as macros]
   [sci.impl.max-or-throw :refer [max-or-throw]]
   [sci.impl.parser :as p]
   [sci.impl.utils :as utils :refer [throw-error-with-location]]))

(declare interpret)
#?(:clj (set! *warn-on-reflection* true))

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
    (when-let [expr (first exprs)]
      (let [expr (macros/macroexpand ctx expr)
            ret (try (interpret ctx expr)
                     (catch #?(:clj Exception :cljs js/Error) e
                       (utils/re-throw-with-location-of-node e expr)))]
        (if-let [n (next exprs)]
          (recur n)
          ret)))))

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
        ;; _ (prn "init" (meta init))
        init (interpret ctx init)
        m (if docstring {:sci/doc docstring} {})
        var-name (with-meta var-name m)]
    (swap! (:env ctx) assoc var-name init)
    init))

(defn lookup [{:keys [:env :bindings]} sym]
  (or
   (find @env sym)
   (find bindings sym)
   (find f/functions sym)
   (when-let [ns (namespace sym)]
     ;; (prn "NS>" ns)
     (when (or (= "clojure.core" ns)
               (= "cljs.core" ns))
       (find f/functions (symbol (name sym)))))))

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
        ;; TODO: can this ever happen now that we resolve symbols at macro-expansion time?
        (throw-error-with-location
         (str "Could not resolve symbol: " (str expr))
         expr))))))

(defn apply-fn [ctx f args]
  ;; (prn "apply fn" f)
  (let [args (map #(interpret ctx %) args)]
    ;; (prn "ARGS" args)
    (apply f args)))

(declare eval-string)

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
        and
        (eval-and ctx (rest expr))
        or
        (eval-or ctx (rest expr))
        let
        (apply eval-let ctx (rest expr))
        def (eval-def ctx expr)
        ;; else
        (if (ifn? f) (apply-fn ctx f (rest expr))
            (throw (new #?(:clj Exception :cljs js/Error)
                        (str "Cannot call " (pr-str f) " as a function."))))))
    expr))

(defn interpret
  [ctx expr]
  ;; (prn "to eval expr" expr (meta expr))
  (let [m (meta expr)
        eval? (:sci.impl/eval m)
        ret
        (cond
          (not eval?) (do nil ;; (prn "not eval" expr)
                          expr)
          (:sci/fn expr) (fns/eval-fn ctx interpret expr)
          (:sci.impl/eval-call m) (eval-call ctx expr)
          (symbol? expr) (resolve-symbol ctx expr)
          (map? expr) (zipmap (map #(interpret ctx %) (keys expr))
                              (map #(interpret ctx %) (vals expr)))
          (or (vector? expr) (set? expr)) (into (empty expr)
                                                (map #(interpret ctx %)
                                                     expr))
          :else (throw (new #?(:clj Exception :cljs js/Error) (str "unexpected: " expr))))]
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
         env (do (swap! env merge bindings) env)
         ctx {:env env
              :bindings {}
              :allow (when allow (set allow))
              :realize-max realize-max
              :start-expression s}
         edn-vals (p/parse-string-all s)]
     (eval-do ctx (cons 'do edn-vals)))))

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
