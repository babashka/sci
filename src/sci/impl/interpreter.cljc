(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure])
  (:require
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [sci.impl.functions :as f]
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.fn-macro :as fn-macro]
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])))

;;;; Readers

(declare interpret)

(defn read-fn [form]
  (with-meta
    (fn [bindings]
      (fn [& [x y z]]
        (interpret (postwalk (fn [elt]
                               (case elt
                                 % x
                                 %1 x
                                 %2 y
                                 %3 z
                                 elt)) form) bindings)))
    {:sci/fn true}))

(defn read-regex [form]
  (re-pattern form))

(defn read-quote [form]
  (list 'quote form))

(defn read-edn [s]
  (edn/read-string
   {:readers {'sci/fn read-fn
              'sci/regex read-regex
              'sci/quote read-quote}}
   s))

(defn eval-string
  ([s] (eval-string s nil))
  ([s {:keys [:bindings]}]
   (let [edn (read-edn (-> s
                           (str/replace "#(" "#sci/fn(")
                           (str/replace "#\"" "#sci/regex\"")
                           (str/replace #"'([{(#\[])"
                                        (fn [m]
                                          (str "#sci/quote " (second m))))))]
     (interpret edn bindings))))

;;;; Macros

(defn expand->
  "The -> macro from clojure.core."
  [[x & forms]]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta (concat (list (first form) x)
                                          (next form))
                         (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defn expand->>
  "The ->> macro from clojure.core."
  [[x & forms]]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta (concat (cons (first form) (next form))
                                          (list x))
                         (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defn eval-as->
  "The ->> macro from clojure.core."
  [bindings & [expr name & forms]]
  (let [v (interpret expr bindings)]
    (if (empty? forms)
      v
      (let [bindings (assoc bindings name v)]
        (apply eval-as-> bindings (first forms) name (rest forms))))))

(defn eval-and
  "The and macro from clojure.core."
  [in args]
  (if (empty? args) true
      (let [[x & xs] args
            v (interpret x in)]
        (if v
          (if (empty? xs) v
              (eval-and in xs))
          v))))

(defn eval-or
  "The or macro from clojure.core."
  [in args]
  (if (empty? args) nil
      (let [[x & xs] args
            v (interpret x in)]
        (if v v
            (if (empty? xs) v
                (eval-or in xs))))))

(defn eval-let
  "The let macro from clojure.core"
  [sci-bindings let-bindings & exprs]
  (let [let-bindings (destructure let-bindings)
        bindings (loop [bindings sci-bindings
                        [let-name let-val & rest-let-bindings] let-bindings]
                   (let [v (interpret let-val bindings)
                         bindings (assoc bindings let-name v)]
                     (if (empty? rest-let-bindings)
                       bindings
                       (recur bindings
                              rest-let-bindings))))]
    (interpret (last exprs) bindings)))

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

(defn lookup [sym bindings]
  (when-let [[k v :as kv]
             (or (find bindings sym)
                 (find f/functions sym))]
    (if-let [m (meta k)]
      (if (:sci/deref! m)
        ;; the evaluation of this expression has been delayed by
        ;; the caller and now is the time to deref it
        [k @v] kv)
      kv)))

(def macros '#{do if when and or -> ->> as-> quote let fn})

(defn resolve-symbol [expr bindings]
  (second
   (or
    (lookup expr bindings)
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

(defn interpret
  [expr bindings]
  (let [i #(interpret % bindings)
        r (cond
            (symbol? expr) (resolve-symbol expr bindings)
            (map? expr)
            (zipmap (map i (keys expr))
                    (map i (vals expr)))
            (or (vector? expr) (set? expr))
            (into (empty expr) (map i expr))
            (seq? expr)
            (if-let [f (first expr)]
              (let [f (or (get macros f)
                          (interpret f bindings))]
                (case f
                  do
                  (eval-do i expr)
                  if
                  (eval-if i expr)
                  when
                  (eval-when i expr)
                  quote (second expr)
                  ->
                  (interpret (expand-> (rest expr)) bindings)
                  ->>
                  (interpret (expand->> (rest expr)) bindings)
                  and
                  (eval-and bindings (rest expr))
                  or
                  (eval-or bindings (rest expr))
                  as->
                  (apply eval-as-> bindings (rest expr))
                  let
                  (apply eval-let bindings (rest expr))
                  fn (fn-macro/eval-fn interpret bindings expr)
                  ;; else
                  (if (ifn? f)
                    (apply-fn f i (rest expr))
                    (throw #?(:clj (Exception. (format "Cannot call %s as a function." (pr-str f)))
                              :cljs (js/Error. (str "Cannot call " (pr-str f) " as a function.")))))))
              expr)
            (-> expr meta :sci/fn)
            ;; read fn passed as higher order fn, still needs input
            (expr bindings)
            :else expr)]
    ;; for debugging:
    ;; (prn expr '-> r)
    r))

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
  )
