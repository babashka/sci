(ns sci.impl.interpreter
  {:no-doc true}
  (:require
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [sci.impl.functions :as f]
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn]))
  #?(:cljs (:require-macros [sci.impl.interpreter :refer [one-of]])))

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

(defmacro one-of [x elements]
  `(let [x# ~x]
     (case x# (~@elements) x# nil)))

(defn lookup [sym bindings]
  (or (find bindings sym)
      (find f/functions sym)))

(defn resolve-symbol [expr bindings]
  (second
   (or
    (lookup expr bindings)
    (when-let [v (one-of expr [if when and or -> ->> quote])]
      [v v])
    (let [n (name expr)]
      (if (str/starts-with? n "'")
        (let [v (symbol (subs n 1))]
          [v v])
        #?(:clj (throw (Exception. (format "Could not resolve symbol: %s." n)))
           :cljs (throw (js/Error. (str "Could not resolve symbol: " n ".")))))))))

(defn apply-fn [f i args]
  (let [args (mapv i args)]
    (apply f args)))

(defn interpret
  [expr bindings]
  (let [i #(interpret % bindings)]
    (cond
      (symbol? expr) (resolve-symbol expr bindings)
      (map? expr)
      (zipmap (map i (keys expr))
              (map i (vals expr)))
      (or (vector? expr) (set? expr))
      (into (empty expr) (map i expr))
      (seq? expr)
      (if-let [f (first expr)]
        (let [f (interpret f bindings)]
          (case f
            (if when)
            (let [[_if cond then else] expr]
              (if (interpret cond bindings)
                (interpret then bindings)
                (interpret else bindings)))
            quote (second expr)
            ->
            (interpret (expand-> (rest expr)) bindings)
            ->>
            (interpret (expand->> (rest expr)) bindings)
            and
            (eval-and bindings (rest expr))
            or
            (eval-or bindings (rest expr))
            (if (ifn? f)
              (apply-fn f i (rest expr))
              (throw #?(:clj (Exception. (format "Cannot call %s as a function." (pr-str f)))
                        :cljs (js/Error. (str "Cannot call " (pr-str f) " as a function.")))))))
        expr)
      ;; read fn passed as higher order fn, still needs input
      (-> expr meta :sci/fn)
      (expr bindings)
      :else expr)))

;;;; Scratch

(comment
  (interpret '(and *in* 3) 1)
  (interpret '(and *in* 3 false) 1)
  (interpret '(or *in* 3) nil)
  (ifn? 'foo)

  (eval-string "'foo")
  (str/replace "'foo" #"'(\S.*)" (fn [m]
                                   (str "#sci/quote "(second m))))
  )
