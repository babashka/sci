(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand])
  (:require
   [clojure.string :as str]
   [sci.impl.fns :as fns]
   [sci.impl.macros :as macros]
   [sci.impl.max-or-throw :refer [max-or-throw]]
   [sci.impl.namespaces :as namespaces]
   [sci.impl.parser :as p]
   [sci.impl.utils :as utils :refer [throw-error-with-location]]
   [clojure.set :as set]))

(declare interpret)
#?(:clj (set! *warn-on-reflection* true))

(def macros '#{do if when and or -> ->> as-> quote let fn def defn
               lazy-seq require})

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
   ;; (find @env sym)
   (find bindings sym)
   (find namespaces/clojure-core sym)
   (when-let [ns (namespace sym)]
     ;; (prn "NS>" ns)
     (when (or (= "clojure.core" ns)
               (= "cljs.core" ns))
       (find namespaces/clojure-core (symbol (name sym)))))))

(defn resolve-symbol [ctx expr]
  ;; (prn "LOOKUP" expr '-> (lookup ctx expr))
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

(defn do-recur!
  [f & args]
  (let [ret (apply f args)]
    (if-let [m (meta ret)]
      (if (:sci.impl/recur m)
        (recur f ret)
        ret)
      ret)))

(defn apply-fn [ctx f args]
  ;; (prn "apply fn" f)
  (let [args (map #(interpret ctx %) args)]
    ;; (prn "ARGS" args)
    (apply do-recur! f args)))

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

(declare eval-string)

(defn eval-call [ctx expr]
  (if (empty? expr) expr
      (let [f (first expr)
            f (or
               (when (= 'recur f) 'recur)
               (get macros f)
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
          ;; else
          (if (ifn? f) (apply-fn ctx f (rest expr))
              (throw (new #?(:clj Exception :cljs js/Error)
                          (str "Cannot call " (pr-str f) " as a function."))))))))

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
    ;; (prn expr '-> ret)
    (if-let [n (:realize-max ctx)]
      (max-or-throw ret (assoc ctx
                               :expression expr)
                    n)
      ret)))

;;;; Called from public API

(defn init-env! [env bindings aliases namespaces]
  (swap! env (fn [env]
               (let [env-val (merge env bindings)
                     namespaces (merge-with merge namespaces/namespaces (:namespaces env) namespaces)
                     aliases (merge namespaces/aliases (:aliases env) aliases)]
                 (assoc env-val
                        :namespaces namespaces
                        :aliases aliases)))))

(def presets
  {:termination-safe
   {:deny '[loop recur trampoline]
    :realize-max 100}})

(defn eval-string
  ([s] (eval-string s nil))
  ([s {:keys [:bindings :env
              :allow :deny
              :realize-max
              :preset ;; used by malli
              :aliases
              :namespaces]}]
   (let [preset (get presets preset)
         env (or env (atom {}))
         _ (init-env! env bindings aliases namespaces)
         ctx {:env env
              :bindings {}
              :allow (not-empty (reduce into #{} [(:allow preset) allow]))
              :deny (not-empty (reduce into #{} [(:deny preset) deny]))
              :realize-max (or realize-max (:realize-max preset))
              :start-expression s}
         edn-vals (p/parse-string-all s)]
     (eval-do ctx (cons 'do edn-vals)))))

;;;; Scratch

(comment
  )
