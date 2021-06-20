(ns sci.impl.evaluator
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.string :as str]
   [sci.impl.faster :as faster :refer [get-2 deref-1]]
   [sci.impl.interop :as interop]
   [sci.impl.macros :as macros]
   [sci.impl.records :as records]
   [sci.impl.types]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     rethrow-with-location-of-node
                                     kw-identical?]]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros [sci.impl.evaluator :refer [def-fn-call resolve-symbol]])))

(declare eval fn-call)

#?(:clj (set! *warn-on-reflection* true))

(def #?(:clj ^:const macros :cljs macros)
  '#{do fn def defn
     syntax-quote})

;;;; Evaluation

(defn eval-and
  "The and macro from clojure.core. Note: and is unrolled in the analyzer, this is a fallback."
  [ctx args]
  (let [args (seq args)]
    (loop [args args]
      (if args
        (let [x (first args)
              v (eval ctx x)]
          (if v
            (let [xs (next args)]
              (if xs
                (recur xs) v)) v))
        true))))

(defn eval-or
  "The or macro from clojure.core. Note: or is unrolled in the analyzer, this is a fallback."
  [ctx args]
  (let [args (seq args)]
    (loop [args args]
      (when args
        (let [x (first args)
              v (eval ctx x)]
          (if v v
              (let [xs (next args)]
                (if xs (recur xs)
                    v))))))))

(defn eval-let
  "The let macro from clojure.core"
  [ctx let-bindings exprs]
  (let [ctx (loop [ctx ctx
                   let-bindings let-bindings]
              (let [let-name (first let-bindings)
                    let-bindings (rest let-bindings)
                    let-val (first let-bindings)
                    rest-let-bindings (next let-bindings)
                    v (eval ctx let-val)
                    bindings (faster/get-2 ctx :bindings)
                    bindings (faster/assoc-3 bindings let-name v)
                    ctx (faster/assoc-3 ctx :bindings bindings)]
                (if-not rest-let-bindings
                  ctx
                  (recur ctx
                         rest-let-bindings))))]
    (when exprs
      (loop [exprs exprs]
        (let [e (first exprs)
              ret (eval ctx e)
              nexprs (next exprs)]
          (if nexprs (recur nexprs)
              ret))))))

(defn handle-meta [ctx m]
  ;; Sometimes metadata needs eval. In this case the metadata has metadata.
  (-> (if-let [mm (meta m)]
        (if (when mm (get-2 mm :sci.impl/op))
          (eval ctx m)
          m)
        m)
      (dissoc :sci.impl/op)))

(defn eval-map
  [ctx expr]
  (if-let [m (meta expr)]
    (if (kw-identical? :eval (:sci.impl/op m))
      (with-meta (zipmap (map #(eval ctx %) (keys expr))
                         (map #(eval ctx %) (vals expr)))
        (handle-meta ctx m))
      expr)
    expr))

(defn eval-def
  [ctx var-name init m]
  (let [init (eval ctx init)
        m (or m (meta var-name))
        m (eval-map ctx m) ;; m is marked with eval op in analyzer only when necessary
        cnn (vars/getName (:ns m))
        assoc-in-env
        (fn [env]
          (let [the-current-ns (get (get env :namespaces) cnn)
                prev (get the-current-ns var-name)
                prev (if-not (vars/var? prev)
                       (vars/->SciVar prev (symbol (str cnn) (str var-name))
                                      (meta prev)
                                      false)
                       prev)
                v (if (kw-identical? :sci.impl/var.unbound init)
                    (doto prev
                      (alter-meta! merge m))
                    (do (vars/bindRoot prev init)
                        (alter-meta! prev merge m)
                        prev))
                the-current-ns (assoc the-current-ns var-name v)]
            (assoc-in env [:namespaces cnn] the-current-ns)))
        env (swap! (:env ctx) assoc-in-env)]
    ;; return var
    (get (get (get env :namespaces) cnn) var-name)))

(defmacro resolve-symbol [ctx sym]
  `(.get ^java.util.Map
         (.get ~(with-meta ctx
                  {:tag 'java.util.Map}) :bindings) ~sym))

(declare eval-string*)

(defn eval-case
  ([ctx case-map case-val]
   (let [v (eval ctx case-val)]
     (if-let [[_ found] (find case-map v)]
       (eval ctx found)
       (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                   (str "No matching clause: " v))))))
  ([ctx case-map case-val case-default]
   (let [v (eval ctx case-val)]
     (if-let [[_ found] (find case-map v)]
       (eval ctx found)
       (eval ctx case-default)))))

(defn eval-try
  [ctx body catches finally]
  (try
    (binding [utils/*in-try* true]
      (eval ctx body))
    (catch #?(:clj Throwable :cljs js/Error) e
      (if-let
          [[_ r]
           (reduce (fn [_ c]
                     (let [clazz (:class c)]
                       (when (instance? clazz e)
                         (reduced
                          [::try-result
                           (eval (assoc-in ctx [:bindings (:binding c)]
                                           e)
                                 (:body c))]))))
                   nil
                   catches)]
        r
        (rethrow-with-location-of-node ctx e body)))
    (finally
      (eval ctx finally))))

;;;; Interop

(defn eval-static-method-invocation [ctx expr]
  (interop/invoke-static-method (first expr)
                                ;; eval args!
                                (map #(eval ctx %) (rest expr))))

#?(:clj
   (defn super-symbols [clazz]
     ;; (prn clazz '-> (map #(symbol (.getName ^Class %)) (supers clazz)))
     (map #(symbol (.getName ^Class %)) (supers clazz))))

(defn eval-instance-method-invocation
  [ctx instance-expr method-str args]
  (let [instance-meta (meta instance-expr)
        tag-class (:tag-class instance-meta)
        instance-expr* (eval ctx instance-expr)]
    (if (and (map? instance-expr*)
             (:sci.impl/record (meta instance-expr*))) ;; a sci record
      (get instance-expr* (keyword (subs method-str 1)))
      (let [instance-class (or tag-class (#?(:clj class :cljs type) instance-expr*))
            instance-class-name #?(:clj (.getName ^Class instance-class)
                                   :cljs (.-name instance-class))
            instance-class-symbol (symbol instance-class-name)
            class->opts (:class->opts ctx)
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
        (let [args (map #(eval ctx %) args)] ;; eval args!
          (interop/invoke-instance-method instance-expr* target-class method-str args))))))

;;;; End interop

;;;; Namespaces

(declare eval-form)

(defn eval-resolve
  ([ctx sym]
   (let [sym (eval ctx sym)]
     (second (@utils/lookup ctx sym false))))
  ([ctx env sym]
   (when-not (contains? env sym)
     (let [sym (eval ctx sym)]
       (second (@utils/lookup ctx sym false))))))

(vreset! utils/eval-resolve-state eval-resolve)

;;;; End namespaces

;;;; Import

(defn eval-import [ctx & import-symbols-or-lists]
  ;;(prn import-symbols-or-lists)
  (let [specs (map #(if (and (seq? %) (= 'quote (first %))) (second %) %)
                   import-symbols-or-lists)
        env (:env ctx)]
    (reduce (fn [_ spec]
              (let [[package classes]
                    (if (symbol? spec)
                      (let [s (str spec)
                            last-dot (str/last-index-of s ".")
                            package+class-name
                            (if last-dot
                              [(symbol (subs s 0 last-dot))
                               [(symbol (subs s (inc last-dot) (count s)))]]
                              [nil [spec]])]
                        package+class-name)
                      (let [p (first spec)
                            cs (rest spec)]
                        [p cs]))]
                (reduce (fn [_ class]
                          (let [fq-class-name (symbol (if package (str package "." class)
                                                          class))]
                            (if-let [clazz (interop/resolve-class ctx fq-class-name)]
                              (let [cnn (vars/current-ns-name)]
                                (swap! env assoc-in [:namespaces cnn :imports class] fq-class-name)
                                clazz)
                              (if-let [rec (records/resolve-record-or-protocol-class ctx package class)]
                                (let [cnn (vars/current-ns-name)]
                                  (swap! env assoc-in [:namespaces cnn class] rec)
                                  rec)
                                (throw (new #?(:clj Exception :cljs js/Error)
                                            (str "Unable to resolve classname: " fq-class-name)))))))
                        nil
                        classes)))
            nil
            specs)))

;;;; End import

(declare eval-string)

(defn eval-do
  "Note: various arities of do have already been unrolled in the analyzer."
  [ctx exprs]
  (let [exprs (seq exprs)]
    (loop [exprs exprs]
      (when exprs
        (let [ret (eval ctx (first exprs))]
          (if-let [exprs (next exprs)]
            (recur exprs)
            ret))))))

(vreset! utils/eval-do* eval-do)

(macros/deftime
  ;; This macro generates a function of the following form for 20 arities:
  #_(defn fn-call [ctx f args]
      (case (count args)
        0 (f)
        1 (let [arg (eval ctx (first args))]
            (f arg))
        2 (let [arg1 (eval ctx (first args))
                args (rest args)
                arg2 (eval ctx (first args))]
            (f arg1 arg2))
        ,,,
        (let [args (mapv #(eval ctx %) args)]
          (apply f args))))
  (defmacro def-fn-call []
    (let [cases
          (mapcat (fn [i]
                    [i (let [arg-syms (map (fn [_] (gensym "arg")) (range i))
                             args-sym 'args ;; (gensym "args")
                             let-syms (interleave arg-syms (repeat args-sym))
                             let-vals (interleave (repeat `(eval ~'ctx (first ~args-sym)))
                                                  (repeat `(rest ~args-sym)))
                             let-bindings (vec (interleave let-syms let-vals))]
                         `(let ~let-bindings
                            (~'f ~@arg-syms)))]) (range 20))
          cases (concat cases ['(let [args (mapv #(eval ctx %) args)]
                                  (apply f args))])]
      ;; Normal apply:
      #_`(defn ~'fn-call ~'[ctx f args]
           (apply ~'f (map #(eval ~'ctx %) ~'args)))
      `(defn ~'fn-call ~'[ctx f args]
         ;; TODO: can we prevent hitting this at all, by analyzing more efficiently?
         ;; (prn :count ~'f ~'(count args) ~'args)
         (case ~'(count args)
           ~@cases)))))

(def-fn-call)

(defn eval
  [ctx expr]
  (try
    (cond (instance? #?(:clj sci.impl.types.EvalFn
                        :cljs sci.impl.types/EvalFn) expr)
          (let [f (.-f ^sci.impl.types.EvalFn expr)]
            (f ctx))
          (instance? #?(:clj sci.impl.types.EvalVar
                        :cljs sci.impl.types/EvalVar) expr)
          (let [v (.-v ^sci.impl.types.EvalVar expr)]
            (deref-1 v))
          #?(:clj (instance? clojure.lang.IPersistentMap expr)
             :cljs (if (nil? expr) false
                       (satisfies? IMap expr)))
          (eval-map ctx expr)
          :else expr)
    (catch #?(:clj Throwable :cljs js/Error) e
      (rethrow-with-location-of-node ctx e expr))))

(vreset! utils/eval* eval)
