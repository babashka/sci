(ns sci.impl.evaluator
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.string :as str]
   [sci.impl.faster :as faster :refer [get-2 deref-1]]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.macros :as macros]
   [sci.impl.records :as records]
   [sci.impl.types :as t]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     rethrow-with-location-of-node
                                     set-namespace!
                                     kw-identical?
                                     ]]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros [sci.impl.evaluator :refer [def-fn-call resolve-symbol]])))

(declare eval fn-call)

#?(:clj (set! *warn-on-reflection* true))

(def #?(:clj ^:const macros :cljs macros)
  '#{do and or quote fn def defn
     lazy-seq try syntax-quote case . in-ns set!
     ;; TODO: make normal function
     require})

;;;; Evaluation

(defn eval-and
  "The and macro from clojure.core."
  [ctx args]
  (let [args (seq args)]
    (loop [args args]
      (if args
        (let [x (first args)
              xs (next args)
              v (eval ctx x)]
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
              v (eval ctx x)]
          (if v v
              (if xs (recur xs)
                  v)))))))

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

(defn eval-if
  [ctx cond then else]
  (if (eval ctx cond)
    (eval ctx then)
    (eval ctx else)))

;; user> (time (dotimes [i 1000000] (let [expr '(1 2 3) cond (first expr) expr (rest expr) then (first expr) expr (rest expr) else (first expr)] [cond then else])))
;; "Elapsed time: 119.671576 msecs"
;; nil
;; user> (time (dotimes [i 1000000] (let [[cond then else] '(1 2 3)] [cond then else])))
;; "Elapsed time: 744.034037 msecs"
;; nil

(defn eval-def
  [ctx [_def var-name ?docstring ?init]]
  #_(prn "def" var-name (vars/getName (:ns (meta var-name))))
  (let [docstring (when ?init ?docstring)
        init (if docstring ?init ?docstring)
        init (eval ctx init)
        m (meta var-name)
        m (eval ctx m)
        cnn (vars/getName (:ns m))
        assoc-in-env
        (fn [env]
          (let [the-current-ns (get-in env [:namespaces cnn])
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
    ;; return var instead of init-val
    (get-in env [:namespaces cnn var-name])))

(defmacro resolve-symbol [ctx sym]
  `(.get ^java.util.Map
         (.get ~(with-meta ctx
                  {:tag 'java.util.Map}) :bindings) ~sym))

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
  [ctx env current-ns the-loaded-ns lib-name
   {:keys [:as :refer :rename :exclude :only :use] :as _parsed-libspec}]
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
                        use)
                    (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym nil)
                    (sequential? refer)
                    (reduce (fn [ns sym]
                              (if (include-sym? sym)
                                (assoc ns (rename-sym sym)
                                       (if-let [[_k v] (find the-loaded-ns sym)]
                                         v
                                         (when-not (:uberscript ctx)
                                           (throw (new #?(:clj Exception :cljs js/Error)
                                                       (str sym " does not exist"))))))
                                ns))
                            the-current-ns
                            refer)
                    :else (throw (new #?(:clj Exception :cljs js/Error)
                                      (str ":refer value must be a sequential collection of symbols"))))
              use (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym only)
              :else the-current-ns)
        env (assoc-in env [:namespaces current-ns] the-current-ns)]
    (when-let [on-loaded (some-> the-loaded-ns :obj meta :sci.impl/required-fn)]
      (on-loaded {}))
    env))

(defn handle-require-libspec
  [ctx lib opts]
  (let [{:keys [:reload]} opts
        env* (:env ctx)
        env @env* ;; NOTE: loading namespaces is not (yet) thread-safe
        cnn (vars/current-ns-name)
        namespaces (get env :namespaces)
        uberscript (:uberscript ctx)
        reload* (or reload uberscript)]
    (if-let [the-loaded-ns (when-not reload* (get namespaces lib))]
      (reset! env* (handle-require-libspec-env ctx env cnn the-loaded-ns lib opts))
      (if-let [load-fn (:load-fn env)]
        (if-let [{:keys [:file :source]} (load-fn {:namespace lib
                                                   :reload reload})]
          (do
            (try (vars/with-bindings
                   {vars/current-ns @vars/current-ns
                    vars/current-file file}
                   (@utils/eval-string* (assoc ctx :bindings {}) source))
                 (catch #?(:clj Exception :cljs js/Error) e
                   (swap! env* update :namespaces dissoc lib)
                   (throw e)))
            (swap! env* (fn [env]
                          (let [namespaces (get env :namespaces)
                                the-loaded-ns (get namespaces lib)]
                            (handle-require-libspec-env ctx env cnn
                                                        the-loaded-ns
                                                        lib opts)))))
          (or (when reload*
                (when-let [the-loaded-ns (get namespaces lib)]
                  (reset! env* (handle-require-libspec-env ctx env cnn the-loaded-ns lib opts))))
              (throw (new #?(:clj Exception :cljs js/Error)
                          (str "Could not find namespace: " lib ".")))))
        (throw (new #?(:clj Exception :cljs js/Error)

                    (str "Could not find namespace " lib ".")))))))

(defn load-lib [ctx prefix lib & options]
  (when (and prefix (pos? (.indexOf (name lib) #?(:clj (int \.)
                                                  :cljs \.))))
    (throw-error-with-location (str "Found lib name '" (name lib) "' containing period with prefix '"
                                    prefix "'.  lib names inside prefix lists must not contain periods")
                               lib))
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)]
    (handle-require-libspec ctx lib opts)))

(defn- prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn- load-libs
  "Loads libs, evaling libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [ctx kw args]
  (let [args* (cons kw args)
        flags (filter keyword? args*)
        opts (interleave flags (repeat true))
        args* (filter (complement keyword?) args*)]
    ;; check for unsupported options
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer}
          unsupported (seq (remove supported flags))]
      (when unsupported
        (throw-error-with-location (apply str "Unsupported option(s) supplied: "
                                          (interpose \, unsupported))
                                   ;; best effort location
                                   args)))
    ;; check a load target was specified
    (when-not (seq args*)
      (throw-error-with-location "Nothing specified to load"
                                 args))
    (doseq [arg args*]
      (if (libspec? arg)
        (apply load-lib ctx nil (prependss arg opts))
        (let [[prefix & args*] arg]
          (when (nil? prefix)
            (throw-error-with-location "prefix cannot be nil"
                                       args))
          (doseq [arg args*]
            (apply load-lib ctx prefix (prependss arg opts))))))))

(defn eval-require
  [ctx & args]
  (load-libs ctx :require args))

(vreset! utils/eval-require-state eval-require)

(defn eval-use
  [ctx & args]
  (load-libs ctx :use args))

(vreset! utils/eval-use-state eval-use)

(defn eval-case
  [ctx [_case {:keys [:case-map :case-val :case-default]}]]
  (let [v (eval ctx case-val)]
    (if-let [[_ found] (find case-map v)]
      (eval ctx found)
      (if (vector? case-default)
        (eval ctx (second case-default))
        (throw (new #?(:clj Exception :cljs js/Error)
                    (str "No matching clause: " v)))))))

(defn eval-try
  [ctx expr]
  (let [{:keys [:body :catches :finally]} (:sci.impl/try expr)]
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
        (eval ctx finally)))))

(defn eval-throw [ctx [_throw ex]]
  (let [ex (eval ctx ex)]
    (throw ex)))

;;;; Interop

(defn eval-static-method-invocation [ctx expr]
  (interop/invoke-static-method (first expr)
                                ;; eval args!
                                (map #(eval ctx %) (rest expr))))

(defn eval-constructor-invocation [ctx [_new #?(:clj class :cljs constructor) args]]
  (let [args (map #(eval ctx %) args)] ;; eval args!
    (interop/invoke-constructor #?(:clj class :cljs constructor) args)))

#?(:clj
   (defn super-symbols [clazz]
     ;; (prn clazz '-> (map #(symbol (.getName ^Class %)) (supers clazz)))
     (map #(symbol (.getName ^Class %)) (supers clazz))))

(defn eval-instance-method-invocation [{:keys [:class->opts] :as ctx}
                                       [_dot instance-expr method-str args :as _expr]]
  (let [instance-meta (meta instance-expr)
        tag-class (:tag-class instance-meta)
        instance-expr* (eval ctx instance-expr)]
    (if (map? instance-expr*) ;; a sci record
      (get instance-expr* (keyword (subs method-str 1)))
      (let [instance-class (or tag-class (#?(:clj class :cljs type) instance-expr*))
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
        (let [args (map #(eval ctx %) args)] ;; eval args!
          (interop/invoke-instance-method instance-expr* target-class method-str args))))))

;;;; End interop

;;;; Namespaces

(defn eval-in-ns [ctx [_in-ns ns-expr]]
  (let [ns-sym (eval ctx ns-expr)]
    (set-namespace! ctx ns-sym nil)
    nil))

(defn eval-refer [ctx ns-sym & exprs]
  (let [ns-sym (eval ctx ns-sym)]
    (loop [exprs exprs]
      (when exprs
        (let [[k v] exprs]
          (case k
            :exclude
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (vars/current-ns-name)]
                       (update-in env [:namespaces cnn :refer ns-sym :exclude]
                                  (fnil into #{}) v))))
            :only
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (vars/current-ns-name)
                           other-ns (get-in env [:namespaces ns-sym])
                           other-vars (select-keys other-ns v)]
                       (update-in env [:namespaces cnn]
                                  merge other-vars)))))
          (recur (nnext exprs)))))))

(vreset! utils/eval-refer-state eval-refer)

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

(defn eval-set! [ctx [_ obj v]]
  (let [obj (eval ctx obj)
        v (eval ctx v)]
    (if (vars/var? obj)
      (t/setVal obj v)
      (throw (ex-info (str "Cannot set " obj " to " v) {:obj obj :v v})))))

(declare eval-string)

(defn eval-do*
  [ctx exprs]
  (loop [[expr & exprs] exprs]
    (let [ret (eval ctx expr)]
      (if-let [exprs (seq exprs)]
        (recur exprs)
        ret))))

(vreset! utils/eval-do* eval-do*)

(defn eval-do
  [ctx expr]
  (when-let [exprs (next expr)]
    (eval-do* ctx exprs)))

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
         (case ~'(count args)
           ~@cases)))))

(def-fn-call)

(defn eval-special-call [ctx f-sym expr]
  (case (utils/strip-core-ns f-sym)
    do (eval-do ctx expr)
    and (eval-and ctx (rest expr))
    or (eval-or ctx (rest expr))
    def (eval-def ctx expr)
    lazy-seq (new #?(:clj clojure.lang.LazySeq
                     :cljs cljs.core/LazySeq)
                  #?@(:clj []
                      :cljs [nil])
                  (eval ctx (second expr))
                  #?@(:clj []
                      :cljs [nil nil]))
    recur (fn-call ctx (comp fns/->Recur vector) (rest expr))
    case (eval-case ctx expr)
    try (eval-try ctx expr)
    ;; interop
    new (eval-constructor-invocation ctx expr)
    . (eval-instance-method-invocation ctx expr)
    throw (eval-throw ctx expr)
    in-ns (eval-in-ns ctx expr)
    set! (eval-set! ctx expr)
    refer (apply eval-refer ctx (rest expr))
    require (apply eval-require ctx (with-meta (rest expr)
                                      (meta expr)))
    use (apply eval-use ctx (with-meta (rest expr)
                              (meta expr)))
    ;; resolve works as a function so this should not be necessary
    ;; resolve (eval-resolve ctx (second expr))
    ;;macroexpand-1 (macroexpand-1 ctx (eval ctx (second expr)))
    ;; macroexpand (macroexpand ctx (eval ctx (second expr)))
    import (apply eval-import ctx (rest expr))
    quote (second expr)))

(defn eval-call [ctx expr]
  (try (let [f (first expr)
             m (meta f)
             op (when m (get-2 m :sci.impl/op))]
         (cond
           (and (symbol? f) (not op))
           (eval-special-call ctx f expr)
           (kw-identical? op :static-access)
           (eval-static-method-invocation ctx expr)
           :else
           (let [f (if op (eval ctx f)
                       f)]
             (if (ifn? f)
               (fn-call ctx f (rest expr))
               (throw (new #?(:clj Exception :cljs js/Error)
                           (str "Cannot call " (pr-str f) " as a function.")))))))
       (catch #?(:clj Throwable :cljs js/Error) e
         (rethrow-with-location-of-node ctx e expr))))

(defn handle-meta [ctx m]
  ;; Sometimes metadata needs eval. In this case the metadata has metadata.
  (-> (if-let [mm (meta m)]
        (if (when mm (get-2 mm :sci.impl/op))
          (eval ctx m)
          m)
        m)
      (dissoc :sci.impl/op)))

(defn eval
  [ctx expr]
  (try
    (if (instance? sci.impl.types.EvalVar expr)
      (let [v (t/getVal expr)]
        (deref-1 v))
      (let [m (meta expr)
            op (when m (get-2 m :sci.impl/op))
            ret
            (if
                (not op) expr
                ;; TODO: moving this up increased performance for #246. We can
                ;; probably optimize it further by not using separate keywords for
                ;; one :sci.impl/op keyword on which we can use a case expression
                (case op
                  :call (eval-call ctx expr)
                  :try (eval-try ctx expr)
                  :fn (let [fn-meta (:sci.impl/fn-meta expr)
                            the-fn (fns/eval-fn ctx eval eval-do* expr)
                            fn-meta (when fn-meta (handle-meta ctx fn-meta))]
                        (if fn-meta
                          (vary-meta the-fn merge fn-meta)
                          the-fn))
                  :static-access (interop/get-static-field expr)
                  :deref! (let [v (first expr)
                                v (if (vars/var? v) @v v)
                                v (force v)]
                            v)
                  :resolve-sym (resolve-symbol ctx expr)
                  needs-ctx (if (identical? op utils/needs-ctx)
                              (partial expr ctx)
                              ;; this should never happen, or if it does, it's
                              ;; someone trying to hack
                              (throw (new #?(:clj Exception :cljs js/Error)
                                          (str "unexpected: " expr ", type: " (type expr), ", meta:" (meta expr)))))
                  eval (if (identical? op utils/evaluate)
                         (expr ctx)
                         (throw (new #?(:clj Exception :cljs js/Error)
                                     (str "unexpected: " expr ", type: " (type expr), ", meta:" (meta expr)))))
                  (cond (map? expr) (with-meta (zipmap (map #(eval ctx %) (keys expr))
                                                       (map #(eval ctx %) (vals expr)))
                                      (handle-meta ctx m))
                        (or (vector? expr) (set? expr))
                        (with-meta (into (empty expr)
                                         (map #(eval ctx %)
                                              expr))
                          (handle-meta ctx m))
                        :else (throw (new #?(:clj Exception :cljs js/Error)
                                          (str "unexpected: " expr ", type: " (type expr), ", meta:" (meta expr)))))))]
        ;; for debugging:
        ;; (prn :eval expr (meta expr) '-> ret (meta ret))
        ret))
    (catch #?(:clj Throwable :cljs js/Error) e
      (if (isa? (some-> e ex-data :type) :sci/error)
        (throw e)
        (rethrow-with-location-of-node ctx e expr)))))

(vreset! utils/eval* eval)
