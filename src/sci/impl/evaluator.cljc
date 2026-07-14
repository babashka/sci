(ns sci.impl.evaluator
  {:no-doc true}
  (:require
   [clojure.string :as str]
   [sci.impl.deftype]
   [sci.impl.interop :as interop]
   [sci.impl.macros :as macros]
   [sci.impl.records :as records]
   [sci.impl.resolve :as resolve]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer [rethrow-with-location-of-node
                                     throw-error-with-location]]
   [sci.impl.vars :as vars]
   [sci.lang :as lang])
  #?(:cljs (:require-macros [sci.impl.evaluator :refer [def-fn-call resolve-symbol]])))

(declare fn-call)

#?(:cljd nil :clj (set! *warn-on-reflection* true))

(def #?(:cljd macros :clj ^:const macros :cljs macros)
  '#{do fn def defn
     syntax-quote})

(defn eval-def
  [ctx bindings var-name init m file]
  (let [init (types/eval init ctx bindings)
        m (types/eval m ctx bindings)
        m (assoc m :name var-name :file file)
        cnn (types/getName (:ns m))
        assoc-in-env
        (fn [env]
          (let [the-current-ns (get (get env :namespaces) cnn)
                prev (get the-current-ns var-name)
                prev (if-not (utils/var? prev)
                       (let [m (meta prev)]
                         (lang/->Var prev var-name m false false nil (:ns m)))
                       prev)
                v (do (when-not (identical? utils/var-unbound init)
                        (vars/bindRoot prev init))
                      (utils/reset-meta!* prev m)
                      prev)
                the-current-ns (assoc the-current-ns var-name v)]
            (assoc-in env [:namespaces cnn] the-current-ns)))
        env (swap! (:env ctx) assoc-in-env)]
    ;; return var
    (get (get (get env :namespaces) cnn) var-name)))

#?(:cljd
   (defmacro resolve-symbol [bindings sym]
     `(get ~bindings ~sym))
   :default
   (defmacro resolve-symbol [bindings sym]
     `(.get ~(with-meta bindings
               {:tag 'java.util.Map}) ~sym)))

(declare eval-string*)

(defn eval-case
  ([ctx bindings case-map case-val]
   (let [v (types/eval case-val ctx bindings)
         found (get case-map v ::not-found)]
     (if (utils/kw-identical? ::not-found found)
       (throw (new #?(:cljd ArgumentError :clj IllegalArgumentException :cljs js/Error)
                   (str "No matching clause: " v)))
       (types/eval found ctx bindings))))
  ([ctx bindings case-map case-val case-default]
   (let [v (types/eval case-val ctx bindings)
         found (get case-map v ::not-found)]
     (if (utils/kw-identical? ::not-found found)
       (types/eval case-default ctx bindings)
       (types/eval found ctx bindings)))))

(defn eval-catches
  "Handles a Throwable `e` thrown by a try body: matches it against the catch
  clauses (returning the handler result) or rethrows. Called only on the
  exception path, so it adds no cost to normal execution. An interrupt signal
  (sci.core/interrupt!) is never catchable here. Public: the jit's compiled
  try delegates its catch dispatch here."
  [ctx bindings body catches sci-error e]
  (if (utils/interrupt-ex? e)
    (throw e)
    (if-let
        [[_ r]
         (reduce (fn [_ c]
                   (let [clazz (:class c)
                         e (if (and sci-error
                                    (not (:sci-error c)))
                             (ex-cause e)
                             e)]
                     (when #?(:cljd
                              ;; no runtime instance? on Dart, see :instance? closures in opts
                              (or (utils/kw-identical? :default clazz)
                                  (let [c (if (types/eval-node? clazz)
                                            (types/eval clazz ctx bindings)
                                            clazz)]
                                    (cond (map? c) (when-let [p (:instance? c)] (p e))
                                          (utils/sci-type? c) (= c (types/type-impl e))
                                          :else (= c (.-runtimeType e)))))
                              :cljs
                              (or (utils/kw-identical? :default clazz)
                                  (if (types/eval-node? clazz)
                                    (instance? (types/eval clazz ctx bindings) e)
                                    (instance? clazz e)))
                              :clj (instance? clazz e))
                       (reduced
                        [::try-result
                         (do (aset #?(:cljd ^List bindings :default ^objects bindings) (:ex-idx c) e)
                             (types/eval (:body c) ctx bindings))]))))
                 nil
                 catches)]
      r
      (rethrow-with-location-of-node ctx bindings e body))))

(defn- eval-try-body
  "Evaluates the try body and applies the catch clauses. No `finally`."
  [ctx bindings body catches sci-error]
  (try
    (binding [utils/*in-try* (or (when sci-error
                                   :sci/error)
                                 ;; try/finally without catch
                                 (seq catches)
                                 utils/*in-try*)]
      (types/eval body ctx bindings))
    (catch #?(:cljd Object :clj Throwable :cljs :default) e
      (eval-catches ctx bindings body catches sci-error e))))

(defn- eval-try-plain
  "Plain host try/catch/finally. Used when no :interrupt-fn is configured: no
  interrupt can be raised, so there is nothing a throwing finally could mask.
  `finally` is nil when the try form has no finally clause; the when-not guard
  skips evaluating it (the common case). Users not using the (experimental)
  interrupt-fn feature pay no overhead."
  [ctx bindings body catches finally sci-error]
  (try
    (binding [utils/*in-try* (or (when sci-error
                                   :sci/error)
                                 (seq catches)
                                 utils/*in-try*)]
      (types/eval body ctx bindings))
    (catch #?(:cljd Object :clj Throwable :cljs :default) e
      (eval-catches ctx bindings body catches sci-error e))
    (finally
      (when-not (nil? finally)
        (types/eval finally ctx bindings)))))

(defn eval-try
  [ctx bindings body catches finally sci-error]
  (if (nil? (:interrupt-fn ctx))
    (eval-try-plain ctx bindings body catches finally sci-error)
    ;; :interrupt-fn is configured. A `finally` that throws would normally mask
    ;; the in-flight exception (host try/finally semantics). An interrupt signal
    ;; must never be masked this way: otherwise sandboxed code could throw from
    ;; `finally` to discard the interrupt and let an outer `catch` swallow it
    ;; (see #1044). So we run `finally` ourselves and, when an interrupt is
    ;; pending, let it win over a non-interrupt exception from the finally body.
    (if (nil? finally)
      (eval-try-body ctx bindings body catches sci-error)
      (let [pending (volatile! nil)
            had-ex  (volatile! false)
            v (try (eval-try-body ctx bindings body catches sci-error)
                   (catch #?(:cljd Object :clj Throwable :cljs :default) e
                     (vreset! pending e)
                     (vreset! had-ex true)
                     nil))]
        (try
          (types/eval finally ctx bindings)
          (catch #?(:cljd Object :clj Throwable :cljs :default) fe
            (if (and @had-ex
                     (utils/interrupt-ex? @pending)
                     (not (utils/interrupt-ex? fe)))
              ;; swallow the finally's exception; the interrupt rethrown below wins
              nil
              (throw fe))))
        (if @had-ex (throw @pending) v)))))

;;;; Interop

#?(:cljs
   (defn allowed-instance-method-invocation [ctx bindings instance-expr method-str args arg-count]
     (let [instance-expr* (types/eval instance-expr ctx bindings)]
       (interop/invoke-instance-method ctx bindings instance-expr* nil method-str args arg-count nil))))

#?(:cljs
   (defn allowed-instance-field-invocation [ctx bindings instance-expr method-str]
     (let [instance-expr* (types/eval instance-expr ctx bindings)]
       (interop/invoke-instance-field instance-expr* nil method-str))))

(def none-sentinel #?(:cljd ^:unique (Object.) :clj (Object.) :cljs (js/Object.)))

(defn get-from-type [instance _method-str method-str-unmunged #?(:cljd arg-count :clj arg-count :cljs args)]
  (if (zero? #?(:cljd arg-count :clj arg-count :cljs (alength args)))
    (if (instance? #?(:cljd records/SciRecord :clj sci.impl.records.SciRecord :cljs sci.impl.records.SciRecord) instance)
      (get instance (keyword method-str-unmunged) none-sentinel)
      (if #?(:cljd (satisfies? types/ICustomType instance)
             :clj (instance? sci.impl.types.ICustomType instance)
             :cljs (implements? sci.impl.types.ICustomType instance))
        (get (types/getFields instance) (symbol method-str-unmunged) none-sentinel)
        none-sentinel))
    none-sentinel))

(defn eval-instance-method-invocation
  ;; one interop node for all sites. cache is a per-site volatile: a monomorphic
  ;; site skips config resolution and meth-cache, reflecting directly on the
  ;; cached method list. override/deny/:closed is resolved only on a cache miss,
  ;; so a site using no member config pays nothing for the feature on the hot path.
  [ctx bindings instance-expr method-str method-str-unmunged method-sym field-access args #?(:cljs allowed) arg-count arg-types cache]
  (let [#?@(:clj [instance-meta (meta instance-expr)
                  tag-class (:tag-class instance-meta)])
        instance-expr* (types/eval instance-expr ctx bindings)
        v (get-from-type instance-expr* method-str method-str-unmunged #?(:cljd arg-count :clj arg-count :cljs args))]
    (if-not (identical? none-sentinel v)
      v
      (let [instance-class #?(:cljd (.-runtimeType instance-expr*)
                              :clj (or (when tag-class
                                          (if (instance? tag-class instance-expr*)
                                            tag-class
                                            (class instance-expr*)))
                                        (class instance-expr*))
                              :cljs (type instance-expr*))
            env @(:env ctx)
            class->opts (:class->opts env)
            #?@(:cljd [] :default [cached @cache])]
        #?(:cljd
           ;; cljd has no reflection: interop dispatches through override fns
           ;; keyed on the :class Type object (runtimeType), every member is
           ;; effectively closed
           (let [type->opts (:type->opts env)
                 class-opts (or (get type->opts instance-class)
                                (when-let [f (:public-class env)]
                                  (get type->opts (f instance-expr*))))
                 section (if field-access :instance-fields :instance-methods)
                 f (get (get class-opts section) method-sym)]
             (case (interop/member-disposition f class-opts section)
               :override (if field-access
                           (f instance-expr*)
                           (apply f instance-expr* (map #(types/eval % ctx bindings) args)))
               (throw-error-with-location
                (str (if field-access "Field " "Method ") method-str " on " instance-class " not allowed") instance-expr)))
           :default
           ;; cache keyed on class->opts identity too: merge-opts swaps in a fresh
           ;; map, so a config change invalidates the cache for free
           (if (and cached
                      (identical? class->opts (aget #?(:clj ^objects cached :cljs cached) 0))
                      (identical? instance-class (aget #?(:clj ^objects cached :cljs cached) 1)))
               ;; fast path: this class resolved to plain interop before; reflect on
               ;; the cached target class (may differ from instance-class via :public-class)
               (if field-access
                 (interop/invoke-instance-field instance-expr* (aget #?(:clj ^objects cached :cljs cached) 2) method-str)
                 #?(:clj (interop/invoke-instance-method-with-methods ctx bindings instance-expr* (aget ^objects cached 2) method-str (aget ^objects cached 3) args arg-count arg-types)
                    :cljs (interop/invoke-instance-method ctx bindings instance-expr* (aget cached 2) method-str args arg-count arg-types)))
               (let [allowed? (or
                               #?(:cljs allowed)
                               (let [instance-class-name #?(:clj (.getName ^Class instance-class)
                                                            :cljs (.-name instance-class))]
                                 (or (get class->opts (symbol instance-class-name))
                                     (get class->opts :allow))))
                     ^Class target-class (if allowed? instance-class
                                             (when-let [f (:public-class env)]
                                               (f instance-expr*)))]
                 (when-not #?(:clj target-class
                              :cljs allowed?)
                   (throw-error-with-location (str "Method " method-str " on " instance-class " not allowed!") instance-expr))
                 ;; resolve config independently of :allow so it wins over :allow :all
                 (let [class-opts (or (when (map? allowed?) allowed?)
                                      (get class->opts
                                           (symbol #?(:clj (.getName ^Class instance-class)
                                                      :cljs (.-name instance-class))))
                                      (when target-class
                                        (get class->opts
                                             (symbol #?(:clj (.getName ^Class target-class)
                                                        :cljs (.-name target-class))))))
                       ;; safe to cache a :public-class result keyed on instance-class
                       ;; unless the instance is a custom type: those share one class
                       ;; but map to different targets per instance
                       cache? (or (identical? target-class instance-class)
                                  (not #?(:clj (instance? sci.impl.types.ICustomType instance-expr*)
                                          :cljs (implements? sci.impl.types.ICustomType instance-expr*))))]
                   (if field-access
                     (let [f (get (:instance-fields class-opts) method-sym)]
                       (case (interop/member-disposition f class-opts :instance-fields)
                         :override
                         (f instance-expr*)
                         :deny
                         (throw-error-with-location (str "Field " method-str " on " instance-class " not allowed!") instance-expr)
                         :reflect
                         (do (when cache?
                               (vreset! cache (object-array #?(:clj [class->opts instance-class target-class nil]
                                                               :cljs [class->opts instance-class target-class]))))
                             (interop/invoke-instance-field instance-expr* target-class method-str))))
                     (let [f (get (:instance-methods class-opts) method-sym)]
                       (case (interop/member-disposition f class-opts :instance-methods)
                         :override
                         (apply f instance-expr* (map #(types/eval % ctx bindings) args))
                         :deny
                         (throw-error-with-location (str "Method " method-str " on " instance-class " not allowed!") instance-expr)
                         :reflect
                         #?(:clj (let [methods (interop/instance-method-list ctx target-class method-str arg-count)]
                                   (when cache?
                                     (vreset! cache (object-array [class->opts instance-class target-class methods])))
                                   (interop/invoke-instance-method-with-methods ctx bindings instance-expr* target-class method-str methods args arg-count arg-types))
                            :cljs (do (when cache?
                                        (vreset! cache (object-array [class->opts instance-class target-class])))
                                      (interop/invoke-instance-method ctx bindings instance-expr* target-class method-str args arg-count arg-types))))))))))))))

;;;; End interop

;;;; Namespaces

(declare eval-form)

(defn eval-resolve
  ([ctx bindings sym]
   (eval-resolve ctx bindings nil sym))
  ([ctx bindings env sym]
   (when (or (not env)
             (not (contains? env sym)))
     (let [sym (types/eval sym ctx bindings)
           res (second
                (resolve/lookup ctx sym false nil (qualified-symbol? sym)))]
       (when-not (sci.impl.types/eval-node? res)
         res)))))

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
                              (let [cnn (utils/current-ns-name)]
                                (swap! env assoc-in [:namespaces cnn :imports class] fq-class-name)
                                clazz)
                              (if-let [type-val
                                       (let [rec-ns (symbol (utils/demunge (str package)))
                                             the-ns (get-in @env [:namespaces rec-ns])
                                             v (or (get (:types the-ns) class)
                                                   (get the-ns class))]
                                         (if (utils/var? v) @v v))]
                                (let [cnn (utils/current-ns-name)]
                                  (swap! env assoc-in [:namespaces cnn :types class] type-val)
                                  type-val)
                                (throw (new #?(:cljd Exception :clj Exception :cljs js/Error)
                                            (str "Unable to resolve classname: " fq-class-name)))))))
                        nil
                        classes)))
            nil
            specs)))

;;;; End import

(declare eval-string)

(macros/deftime
  ;; This macro generates a function of the following form for 20 arities:
  #_(defn fn-call [ctx bindings f args]
      (case (count args)
        0 (f)
        1 (let [arg (eval ctx bindings (first args))]
            (f arg))
        2 (let [arg1 (eval ctx bindings (first args))
                args (rest args)
                arg2 (eval ctx bindings (first args))]
            (f arg1 arg2))
        ,,,
        (let [args (mapv #(eval ctx bindings %) args)]
          (apply f args))))
  (defmacro def-fn-call []
    (let [cases
          (mapcat (fn [i]
                    [i (let [arg-syms (map (fn [_] (gensym "arg")) (range i))
                             args-sym 'args ;; (gensym "args")
                             let-syms (interleave arg-syms (repeat args-sym))
                             let-vals (interleave (repeat `(types/eval (first ~args-sym) ~'ctx ~'bindings))
                                                  (repeat `(rest ~args-sym)))
                             let-bindings (vec (interleave let-syms let-vals))]
                         `(let ~let-bindings
                            (~'f ~@arg-syms)))]) (range 20))
          cases (concat cases ['(let [args (mapv #(types/eval % ctx bindings) args)]
                                  (apply f args))])]
      ;; Normal apply:
      #_`(defn ~'fn-call ~'[ctx f args]
           (apply ~'f (map #(eval ~'ctx %) ~'args)))
      `(defn ~'fn-call ~'[ctx bindings f args]
         ;; TODO: can we prevent hitting this at all, by analyzing more efficiently?
         ;; (prn :count ~'f ~'(count args) ~'args)
         (case ~'(count args)
           ~@cases)))))

(def-fn-call)

;; The following types cannot be treated as constants in the analyzer
#?(:cljd nil
   :clj (extend-protocol types/Eval
          java.lang.Class
          (eval [expr _ _]
            expr)
          clojure.lang.PersistentArrayMap
          (eval [expr _ _]
            expr)
          clojure.lang.PersistentVector
          (eval [expr _ _]
            expr)
          clojure.lang.Symbol
          (eval [expr _ _]
            expr)
          sci.lang.Namespace
          (eval [expr _ _]
            expr)
          sci.lang.Var
          (eval [expr _ _]
            expr)
          clojure.lang.MultiFn
          (eval [expr _ _]
            expr)
          Object
          (eval [expr _ _]
            expr)
          ;; literal nils are treated like constants, but nil might also happen
          ;; as a result of analysis
          nil (eval [_ _ _] nil)))
