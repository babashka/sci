(ns sci.impl.evaluator
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.string :as str]
   [sci.impl.deftype]
   [sci.impl.interop :as interop]
   [sci.impl.macros :as macros]
   [sci.impl.records]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer [#?(:cljs kw-identical?)
                                     rethrow-with-location-of-node
                                     throw-error-with-location]]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros [sci.impl.evaluator :refer [def-fn-call resolve-symbol]])))

(declare fn-call)

#?(:clj (set! *warn-on-reflection* true))

(def #?(:clj ^:const macros :cljs macros)
  '#{do fn def defn
     syntax-quote})

(defn eval-def
  [ctx bindings var-name init m]
  (let [init (types/eval init ctx bindings)
        m (types/eval m ctx bindings)
        cnn (types/getName (:ns m))
        assoc-in-env
        (fn [env]
          (let [the-current-ns (get (get env :namespaces) cnn)
                prev (get the-current-ns var-name)
                prev (if-not (utils/var? prev)
                       (sci.lang.Var. prev (symbol (str cnn) (str var-name))
                                      (meta prev)
                                      false
                                      false)
                       prev)
                v (if (identical? utils/var-unbound init)
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

(defmacro resolve-symbol [bindings sym]
  `(.get ~(with-meta bindings
            {:tag 'java.util.Map}) ~sym))

(declare eval-string*)

(defn eval-case
  ([ctx bindings case-map case-val]
   (let [v (types/eval case-val ctx bindings)
         found (get case-map v ::not-found)]
     (if (utils/kw-identical? ::not-found found)
       (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                   (str "No matching clause: " v)))
       (types/eval found ctx bindings))))
  ([ctx bindings case-map case-val case-default]
   (let [v (types/eval case-val ctx bindings)
         found (get case-map v ::not-found)]
     (if (utils/kw-identical? ::not-found found)
       (types/eval case-default ctx bindings)
       (types/eval found ctx bindings)))))

(defn eval-try
  [ctx bindings body catches finally sci-error]
  (try
    (binding [utils/*in-try* (or (and (not sci-error)
                                      ;; try/finally without catch
                                      (seq catches)
                                      )
                                 utils/*in-try*)]
      (types/eval body ctx bindings))
    (catch #?(:clj Throwable :cljs :default) e
      (if-let
          [[_ r]
           (reduce (fn [_ c]
                     (let [clazz (:class c)]
                       (when #?(:cljs
                                (or (kw-identical? :default clazz)
                                    (if (instance? sci.impl.types/NodeR clazz)
                                      (instance? (types/eval clazz ctx bindings) e)
                                      (instance? clazz e)))
                                :clj (instance? clazz e))
                         (reduced
                          [::try-result
                           (do (aset ^objects bindings (:ex-idx c) e)
                               (types/eval (:body c) ctx bindings))]))))
                   nil
                   catches)]
        r
        (rethrow-with-location-of-node ctx bindings e body)))
    (finally
      (types/eval finally ctx bindings))))

;;;; Interop

(defn eval-instance-method-invocation
  [ctx bindings instance-expr method-str field-access args #?(:cljs allowed)]
  (let [instance-meta (meta instance-expr)
        tag-class (:tag-class instance-meta)
        instance-expr* (types/eval instance-expr ctx bindings)]
    ;; (prn (type instance-expr*))
    (if (instance? sci.impl.records.SciRecord instance-expr*)
      (get instance-expr* (keyword
                           ;; TODO: strip leading dash in analyzer
                           method-str))
      (if (instance? sci.impl.deftype.SciType instance-expr*)
        (get (types/getVal instance-expr*)
             (symbol
              ;; TODO: strip leading dash in analyzer
              method-str))
        (let [instance-class (or tag-class (#?(:clj class :cljs type) instance-expr*))
              env @(:env ctx)
              class->opts (:class->opts env)
              allowed? (or
                        #?(:cljs allowed)
                        (get class->opts :allow)
                        (let [instance-class-name #?(:clj (.getName ^Class instance-class)
                                                     :cljs (.-name instance-class))
                              instance-class-symbol (symbol instance-class-name)]
                          (get class->opts instance-class-symbol)))
              ^Class target-class (if allowed? instance-class
                                      (when-let [f (:public-class env)]
                                        (f instance-expr*)))]
          ;; we have to check options at run time, since we don't know what the class
          ;; of instance-expr is at analysis time
          (when-not #?(:clj target-class
                       :cljs allowed?)
            (throw-error-with-location (str "Method " method-str " on " instance-class " not allowed!") instance-expr))
          (if field-access
            (interop/invoke-instance-field instance-expr* target-class method-str)
            (let [args (map #(types/eval % ctx bindings) args)] ;; eval args!
              (interop/invoke-instance-method instance-expr* target-class method-str args))))))))

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
           res (second (@utils/lookup ctx sym false))]
       (when-not #?(:cljs (instance? sci.impl.types/NodeR res)
                    :clj (instance? sci.impl.types.Eval res))
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
                              (if-let [rec-var
                                       (let [rec-ns (symbol (utils/demunge (str package)))
                                             rec-var (get-in @env [:namespaces rec-ns class])]
                                         rec-var)]
                                (let [cnn (utils/current-ns-name)]
                                  (swap! env assoc-in [:namespaces cnn :refers class] rec-var)
                                  @rec-var)
                                (throw (new #?(:clj Exception :cljs js/Error)
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
#?(:clj (extend-protocol types/Eval
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
