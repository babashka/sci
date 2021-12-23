(ns sci.impl.utils
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :as str]
            [sci.impl.types :as t]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(derive :sci.error/realized-beyond-max :sci/error)
(derive :sci.error/parse :sci/error)

(defn constant? [x]
  (or (number? x) (string? x) (keyword? x) (boolean? x)))

(def kw-identical? #?(:clj identical? :cljs keyword-identical?))

(defn throw-error-with-location
  ([msg iobj] (throw-error-with-location msg iobj {}))
  ([msg iobj data]
   (let [{:keys [:line :column :file]
          :or {file @vars/current-file}} (meta iobj)]
     (throw (ex-info msg (merge {:type :sci/error
                                 :line line
                                 :column column
                                 :file file} data))))))

(def ^:dynamic *in-try* false)

(defn macro? [f]
  (when-let [m (meta f)]
    (or (:sci/macro m)
        (:macro m))))

(def needs-ctx (symbol "needs-ctx"))

#?(:cljs
   (def allowed-append "used for allowing interop in with-out-str"
     (symbol "append")))

#?(:clj
   (defn rewrite-ex-msg [ex-msg env fm]
     (if ex-msg
       (let [[_ printed-fn] (re-matches #"Wrong number of args \(\d+\) passed to: (.*)" ex-msg)
             fn-pat #"(sci\.impl\.)?fns/fun/arity-([0-9])+--\d+"
             [match _prefix arity] (re-find fn-pat ex-msg)
             prefix "sci.impl."
             friendly-name (when arity (str "function of arity " arity))
             ex-msg (if (:name fm)
                      (let [ns (symbol (str (:ns fm)))
                            var-name (:name fm)
                            var (get-in @env [:namespaces ns var-name])
                            fstr (when var (let [varf (if (instance? clojure.lang.IDeref var)
                                                        (deref var)
                                                        var)
                                                 varf (or
                                                       ;; resolve macro inner fn for comparison
                                                       (some-> varf meta :sci.impl/inner-fn)
                                                       varf)
                                                 fstr (clojure.lang.Compiler/demunge (str varf))
                                                 fstr (first (str/split fstr #"@"))
                                                 fstr (str/replace fstr (re-pattern (str "^" prefix)) "")]
                                            fstr))]
                        (cond (and fstr printed-fn (= fstr printed-fn))
                              (str/replace ex-msg printed-fn
                                           (str (:ns fm) "/" (:name fm)))
                              friendly-name (str/replace ex-msg match friendly-name)
                              :else ex-msg))
                      ex-msg)]
         ex-msg)
       ex-msg)))

(defn rethrow-with-location-of-node
  ([ctx ^Throwable e raw-node] (rethrow-with-location-of-node ctx (:bindings ctx) e raw-node))
  ([ctx bindings ^Throwable e raw-node]
   (if #?(:clj (or *in-try*
                   (not= (:main-thread-id ctx)
                         (.getId (Thread/currentThread))))
          :cljs *in-try*) (throw e)
       (let [stack (t/stack raw-node)
             node (t/sexpr raw-node)
             f (when (seqable? node)
                 (first node))
             fm (or (:sci.impl/f-meta stack)
                    (some-> f meta))
             env (:env ctx)
             id (:id ctx)
             d (ex-data e)
             st (or (when-let [st (:sci.impl/callstack d)]
                      st)
                    (volatile! '()))]
         (when stack
           (when-not (:special stack)
             #_(swap! env update-in [:sci.impl/callstack id]
                    (fn [vt]
                      (if vt
                        (do (vswap! vt conj stack)
                           vt)
                        (volatile! (list stack)))))
             (vswap! st conj stack)))
         (let [d (ex-data e)
               ;; st (:sci.impl/callstack d)
               wrapping-sci-error? (isa? (:type d) :sci/error)]
           (if wrapping-sci-error?
             (throw e)
             (let [ex-msg #?(:clj (.getMessage e)
                             :cljs (.-message e))
                   {:keys [:line :column :file]}
                   (or stack
                       (some-> env deref
                               :sci.impl/callstack (get id)
                               deref last meta)
                       (meta node))]
               (if (and line column)
                 (let [ex-msg #?(:clj (rewrite-ex-msg ex-msg env fm)
                                 :cljs ex-msg)
                       new-exception
                       (let [new-d {:type :sci/error
                                    :line line
                                    :column column
                                    :message ex-msg
                                    :sci.impl/callstack
                                    st
                                    #_(delay (when-let
                                               [v (get-in @(:env ctx) [:sci.impl/callstack (:id ctx)])]
                                             @v))
                                    :file file
                                    :locals bindings}]
                         (ex-info ex-msg new-d e))]
                   (throw new-exception))
                 (throw e)))))))))

(defn- iobj? [obj]
  (and #?(:clj (instance? clojure.lang.IObj obj)
          :cljs (implements? IWithMeta obj))
       (meta obj)))

(defn vary-meta*
  "Only adds metadata to obj if d is not nil and if obj already has meta"
  [obj f & args]
  (if (iobj? obj)
    (apply vary-meta obj f args)
    obj))

(defn strip-core-ns [sym]
  (case (namespace sym)
    ("clojure.core" "cljs.core") (symbol (name sym))
    sym))

(def allowed-loop (symbol "loop"))
(def allowed-recur (symbol "recur"))

(defn walk*
  [inner form]
  (cond
    (:sci.impl/op (meta form)) form
    (list? form) (with-meta (apply list (map inner form))
                   (meta form))
    #?(:clj (instance? clojure.lang.IMapEntry form) :cljs (map-entry? form))
    #?(:clj (clojure.lang.MapEntry/create (inner (key form)) (inner (val form)))
       :cljs (MapEntry. (inner (key form)) (inner (val form)) nil))
    (seq? form) (with-meta (doall (map inner form))
                  (meta form))
    #?(:clj (instance? clojure.lang.IRecord form)
       :cljs (record? form))
    (reduce (fn [r x] (conj r (inner x))) form form)
    (coll? form) (into (empty form) (map inner form))
    :else form))

(defn prewalk
  "Prewalk with metadata preservation. Does not prewalk :sci.impl/op nodes."
  [f form]
  (walk* (partial prewalk f) (f form)))

(defn namespace-object
  "Fetches namespaces from env if it exists. Else, if `create?`,
  produces one regardless of the existince of the namespace in env and
  adds it to env before returning it."
  [env ns-sym create? attr-map]
  (let [env* @env
        ns-map (get-in env* [:namespaces ns-sym])]
    (or (:obj ns-map)
        (when (or ns-map create?)
          (let [ns-obj (vars/->SciNamespace ns-sym attr-map)]
            (swap! env assoc-in [:namespaces ns-sym :obj] ns-obj)
            ns-obj)))))

(defn set-namespace! [ctx ns-sym attr-map]
  (let [env (:env ctx)
        attr-map (merge (meta ns-sym) attr-map)
        ns-obj (namespace-object env ns-sym true attr-map)]
    (t/setVal vars/current-ns ns-obj)))

(def eval-form-state (volatile! nil))
(def eval-require-state (volatile! nil))
(def eval-use-state (volatile! nil))
(def eval-resolve-state (volatile! nil))
(def eval-refer-state (volatile! nil))
(def macroexpand* (volatile! nil))
(def macroexpand-1* (volatile! nil))
(def eval* (volatile! nil))
(def eval-do* (volatile! nil))
(def eval-fn (volatile! nil))
(def eval-string* (volatile! nil))
(def lookup (volatile! nil))

(defn eval [sci-ctx form]
  (@eval-form-state sci-ctx form))

(defn split-when
  "Like partition-by but splits collection only when `pred` returns
  a truthy value. E.g. `(split-when odd? [1 2 3 4 5]) => ((1 2) (3 4) (5))`"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           f (complement pred)
           run (cons fst (take-while #(f %) (next s)))]
       (cons run (split-when pred (lazy-seq (drop (count run) s))))))))

(def ana-macros
  '#{do if and or let fn fn* def defn
     comment loop lazy-seq case try defmacro
     declare expand-dot* expand-constructor new . import in-ns ns var
     set! resolve #_#_macroexpand-1 macroexpand})

(defn ctx-fn
  ([f expr]
   (t/->EvalFn f nil expr nil))
  ([f m expr]
   (t/->EvalFn f m expr nil))
  ([f m expr stack]
   (t/->EvalFn f m expr stack)))

(defn maybe-destructured
  [params body]
  (if (every? symbol? params)
    {:params params
     :body body}
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        {:params new-params
         :body [`(let ~lets
                   ~@body)]}))))

(defn log [& xs]
  #?(:clj (.println System/err (str/join " " xs))
     :cljs (.log js/console (str/join " " xs))))
