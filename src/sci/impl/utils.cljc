(ns sci.impl.utils
  {:no-doc true}
  (:require [clojure.string :as str]
            [sci.impl.types :as t]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(derive :sci.error/realized-beyond-max :sci/error)
(derive :sci.error/parse :sci/error)

(defn constant? [x]
  (or (number? x) (string? x) (keyword? x) (boolean? x)))

(defn eval? [x]
  (some-> x meta :sci.impl/op))

(def kw-identical? #?(:clj identical? :cljs keyword-identical?))

(defn mark-eval-call
  ([expr]
   (vary-meta
    expr
    (fn [m]
      (-> m
          (assoc :sci.impl/op :call)
          (assoc :ns @vars/current-ns)
          (assoc :file @vars/current-file)))))
  ([expr extra-key extra-val]
   (vary-meta
    expr
    (fn [m]
      (-> m
          (assoc :sci.impl/op :call)
          (assoc :ns @vars/current-ns)
          (assoc :file @vars/current-file)
          (assoc extra-key extra-val))))))

(defn mark-eval
  [expr]
  (vary-meta
   expr
   (fn [m]
     (assoc m :sci.impl/op :eval))))

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

(defn rethrow-with-location-of-node [ctx ^Throwable e raw-node]
  (if *in-try* (throw e)
      (let [node (t/sexpr raw-node)
            m (meta node)
            f (when (seqable? node) (first node))
            fm (some-> f meta)
            op (when (and fm m)
                 (.get ^java.util.Map m :sci.impl/op))
            special? (or
                      ;; special call like def
                      (and (symbol? f) (not op))
                      ;; anonymous function
                      (kw-identical? :fn op)
                      ;; special thing like require
                      (identical? needs-ctx op))
            env (:env ctx)
            id (:id ctx)]
        (when (not special?)
          (swap! env update-in [:sci.impl/callstack id]
                 (fn [vt]
                   (if vt
                     (do (vswap! vt conj node)
                         vt)
                     (volatile! (list node))))))
        (let [d (ex-data e)
              wrapping-sci-error? (isa? (:type d) :sci/error)]
          (if wrapping-sci-error?
            (throw e)
            (let [ex-msg #?(:clj (.getMessage e)
                            :cljs (.-message e))
                  {:keys [:line :column :file]}
                  (or (some-> env deref
                              :sci.impl/callstack (get id)
                              deref last meta)
                      (meta node))]
              (if (and line column)
                (let [ex-msg (if (and ex-msg (:name fm))
                               (str/replace ex-msg #"(sci\.impl\.)?fns/fun/[a-zA-Z0-9-]+--\d+"
                                            (str (:ns fm) "/" (:name fm)))
                               ex-msg)
                      new-exception
                      (let [new-d {:type :sci/error
                                   :line line
                                   :column column
                                   :message ex-msg
                                   :sci.impl/callstack
                                   (delay (when-let
                                              [v (get-in @(:env ctx) [:sci.impl/callstack (:id ctx)])]
                                            @v))
                                   :file file
                                   :locals (:bindings ctx)}]
                        (ex-info ex-msg new-d e))]
                  (throw new-exception))
                (throw e))))))))

(defn iobj? [obj]
  (and #?(:clj (instance? clojure.lang.IObj obj)
          :cljs (implements? IWithMeta obj))
       (meta obj)))

(defn vary-meta*
  "Only adds metadata to obj if d is not nil and if obj already has meta"
  [obj f & args]
  (if (iobj? obj)
    (apply vary-meta obj f args)
    obj))

(defn merge-meta
  "Only adds metadata to obj if d is not nil and if meta on obj isn't already nil."
  [obj d]
  (if (and d #?(:clj (instance? clojure.lang.IObj obj)
                :cljs (implements? IWithMeta obj)))
    (if-let [m (meta obj)]
      (do
        nil
        ;; this should not happen, turn on for debugging
        #_(when (identical? m d) (prn :identical obj d m))
        (with-meta obj (merge m d)))
      obj)
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
     comment loop lazy-seq for doseq case try defmacro
     declare expand-dot* expand-constructor new . import in-ns ns var
     set! resolve #_#_macroexpand-1 macroexpand})

(defn ctx-fn
  ([f expr]
   (t/->EvalFn f nil expr))
  ([f m expr]
   (t/->EvalFn f m expr)))

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
