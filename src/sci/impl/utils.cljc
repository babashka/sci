(ns sci.impl.utils
  {:no-doc true}
  (:require [sci.impl.types :as t]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(derive :sci.error/realized-beyond-max :sci/error)
(derive :sci.error/parse :sci/error)

(defn constant? [x]
  (or (number? x) (string? x) (keyword? x) (boolean? x)))

(defn mark-resolve-sym
  [sym]
  (vary-meta
   sym
   (fn [m]
     (assoc m :sci.impl/op :resolve-sym))))

(defn eval? [x]
  (some-> x meta :sci.impl/op))

(def kw-identical? #?(:clj identical? :cljs keyword-identical?))

(defn gensym*
  ([] (mark-resolve-sym (gensym)))
  ([prefix] (mark-resolve-sym (gensym prefix))))

(defn mark-eval-call
  [expr]
  (vary-meta
   expr
   (fn [m]
     (assoc m
            :sci.impl/op :call
            :ns @vars/current-ns
            :file @vars/current-file))))

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

(defn rethrow-with-location-of-node [ctx ^Throwable e node]
  (let [interpret? (t/-interpret? node)
        node* (if interpret?
                (t/-expr node)
                node)
        m (meta node*)
        f (when (seqable? node*) (first node*))
        fm (some-> f meta)
        op (when fm (if interpret?
                      (t/-tag node)
                      ;; TODO: this is probably wrong?
                      (when m (.get ^java.util.Map m :sci.impl/op))))
        node node*]
    (when (not (or
                ;; special call like def
                (and (symbol? f) (not op))
                ;; anonymous function
                (kw-identical? :fn op)
                ;; special thing like require
                (identical? needs-ctx op)))
      (swap! (:env ctx) update-in [:callstack (:id ctx)]
             (fn [vt]
               (if vt
                 (do (vswap! vt conj node)
                     vt)
                 (volatile! (list node))))))
    (if-not *in-try*
      (let [d (ex-data e)]
        (if (isa? (:type d) :sci/error)
          (throw e)
          (let [ex-msg #?(:clj (or (.getMessage e))
                          :cljs (.-message e))
                {:keys [:line :column :file]
                 :or {line (:line ctx)
                      column (:column ctx)}} (meta node)]
            (if (and line column)
              (let [m ex-msg
                    new-exception
                    (let [d (ex-data e)
                          base {:type :sci/error
                                :line line
                                :column column
                                :message m
                                :callstack (delay (when-let [v (get-in @(:env ctx) [:callstack (:id ctx)])]
                                                    @v))
                                :file file
                                :locals (:bindings ctx)}
                          phase (:phase ctx)
                          base (if phase
                                 (assoc base :phase phase)
                                 base)]
                      (ex-info m (merge base d) e))]
                (throw new-exception))
              (throw e))))
        (throw e))
      (throw e))))

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
(def eval-macroexpand-state (volatile! nil))
(def interpret (volatile! nil))
(def eval-do* (volatile! nil))
(def eval-fn (volatile! nil))
(def eval-call (volatile! nil))

(defn split-when
  "Like partition-by but splits collection only when `pred` returns
  a truthy value. E.g70. `(split-when odd? [1 2 3 4 5]) => ((1 2) (3 4) (5))`"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           f (complement pred)
           run (cons fst (take-while #(f %) (next s)))]
       (cons run (split-when pred (lazy-seq (drop (count run) s))))))))
