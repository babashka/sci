(ns sci.core
  "The main SCI API namespace."
  (:refer-clojure :exclude [with-bindings with-in-str with-out-str
                            with-redefs binding future pmap alter-var-root
                            intern ns create-ns set! *1 *2 *3 *e
                            ns-name assert print-dup find-ns all-ns ns-name
                            resolve])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]
   [clojure.tools.reader.reader-types :as rt]
   [edamame.core :as edamame]
   [edamame.impl.parser]
   [sci.ctx-store :as store]
   [sci.impl.callstack :as cs]
   [sci.impl.interpreter :as i]
   [sci.impl.io :as sio]
   [sci.impl.macros :as macros]
   [sci.impl.namespaces :as namespaces]
   [sci.impl.opts :as opts]
   [sci.impl.parser :as parser]
   [sci.impl.types :as t]
   [sci.impl.unrestrict :as unrestrict]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]
   [sci.lang])
  #?(:cljs (:require-macros
            [sci.core :refer [with-bindings with-out-str copy-var
                              copy-ns]]
            [sci.impl.cljs])))

#?(:clj (set! *warn-on-reflection* true))

(defn new-var
  "Returns a new sci var."
  ([name] (doto (new-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta] (sci.lang.Var. init-val name (assoc meta :name (utils/unqualify-symbol name)) false false nil)))

(defn new-dynamic-var
  "Same as new-var but adds :dynamic true to meta."
  ([name] (doto (new-dynamic-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-dynamic-var name init-val (meta name)))
  ([name init-val meta] (sci.lang.Var. init-val name (assoc meta :dynamic true :name (utils/unqualify-symbol name)) false false nil)))

(defn set!
  "Establish thread local binding of dynamic var"
  [dynamic-var v]
  (t/setVal dynamic-var v))

(defn new-macro-var
  "Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself."
  ([name init-val] (new-macro-var name init-val (meta name)))
  ([name init-val meta] (sci.lang.Var.
                         (vary-meta init-val
                                    assoc :sci/macro true)
                         name (assoc meta :macro true :name (utils/unqualify-symbol name)) false false nil)))

(defmacro copy-var
  "Copies contents from var `sym` to a new sci var. The value `ns` is an
  object created with `sci.core/create-ns`.

  Options:

  - :name: The name of the copied var. Defaults to the original var name."
  ([sym ns]
   `(copy-var ~sym ~ns nil))
  ([sym ns opts]
   (let [nm (:name opts)]
     `(let [ns# ~ns
            var# (var ~sym)
            val# (deref var#)
            m# (-> var# meta)
            name# (or ~nm (:name m#))
            new-m# {:doc (:doc m#)
                    :name name#
                    :arglists (:arglists m#)
                    :ns ns#}]
        (cond (:dynamic m#)
              (new-dynamic-var name# val# new-m#)
              (or (:macro m#) (:sci/macro m#))
              (new-macro-var name# val# new-m#)
              :else (new-var name# val# new-m#))))))

(defn copy-var*
  "Copies Clojure var to SCI var. Runtime analog of compile time `copy-var`."
  [clojure-var sci-ns]
  (let [m (meta clojure-var)
        nm (:name m)
        doc (:doc m)
        arglists (:arglists m)
        dynamic (:dynamic m)
        macro (:macro m)
        new-m (cond-> {:ns sci-ns
                       :name nm}
                macro (assoc :macro true)
                doc (assoc :doc doc)
                arglists (assoc :arglists arglists)
                dynamic (assoc :dynamic dynamic))]
    (new-var nm @clojure-var new-m)))

(macros/deftime
  (defmacro with-bindings
    "Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka."
    [bindings-map & body]
    `(let [bm# ~bindings-map]
       (c/assert (map? bm#))
       (vars/push-thread-bindings bm#) ;; important: outside try
       (try
         (do ~@body)
         (finally (vars/pop-thread-bindings)))))

  (defmacro binding
    "Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values."
    [bindings & body]
    (vector? bindings)
    (even? (count bindings))
    `(with-bindings ~(apply hash-map bindings)
       (do ~@body))))

;; I/O
(def in "SCI var that represents SCI's `clojure.core/*in*`" sio/in)
(def out "SCI var that represents SCI's `clojure.core/*out*`" sio/out)
(def err "SCI var that represents SCI's `clojure.core/*err*`" sio/err)
(def ns "SCI var that represents SCI's `clojure.core/*ns*`" utils/current-ns)
(def file "SCI var that represents SCI's `clojure.core/*file*`" utils/current-file)
(def read-eval "SCI var that represents SCI's `clojure.core/*read-eval*`" parser/read-eval)
(def print-length "SCI var that represents SCI's `clojure.core/*print-length*`" sio/print-length)
(def print-level "SCI var that represents SCI's `clojure.core/*print-level*`" sio/print-level)
(def print-meta "SCI var that represents SCI's `clojure.core/*print-meta*`" sio/print-meta)
(def print-readably "SCI var that represents SCI's `clojure.core/*print-readably*`" sio/print-readably)
(def print-dup "SCI var that represents SCI's `clojure.core/*print-dup*`" sio/print-dup-var)
(def print-namespace-maps "SCI var that represents SCI's `clojure.core/*print-namespace-maps*`" sio/print-namespace-maps)
#?(:cljs (def print-fn "SCI var that represents SCI's `cljs.core/*print-fn*`" sio/print-fn))
#?(:cljs (def print-err-fn "SCI var that represents SCI's `cljs.core/*print-err-fn*`" sio/print-err-fn))
#?(:cljs (def print-newline "SCI var that represents SCI's `cljs.core/*print-newline*`" sio/print-newline))
(def assert "SCI var that represents SCI's clojure.core/*assert*" namespaces/assert-var)

(def *1 namespaces/*1)
(def *2 namespaces/*2)
(def *3 namespaces/*3)
(def *e namespaces/*e)

;; REPL variables

(macros/deftime
  (defmacro with-in-str
    "Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s."
    [s & body]
    `(let [in# (-> (java.io.StringReader. ~s)
                   (clojure.lang.LineNumberingPushbackReader.))]
       (with-bindings {in in#}
         (do ~@body)))))

(macros/deftime
  (defmacro with-out-str
    "Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
    [& body]
    (macros/? :clj
              `(let [out# (java.io.StringWriter.)]
                 (with-bindings {out out#}
                   (do ~@body)
                   (str out#)))
              :cljs
              `(let [sb# (goog.string/StringBuffer.)]
                 (cljs.core/binding []
                   (with-bindings {sci.core/print-newline true
                                   sci.core/print-fn (fn [x#] (.append sb# x#))}
                     (do ~@body)
                     (str sb#)))))))

(macros/deftime
  (defmacro future
    "Like clojure.core/future but also conveys sci bindings to the thread."
    [& body]
    `(let [f# (-> (fn [] ~@body)
                  (vars/binding-conveyor-fn))]
       (future-call f#))))

#?(:clj (defn pmap
          "Like clojure.core/pmap but also conveys sci bindings to the threads."
          ([f coll]
           (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
                 rets (map #(future (f %)) coll)
                 step (fn step [[x & xs :as vs] fs]
                        (lazy-seq
                         (if-let [s (seq fs)]
                           (cons (deref x) (step xs (rest s)))
                           (map deref vs))))]
             (step rets (drop n rets))))
          ([f coll & colls]
           (let [step (fn step [cs]
                        (lazy-seq
                         (let [ss (map seq cs)]
                           (when (every? identity ss)
                             (cons (map first ss) (step (map rest ss)))))))]
             (pmap #(apply f %) (step (cons coll colls)))))))

(defn alter-var-root
  "Atomically alters the root binding of sci var v by applying f to its
  current value plus any args."
  ([v f]
   (c/binding [unrestrict/*unrestricted* true]
     (vars/alter-var-root v f)))
  ([v f & args]
   (c/binding [unrestrict/*unrestricted* true]
     (apply vars/alter-var-root v f args))))

(defn intern
  "Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var."
  ([ctx sci-ns name]
   (store/with-ctx ctx
     (namespaces/sci-intern sci-ns name)))
  ([ctx sci-ns name val]
   (store/with-ctx ctx
     (namespaces/sci-intern sci-ns name val))))

(defn eval-string
  "Evaluates string `s` as one or multiple Clojure expressions using the Small Clojure Interpreter.

  The map `opts` may contain the following:

  - `:namespaces`: a map of symbols to namespaces, where a namespace
  is a map with symbols to values, e.g.: `{'foo.bar {'x 1}}`. These
  namespaces can be used with `require`.

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:namespaces` have to be explicitly
  enumerated.

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb}).

  - `:ns-aliases`: a map of aliases to namespaces that are globally valid, e.g. `{'clojure.test 'cljs.test}`

  - `:bindings`: DEPRECATED - `:bindings x` is the same as `:namespaces {'user x}`."
  ([s] (eval-string s nil))
  ([s opts]
   (i/eval-string s opts)))

(defn init
  "Creates an initial sci context from given options `opts`. The context
  can be used with `eval-string*`. See `eval-string` for available
  options. The internal organization of the context is implementation
  detail and may change in the future."
  [opts]
  (opts/init opts))

(defn merge-opts
  "Updates a context with opts merged in and returns it."
  [ctx opts]
  (opts/merge-opts ctx opts))

(defn fork
  "Forks a context (as produced with `init`) into a new context. Any new
  vars created in the new context won't be visible in the original
  context."
  [ctx]
  (update ctx :env (fn [env] (atom @env))))

(defn eval-string*
  "Evaluates string `s` in the context of `ctx` (as produced with
  `init`)."
  [ctx s]
  (sci.impl.interpreter/eval-string* ctx s))

(defn eval-string+
  "Evaluates string `s` in the context of `ctx` (as produced with
  `init`).

  Options:
  *`:ns` - the namespace to start evaluation in (defaults to the value of `sci/ns`)

  Returns map with:
  * `:val` - the evaluated value
  * `:ns` - the namespace object"
  ([ctx s]
   (eval-string+ ctx s nil))
  ([ctx s opts]
   (sci.impl.interpreter/eval-string* ctx s (assoc opts :sci.impl/eval-string+ true))))

(defn create-ns
  "Creates namespace object. Can be used in var metadata."
  ([sym] (create-ns sym nil))
  ([sym meta]
   (sci.lang/->Namespace sym meta)))

(defn parse-string
  "Parses string `s` in the context of `ctx` (as produced with
  `init`)."
  ([ctx s]
   (parser/parse-string ctx s)))

(defn reader
  "Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader."
  [x]
  (parser/reader x))

(defn source-reader [x]
  (edamame/source-reader x))

(defn get-line-number [reader]
  (parser/get-line-number reader))

(defn get-column-number [reader]
  (parser/get-column-number reader))

(defn parse-next
  "Parses next form from reader"
  ([ctx reader] (parse-next ctx reader {}))
  ([ctx reader opts]
   (let [v (parser/parse-next ctx reader opts)]
     (if (utils/kw-identical? parser/eof v)
       (or (get opts :eof)
           ::eof)
       v))))

(defn parse-next+string
  "Parses next form from reader"
  ([ctx reader] (parse-next+string ctx reader {}))
  ([ctx reader opts]
   (if (rt/source-logging-reader? reader)
     (let [v (parse-next ctx reader opts)
           s (str/trim (str (edamame.impl.parser/buf reader)))]
       [v s])
     (throw (ex-info "parse-next+string must be called with source-reader" {})))))

(defn eval-form
  "Evaluates form (as produced by `parse-string` or `parse-next`) in the
  context of `ctx` (as produced with `init`). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`"
  [ctx form]
  (let [ctx (assoc ctx :id (or (:id ctx) (gensym)))]
    (i/eval-form ctx form)))

(defn stacktrace
  "Returns list of stacktrace element maps from exception, if available."
  [ex]
  (some-> ex ex-data :sci.impl/callstack cs/stacktrace))

(defn format-stacktrace
  "Returns a list of formatted stack trace elements as strings from stacktrace."
  [stacktrace]
  (cs/format-stacktrace stacktrace))

(defn ns-name
  "Returns name of SCI ns as symbol."
  [sci-ns]
  (t/getName sci-ns))

(defn -copy-ns
  {:no-doc true}
  [ns-publics-map sci-ns]
  (reduce (fn [ns-map [var-name var]]
            (let [m (:meta var)]
              (assoc ns-map var-name
                     (new-var var-name (if-let [var (:var var)]
                                         @var
                                         (:val var))
                              (assoc m :ns sci-ns :name var-name)))))
          {}
          ns-publics-map))

(defn- process-publics [publics {:keys [exclude]}]
  (let [publics (if exclude (apply dissoc publics exclude) publics)]
    publics))

(defn- exclude-when-meta [publics-map meta-fn key-fn val-fn skip-keys]
  (reduce (fn [ns-map [var-name var]]
            (if-let [m (meta-fn var)]
              (if (some m skip-keys)
                ns-map
                (assoc ns-map (key-fn var-name) (val-fn var m)))
              ns-map))
          {}
          publics-map))

(defn normalize-meta [m]
  (if-let [sci-macro (:sci/macro m)]
    (assoc m :macro sci-macro)
    m))

(defn- meta-fn [opts]
  (cond (= :all opts) normalize-meta
        opts #(-> (select-keys % opts) normalize-meta)
        :else #(-> (select-keys % [:arglists
                                   :no-doc
                                   :macro
                                   :sci/macro
                                   :doc
                                   :dynamic])
                   normalize-meta)))

(macros/deftime
  (defmacro copy-ns
    "Returns map of names to SCI vars as a result of copying public
  Clojure vars from ns-sym (a symbol). Attaches sci-ns (result of
  sci/create-ns) to meta. Copies :name, :macro :doc, :no-doc
  and :argslists metadata.

  Options:

  - :exclude: a seqable of names to exclude from the
  namespace. Defaults to none.

  - :copy-meta: a seqable of keywords to copy from the original var
  meta.  Use :all instead of a seqable to copy all. Defaults
  to [:doc :arglists :macro].

  - :exclude-when-meta: seqable of keywords; vars with meta matching
  these keys are excluded.  Defaults to [:no-doc :skip-wiki]

  The selection of vars is done at compile time which is mostly
  important for ClojureScript to not pull in vars into the compiled
  JS. Any additional vars can be added after the fact with sci/copy-var
  manually."
    ([ns-sym sci-ns] `(copy-ns ~ns-sym ~sci-ns nil))
    ([ns-sym sci-ns opts]
     (macros/? :clj
               ;; this branch is hit by macroexpanding in JVM Clojure, not in the CLJS compiler
               (let [publics-map (ns-publics ns-sym)
                     publics-map (process-publics publics-map opts)
                     mf (meta-fn (:copy-meta opts))
                     publics-map (exclude-when-meta
                                  publics-map
                                  meta
                                  (fn [k]
                                    (list 'quote k))
                                  (fn [var m]
                                    {:name (list 'quote (:name m))
                                     :var var
                                     :meta (list 'quote (mf m))})
                                  (or (:exclude-when-meta opts)
                                      [:no-doc :skip-wiki]))]
                 ;; (prn publics-map)
                 `(-copy-ns ~publics-map ~sci-ns))
               :cljs #?(:clj
                        ;; this branch is hit by macroexpanding within the CLJS
                        ;; compiler on the JVM. At ths point, cljs-ns-publics
                        ;; refers to the right var.
                        (let [ns? #_:clj-kondo/ignore
                              (sci.impl.cljs/cljs-find-ns ns-sym)
                              _ (when-not ns?
                                  (throw (ex-info (str "Copying non-existent namespace: " ns-sym) {:ns ns-sym})))
                              publics-map
                              #_:clj-kondo/ignore
                              (sci.impl.cljs/cljs-ns-publics ns-sym)
                              publics-map (process-publics publics-map opts)
                              mf (meta-fn (:copy-meta opts))
                              publics-map (exclude-when-meta
                                           publics-map
                                           :meta
                                           (fn [k]
                                             (list 'quote k))
                                           (fn [var m]
                                             {:name (list 'quote (:name var))
                                              :val (:name var)
                                              :meta (let [m (mf m)]
                                                      (if (:protocol-symbol m)
                                                        (list 'quote m)
                                                        m))})
                                           (or (:exclude-when-meta opts)
                                               [:no-doc :skip-wiki]))]
                          `(-copy-ns ~publics-map ~sci-ns))
                        :cljs
                        ;; this branch is hit by self-hosted
                        (let [ns?
                              #_:clj-kondo/ignore
                              (cljs.analyzer.api/find-ns ns-sym)
                              _ (when-not ns?
                                  (throw (ex-info (str "Copying non-existent namespace: " ns-sym) {:ns ns-sym})))
                              publics-map
                              #_:clj-kondo/ignore
                              (cljs.analyzer.api/ns-publics ns-sym)
                              publics-map (process-publics publics-map opts)
                              mf (meta-fn (:copy-meta opts))
                              publics-map (exclude-when-meta
                                           publics-map
                                           :meta
                                           (fn [k]
                                             (list 'quote k))
                                           (fn [var m]
                                             {:name (list 'quote (:name var))
                                              :val (:name var)
                                              :meta (let [m (mf m)]
                                                      (if (:protocol-symbol m)
                                                        (list 'quote m)
                                                        m))})
                                           (or (:exclude-when-meta opts)
                                               [:no-doc :skip-wiki]))]
                          `(-copy-ns ~publics-map ~sci-ns)))))))

(defn add-import!
  "Adds import of class named by `class-name` (a symbol) to namespace named by `ns-name` (a symbol) under alias `alias` (a symbol). Returns mutated context."
  [ctx ns-name class-name alias]
  ;; This relies on an internal format of the context and may change at any time.
  (swap! (:env ctx)
         (fn [env]
           (-> env
               (assoc-in [:namespaces ns-name :imports alias] class-name)
               (update-in [:namespaces ns-name :refer 'clojure.core :exclude] (fnil conj #{}) alias))))
  ctx)

(defn add-class!
  "Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context."
  [ctx class-name class]
  ;; This relies on an internal format of the context and may change at any time.
  (let [env (:env ctx)]
    (swap! env (fn [env]
                 (-> env
                     (assoc-in [:class->opts class-name :class] class)
                     (assoc-in [:raw-classes class-name] class))))
    ctx))

(defn add-namespace!
  "Adds namespace map `ns-map` named by the symbol `ns-name` to
  `ctx`. Returns mutated context."
  [ctx ns-name ns-map]
  (swap! (:env ctx) update-in [:namespaces ns-name] merge ns-map)
  ctx)

(defn find-ns
  "Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`."
  [ctx ns-sym]
  (namespaces/sci-find-ns* ctx ns-sym))

(defn all-ns
  "Returns all SCI ns objects in the `ctx`"
  [ctx]
  (store/with-ctx ctx
    (namespaces/sci-all-ns)))

(defn enable-unrestricted-access!
  "Calling this will enable
  - Altering core vars using `alter-var-root`
  - In CLJS: `set!` is able to set the value of any var.
  - In CLJS: instance method calls are not restricted to only `:classes`

  In the future, more unrestricted access may be added, so only use this when you're not using SCI as a sandbox."
  []
  #?(:cljs (set! unrestrict/*unrestricted* true)
     :clj (c/alter-var-root #'unrestrict/*unrestricted* (constantly true))))

(defn var->symbol
  "Returns a fully qualified symbol from a `sci.lang.Var`"
  [sci-var]
  (let [m (meta sci-var)
        sci-ns (:ns m)
        n (:name m)]
    (symbol (str sci-ns) (str n))))

(defn resolve [ctx sym]
  (@utils/eval-resolve-state ctx {} sym))

#?(:cljs
   (defn add-js-lib!
     "Add js library to context, so it can be used with `require`."
     [ctx name-str js-lib]
     (swap! (:env ctx) assoc-in [:js-libs name-str] js-lib)
     ctx))

;;;; Scratch
