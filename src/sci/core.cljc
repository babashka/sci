(ns sci.core
  (:refer-clojure :exclude [with-bindings with-in-str with-out-str
                            with-redefs binding future pmap alter-var-root
                            intern ns create-ns set! *1 *2 *3 *e
                            ns-name])
  (:require
   [sci.impl.callstack :as cs]
   [sci.impl.interpreter :as i]
   [sci.impl.io :as sio]
   [sci.impl.macros :as macros]
   [sci.impl.namespaces :as namespaces]
   [sci.impl.opts :as opts]
   [sci.impl.parser :as parser]
   [sci.impl.types :as t]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros
            [sci.core :refer [with-bindings with-out-str copy-var copy-ns]])))

#?(:clj (set! *warn-on-reflection* true))

(defn new-var
  "Returns a new sci var."
  ([name] (doto (new-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name meta false)))

(defn new-dynamic-var
  "Same as new-var but adds :dynamic true to meta."
  ([name] (doto (new-dynamic-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-dynamic-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name (assoc meta :dynamic true) false)))

(defn set!
  "Establish thread local binding of dynamic var"
  [dynamic-var v]
  (t/setVal dynamic-var v))

(defn new-macro-var
  "Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself."
  ([name init-val] (new-macro-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar.
                         (vary-meta init-val
                                    assoc :sci/macro true)
                         name (assoc meta :macro true) false)))

(defmacro copy-var
  "Copies contents from var `sym` to a new sci var. The value `ns` is an
  object created with `sci.core/create-ns`."
  ([sym ns]
   `(let [ns# ~ns
          var# (var ~sym)
          val# (deref var#)
          m# (-> var# meta)
          ns-name# (vars/getName ns#)
          name# (:name m#)
          name-sym# (symbol (str ns-name#) (str name#))
          new-m# {:doc (:doc m#)
                  :name name#
                  :arglists (:arglists m#)
                  :ns ns#}]
      (cond (:dynamic m#)
            (new-dynamic-var name# val# new-m#)
            (:macro m#)
            (new-macro-var name# val# new-m#)
            :else (new-var name# val# new-m#)))))

(macros/deftime
  (defmacro with-bindings
    "Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka."
    [bindings-map & body]
    `(let [bm# ~bindings-map]
       (assert (map? bm#))
       (vars/push-thread-bindings bm#) ;; important: outside try
       (try
         (do ~@body)
         (finally (vars/pop-thread-bindings)))))

  (defmacro binding
    "Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values."
    [bindings & body]
    (assert (vector? bindings))
    (assert (even? (count bindings)))
    `(with-bindings ~(apply hash-map bindings)
       (do ~@body))))

;; I/O
(def in "Sci var that represents sci's `clojure.core/*in*`" sio/in)
(def out "Sci var that represents sci's `clojure.core/*out*`" sio/out)
(def err "Sci var that represents sci's `clojure.core/*err*`" sio/err)
(def ns "Sci var that represents sci's `clojure.core/*ns*`" vars/current-ns)
(def file "Sci var that represents sci's `clojure.core/*file*`" vars/current-file)
(def print-length "Sci var that represents sci's `clojure.core/*print-length*`" sio/print-length)
(def print-level "Sci var that represents sci's `clojure.core/*print-level*`" sio/print-level)
(def print-meta "Sci var that represents sci's `clojure.core/*print-meta*`" sio/print-meta)
(def print-readably "Sci var that represents sci's `clojure.core/*print-readably*`" sio/print-readably)
#?(:cljs (def print-fn "Sci var that represents sci's `cljs.core/*print-fn*`" sio/print-fn))
#?(:cljs (def print-newline "Sci var that represents sci's `cljs.core/*print-newline*`" sio/print-newline))

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
  [v f & args]
  (apply vars/alter-var-root v f args))

(defn intern
  "Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var."
  ([ctx sci-ns name]
   (namespaces/sci-intern ctx sci-ns name))
  ([ctx sci-ns name val]
   (namespaces/sci-intern ctx sci-ns name val)))

(defn eval-string
  "Evaluates string `s` as one or multiple Clojure expressions using the Small Clojure Interpreter.

  The map `opts` may contain the following:

  - `:namespaces`: a map of symbols to namespaces, where a namespace
  is a map with symbols to values, e.g.: `{'foo.bar {'x 1}}`. These
  namespaces can be used with `require`.

  - `:bindings`: `:bindings x` is the same as `:namespaces {'user x}`.

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:bindings` or `:namespaces` have to be explicitly
  enumerated.

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb}).

  - `:env`: an atom with a map in which state from the
  evaluation (defined namespaced and vars) will be persisted for
  re-use over multiple calls.

  - `:disable-arity-checks`: disables arity checks for single-arity
  functions."
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

(defn create-ns
  "Creates namespace object. Can be used in var metadata."
  ([sym] (create-ns sym nil))
  ([sym meta]
   (vars/->SciNamespace sym meta)))

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
  (namespaces/sci-ns-name sci-ns))

(defn -copy-ns
  {:no-doc true}
  [ns-publics-map sci-ns]
  (reduce (fn [ns-map [var-name var]]
            (let [m (:meta var)]
              (assoc ns-map var-name
                     (new-var (symbol var-name) (:val var)
                              (assoc m :ns sci-ns)))))
          {}
          ns-publics-map))

(defn- process-publics [publics {:keys [exclude]}]
  (let [publics (if exclude (apply dissoc publics exclude) publics)]
    publics))

(defn- exclude-when-meta [publics-map meta-fn key-fn val-fn skip-keys ]
  (reduce (fn [ns-map [var-name var]]
            (let [m (meta-fn var)]
              (if (some m skip-keys)
                ns-map
                (assoc ns-map (key-fn var-name) (val-fn var m)))))
          {}
          publics-map))

(defn- meta-fn [opts]
  (cond (= :all opts) identity
        opts #(select-keys %  opts)
        :else #(select-keys % [:arglists
                               :no-doc
                               :macro
                               :doc])))

(macros/deftime
  (def ^:private cljs-ns-publics
    (try (resolve 'cljs.analyzer.api/ns-publics)
         (catch #?(:clj Exception
                   :cljs :default) _ nil)))
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
  manually.
"
    ([ns-sym sci-ns] `(copy-ns ~ns-sym ~sci-ns nil))
    ([ns-sym sci-ns opts]
     (macros/? :clj (let [publics-map (ns-publics ns-sym)
                          publics-map (process-publics publics-map opts)
                          mf (meta-fn (:copy-meta opts))
                          publics-map (exclude-when-meta
                                       publics-map
                                       meta
                                       (fn [k]
                                         (list 'quote k))
                                       (fn [var m]
                                         {:name (list 'quote (:name m))
                                          :val (deref var)
                                          :meta (list 'quote (mf m))})
                                       (or (:exclude-when-meta opts)
                                           [:no-doc :skip-wiki]))]
                      `(-copy-ns ~publics-map ~sci-ns))
               :cljs (let [publics-map (cljs-ns-publics ns-sym)
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
                                           :meta (mf m)})
                                        (or (:exclude-when-meta opts)
                                            [:no-doc :skip-wiki]))]
                       `(-copy-ns ~publics-map ~sci-ns))))))

;;;; Scratch
