(ns sci.core
  (:refer-clojure :exclude [with-bindings with-in-str with-out-str
                            with-redefs binding future pmap alter-var-root
                            ns create-ns set! *1 *2 *3 *e])
  (:require
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
            [sci.core :refer [with-bindings with-out-str copy-var]])))

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

(def in "Sci var that represents sci's `clojure.core/*in*`" sio/in)
(def out "Sci var that represents sci's `clojure.core/*out*`" sio/out)
(def err "Sci var that represents sci's `clojure.core/*err*`" sio/err)
(def ns "Sci var that represents sci's `clojure.core/*ns*`" vars/current-ns)
(def file "Sci var that represents sci's `clojure.core/*file*`" vars/current-file)
(def print-length "Sci var that represents sci's `clojure.core/*print-length*`" sio/print-length)
(def print-level "Sci var that represents sci's `clojure.core/*print-level*`" sio/print-level)
(def print-meta "Sci var that represents sci's `clojure.core/*print-meta*`" sio/print-meta)
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
    `(let [out# (macros/? :clj (java.io.StringWriter.)
                          :cljs (goog.string/StringBuffer.))]
       (with-bindings {out out#}
         (do ~@body)
         (str out#)))))

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

;;;; Scratch

(comment
  (eval-string "(inc x)" {:bindings {'x 2}})
  )
