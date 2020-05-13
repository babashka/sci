(ns sci.core
  (:refer-clojure :exclude [with-bindings with-in-str with-out-str
                            with-redefs binding future pmap alter-var-root
                            ns create-ns])
  (:require
   [sci.impl.interpreter :as i]
   [sci.impl.io :as sio]
   [sci.impl.macros :as macros]
   [sci.impl.opts :as opts]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros
            [sci.core :refer [with-bindings with-out-str copy-var]])))

#?(:clj (set! *warn-on-reflection* true))

(defn new-var
  "Alpha! Returns a new sci var. API subject to change."
  ([name] (doto (new-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name meta)))

(defn new-dynamic-var
  "Alpha! Same as new-var but adds :dynamic true to meta. API subject to
  change."
  ([name] (doto (new-dynamic-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-dynamic-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name (assoc meta :dynamic true))))

(defn new-macro-var
  "Alpha! Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself. API subject to change."
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar.
                         (vary-meta init-val
                                    assoc :sci/macro true)
                         name (assoc meta :macro true))))

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
    `(with-bindings ~(apply hash-map bindings)
       (do ~@body))))

(def in "Sci var that represents sci's `clojure.core/*in*`" sio/in)
(def out "Sci var that represents sci's `clojure.core/*out*`" sio/out)
(def err "Sci var that represents sci's `clojure.core/*err*`" sio/err)
(def ns "Sci var that represents sci's `clojure.core/*ns*`" vars/current-ns)
(def file "Sci var that represents sci's `clojure.core/*file*`" vars/current-file)
(def print-length "Sci var that represents sci's `clojure.core/*print-length*`" sio/print-length)

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

  - `:bindings`: a map of symbols to values, e.g.: `{'x 1}`. The
  symbols will acts as names bound to the corresponding values in the
  expressions.

  - `:namespaces`: a map of symbols to namespaces, where a namespace
  is a map with symbols to values, e.g.: `{'foo.bar {'x 1}}`. These
  namespaces can be used with `require`.

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:bindings` or `:namespaces` have to be explicitly
  enumerated.

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:realize-max`: integer; when provided, program may realize a
  maximum number of elements from sequences, e.g. `(vec (range))` will
  throw for any number. This also applies to sequences returned from
  the expression to the caller.

  - `:preset`: a pretermined set of options. Currently only
  `:termination-safe` is supported, which will set `:realize-max` to
  `100` and disallows the symbols `loop`, `recur` and `trampoline`.

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb}).

  - `:env`: an atom with a map in which state from the
  evaluation (defined namespaced and vars) will be persisted for
  re-use over multiple calls.
  "
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

;;;; Scratch

(comment
  (eval-string "(inc x)" {:bindings {'x 2}})
  )
