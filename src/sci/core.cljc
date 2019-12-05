(ns sci.core
  (:refer-clojure :exclude [*in* *out* *err* *ns* with-bindings with-in-str with-out-str
                            with-redefs])
  (:require
   [sci.impl.interpreter :as i]
   [sci.impl.vars :as vars]
   [sci.impl.io :as sio]
   [sci.impl.macros :as macros])
  #?(:cljs (:require-macros [sci.core :refer [with-bindings with-out-str with-redefs]])))

#?(:clj (set! *warn-on-reflection* true))

(defn new-var
  "Alpha! Returns a new sci var. API subject to change."
  ([name] (doto (new-var name nil nil)
            (vars/unbind)))
  ([name val] (new-var name val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name meta)))

(defn new-dynamic-var
  "Alpha! Same as new-var but adds :dynamic true to meta. API subject to
  change."
  ([name] (doto (new-dynamic-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-dynamic-var name init-val (meta name)))
  ([name init-val meta] (sci.impl.vars.SciVar. init-val name (assoc meta :dynamic true))))

(defn set-var-root!
  "Alpha! Sets root of sci var. API subject to change."
  [sci-var root-val]
  (vars/bindRoot sci-var root-val))

(macros/deftime
  (defmacro with-bindings
    "Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka."
    [bindings & body]
    `(do (vars/push-thread-bindings ~bindings) ;; important: outside try
         (try
           (do ~@body)
           (finally (vars/pop-thread-bindings)))))

  (defmacro with-redefs
    "Like with-bindings, but temporarily redefines sci vars during a call
  to body. Must be called with map of sci dynamic vars to values. Each
  val of binding-map will replace the root value of its key which must
  be a sci var.  After body, the root values of all the sci vars will
  be set back to their old values. These temporary changes will be
  visible in all threads."
    [binding-map & body]
    `(let [root-bind# (fn [m#]
                        (doseq [[a-var# a-val#] m#]
                          (vars/bindRoot a-var# a-val#)))
           bm# ~binding-map
           ks# (keys bm#)
           old-vals# (zipmap ks#
                             (map #(vars/getRawRoot %) ks#))]
       (try
         (root-bind# bm#)
         (do ~@body)
         (finally
           (root-bind# old-vals#))))))

;; `*in*`, `*out*`, `*err*` are set to :dynamic to suppress a compiler warning
;; they are really not dynamic to sci library users, but represent dynamic vars
;; *inside* sci
(def ^:dynamic *in* "Sci var that represents sci's `clojure.core/*in*`" sio/in)
#?(:clj (.setDynamic #'*in* false))
(alter-meta! #'*in* assoc :dynamic false)

(def ^:dynamic *out* "Sci var that represents sci's `clojure.core/*out*`" sio/out)
#?(:clj (.setDynamic #'*out* false))
(alter-meta! #'*out* assoc :dynamic false)

(def ^:dynamic *err* "Sci var that represents sci's `clojure.core/*err*`" sio/err)
#?(:clj (.setDynamic #'*err* false))
(alter-meta! #'*err* assoc :dynamic false)

(macros/deftime
  (defmacro with-in-str
    "Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s."
    [s & body]
    `(let [in# (-> (java.io.StringReader. ~s)
                   (clojure.lang.LineNumberingPushbackReader.))]
       (with-bindings {*in* in#}
         (do ~@body)))))

(macros/deftime
  (defmacro with-out-str
    "Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
    [& body]
    `(let [out# (macros/? :clj (java.io.StringWriter.)
                          :cljs (goog.string/StringBuffer.))]
       (with-bindings {*out* out#}
         (do ~@body)
         (str out#)))))

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

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb})."
  ([s] (eval-string s nil))
  ([s opts]
   (i/eval-string s opts)))

;;;; Scratch

(comment
  (eval-string "(inc x)" {:bindings {'x 2}})
  )
