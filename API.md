# API

Anything in the SCI repository with `impl` in the name should be regarded
implementation detail and may change at any time. Please reach out if you end up
needing something from the dark `impl` side!
# Table of contents
-  [`sci.async`](#sciasync) 
    -  [`await`](#await) - Mark promise to be flatteded into top level async evaluation, similar
    -  [`await?`](#await?) - Check if promise was marked with <code>await</code>.
    -  [`eval-string*`](#eval-string)
    -  [`require`](#require) - Async require that can be substituted for sync require by
-  [`sci.core`](#scicore)  - The main SCI API namespace.
    -  [`*1`](#1)
    -  [`*2`](#2)
    -  [`*3`](#3)
    -  [`*e`](#e)
    -  [`add-class!`](#add-class) - Adds class (JVM class or JS object) to <code>ctx</code> as <code>class-name</code> (a
    -  [`add-import!`](#add-import) - Adds import of class named by <code>class-name</code> (a symbol) to namespace named by `ns-
    -  [`add-namespace!`](#add-namespace) - Adds namespace map <code>ns-map</code> named by the symbol <code>ns-name</code> to
    -  [`all-ns`](#all-ns) - Returns all SCI ns objects in the <code>ctx</code>
    -  [`alter-var-root`](#alter-var-root) - Atomically alters the root binding of sci var v by applying f to its
    -  [`assert`](#assert) - SCI var that represents SCI's clojure.core/*assert*
    -  [`binding`](#binding) - Macro for binding sci vars
    -  [`cljs-ns-publics`](#cljs-ns-publics)
    -  [`copy-ns`](#copy-ns) - Returns map of names to SCI vars as a result of copying public
    -  [`copy-var`](#copy-var) - Copies contents from var <code>sym</code> to a new sci var
    -  [`create-ns`](#create-ns) - Creates namespace object
    -  [`err`](#err) - SCI var that represents SCI's <code>clojure.core/*err*</code>
    -  [`eval-form`](#eval-form) - Evaluates form (as produced by <code>parse-string</code> or <code>parse-next</code>) in the
    -  [`eval-string`](#eval-string-1) - Evaluates string <code>s</code> as one or multiple Clojure expressions using the Small Cloj
    -  [`eval-string*`](#eval-string-2) - Evaluates string <code>s</code> in the context of <code>ctx</code> (as produced with
    -  [`file`](#file) - SCI var that represents SCI's <code>clojure.core/*file*</code>
    -  [`find-ns`](#find-ns) - Returns SCI ns object as created with <code>sci/create-ns</code> from <code>ctx</code> found by `ns-sy
    -  [`fork`](#fork) - Forks a context (as produced with <code>init</code>) into a new context
    -  [`format-stacktrace`](#format-stacktrace) - Returns a list of formatted stack trace elements as strings from stacktrace.
    -  [`future`](#future) - Like clojure.core/future but also conveys sci bindings to the thread.
    -  [`get-column-number`](#get-column-number)
    -  [`get-line-number`](#get-line-number)
    -  [`in`](#in) - SCI var that represents SCI's <code>clojure.core/*in*</code>
    -  [`init`](#init) - Creates an initial sci context from given options <code>opts</code>
    -  [`intern`](#intern) - Finds or creates a sci var named by the symbol name in the namespace
    -  [`merge-opts`](#merge-opts) - Updates a context with opts merged in and returns it.
    -  [`new-dynamic-var`](#new-dynamic-var) - Same as new-var but adds :dynamic true to meta.
    -  [`new-macro-var`](#new-macro-var) - Same as new-var but adds :macro true to meta as well
    -  [`new-var`](#new-var) - Returns a new sci var.
    -  [`ns`](#ns) - SCI var that represents SCI's <code>clojure.core/*ns*</code>
    -  [`ns-name`](#ns-name) - Returns name of SCI ns as symbol.
    -  [`out`](#out) - SCI var that represents SCI's <code>clojure.core/*out*</code>
    -  [`parse-next`](#parse-next) - Parses next form from reader
    -  [`parse-string`](#parse-string) - Parses string <code>s</code> in the context of <code>ctx</code> (as produced with
    -  [`pmap`](#pmap) - Like clojure.core/pmap but also conveys sci bindings to the threads.
    -  [`print-dup`](#print-dup) - SCI var that represents SCI's <code>clojure.core/*print-dup*</code>
    -  [`print-err-fn`](#print-err-fn) - SCI var that represents SCI's <code>cljs.core/*print-err-fn*</code>
    -  [`print-fn`](#print-fn) - SCI var that represents SCI's <code>cljs.core/*print-fn*</code>
    -  [`print-length`](#print-length) - SCI var that represents SCI's <code>clojure.core/*print-length*</code>
    -  [`print-level`](#print-level) - SCI var that represents SCI's <code>clojure.core/*print-level*</code>
    -  [`print-meta`](#print-meta) - SCI var that represents SCI's <code>clojure.core/*print-meta*</code>
    -  [`print-newline`](#print-newline) - SCI var that represents SCI's <code>cljs.core/*print-newline*</code>
    -  [`print-readably`](#print-readably) - SCI var that represents SCI's <code>clojure.core/*print-readably*</code>
    -  [`read-eval`](#read-eval) - SCI var that represents SCI's <code>clojure.core/*read-eval*</code>
    -  [`reader`](#reader) - Coerces x into indexing pushback-reader to be used with
    -  [`set!`](#set) - Establish thread local binding of dynamic var
    -  [`stacktrace`](#stacktrace) - Returns list of stacktrace element maps from exception, if available.
    -  [`with-bindings`](#with-bindings) - Macro for binding sci vars
    -  [`with-in-str`](#with-in-str) - Evaluates body in a context in which sci's *in* is bound to a fresh
    -  [`with-out-str`](#with-out-str) - Evaluates exprs in a context in which sci's *out* is bound to a fresh
-  [`sci.lang`](#scilang) 
    -  [`->Type`](#->Type) - Representation of a SCI custom type, created e.g
    -  [`IVar`](#IVar) - Marker interface for SCI vars.
    -  [`Type`](#Type) - Representation of a SCI custom type, created e.g
# sci.async 





## `await`
``` clojure

(await promise)
```


Mark promise to be flatteded into top level async evaluation, similar
  to top level await.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L107-L112)</sub>
## `await?`
``` clojure

(await? promise)
```


Check if promise was marked with `await`.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L114-L117)</sub>
## `eval-string*`
``` clojure

(eval-string* ctx s)
```

<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L84-L105)</sub>
## `require`

Async require that can be substituted for sync require by
  `{:namespaces {'clojure.core {'require scia/require}}}`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L123-L126)</sub>
# sci.core 


The main SCI API namespace.



## `*1`
<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L114-L114)</sub>
## `*2`
<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L115-L115)</sub>
## `*3`
<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L116-L116)</sub>
## `*e`
<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L117-L117)</sub>
## `add-class!`
``` clojure

(add-class! ctx class-name class)
```


Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L441-L451)</sub>
## `add-import!`
``` clojure

(add-import! ctx ns-name class-name alias)
```


Adds import of class named by `class-name` (a symbol) to namespace named by `ns-name` (a symbol) under alias `alias` (a symbol). Returns mutated context.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L453-L458)</sub>
## `add-namespace!`
``` clojure

(add-namespace! ctx ns-name ns-map)
```


Adds namespace map `ns-map` named by the symbol `ns-name` to
  `ctx`. Returns mutated context.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L460-L465)</sub>
## `all-ns`
``` clojure

(all-ns ctx)
```


Returns all SCI ns objects in the `ctx`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L472-L475)</sub>
## `alter-var-root`
``` clojure

(alter-var-root v f)
(alter-var-root v f & args)
```


Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L178-L184)</sub>
## `assert`

SCI var that represents SCI's clojure.core/*assert*
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L112-L112)</sub>
## `binding`
``` clojure

(binding bindings & body)
```


Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L88-L95)</sub>
## `cljs-ns-publics`
<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L351-L351)</sub>
## `copy-ns`
``` clojure

(copy-ns ns-sym sci-ns)
(copy-ns ns-sym sci-ns opts)
```


Macro.


Returns map of names to SCI vars as a result of copying public
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
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L359-L439)</sub>
## `copy-var`
``` clojure

(copy-var sym ns)
(copy-var sym ns opts)
```


Macro.


Copies contents from var `sym` to a new sci var. The value `ns` is an
  object created with `sci.core/create-ns`. If new-name is supplied, the 
  copied var will be named new-name.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L53-L74)</sub>
## `create-ns`
``` clojure

(create-ns sym)
(create-ns sym meta)
```


Creates namespace object. Can be used in var metadata.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L250-L254)</sub>
## `err`

SCI var that represents SCI's `clojure.core/*err*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L100-L100)</sub>
## `eval-form`
``` clojure

(eval-form ctx form)
```


Evaluates form (as produced by `parse-string` or `parse-next`) in the
  context of `ctx` (as produced with `init`). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L284-L291)</sub>
## `eval-string`
``` clojure

(eval-string s)
(eval-string s opts)
```


Evaluates string `s` as one or multiple Clojure expressions using the Small Clojure Interpreter.

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
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L197-L222)</sub>
## `eval-string*`
``` clojure

(eval-string* ctx s)
```


Evaluates string `s` in the context of `ctx` (as produced with
  `init`).
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L244-L248)</sub>
## `file`

SCI var that represents SCI's `clojure.core/*file*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L102-L102)</sub>
## `find-ns`
``` clojure

(find-ns ctx ns-sym)
```


Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L467-L470)</sub>
## `fork`
``` clojure

(fork ctx)
```


Forks a context (as produced with `init`) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L237-L242)</sub>
## `format-stacktrace`
``` clojure

(format-stacktrace stacktrace)
```


Returns a list of formatted stack trace elements as strings from stacktrace.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L298-L301)</sub>
## `future`
``` clojure

(future & body)
```


Macro.


Like clojure.core/future but also conveys sci bindings to the thread.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L152-L157)</sub>
## `get-column-number`
``` clojure

(get-column-number reader)
```

<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L271-L272)</sub>
## `get-line-number`
``` clojure

(get-line-number reader)
```

<sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L268-L269)</sub>
## `in`

SCI var that represents SCI's `clojure.core/*in*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L98-L98)</sub>
## `init`
``` clojure

(init opts)
```


Creates an initial sci context from given options `opts`. The context
  can be used with `eval-string*`. See `eval-string` for available
  options. The internal organization of the context is implementation
  detail and may change in the future.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L224-L230)</sub>
## `intern`
``` clojure

(intern ctx sci-ns name)
(intern ctx sci-ns name val)
```


Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L186-L195)</sub>
## `merge-opts`
``` clojure

(merge-opts ctx opts)
```


Updates a context with opts merged in and returns it.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L232-L235)</sub>
## `new-dynamic-var`
``` clojure

(new-dynamic-var name)
(new-dynamic-var name init-val)
(new-dynamic-var name init-val meta)
```


Same as new-var but adds :dynamic true to meta.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L32-L37)</sub>
## `new-macro-var`
``` clojure

(new-macro-var name init-val)
(new-macro-var name init-val meta)
```


Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L44-L51)</sub>
## `new-var`
``` clojure

(new-var name)
(new-var name init-val)
(new-var name init-val meta)
```


Returns a new sci var.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L25-L30)</sub>
## `ns`

SCI var that represents SCI's `clojure.core/*ns*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L101-L101)</sub>
## `ns-name`
``` clojure

(ns-name sci-ns)
```


Returns name of SCI ns as symbol.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L303-L306)</sub>
## `out`

SCI var that represents SCI's `clojure.core/*out*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L99-L99)</sub>
## `parse-next`
``` clojure

(parse-next ctx reader)
(parse-next ctx reader opts)
```


Parses next form from reader
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L274-L282)</sub>
## `parse-string`
``` clojure

(parse-string ctx s)
```


Parses string `s` in the context of `ctx` (as produced with
  `init`).
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L256-L260)</sub>
## `pmap`
``` clojure

(pmap f coll)
(pmap f coll & colls)
```


Like clojure.core/pmap but also conveys sci bindings to the threads.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L159-L176)</sub>
## `print-dup`

SCI var that represents SCI's `clojure.core/*print-dup*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L108-L108)</sub>
## `print-err-fn`

SCI var that represents SCI's `cljs.core/*print-err-fn*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L110-L110)</sub>
## `print-fn`

SCI var that represents SCI's `cljs.core/*print-fn*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L109-L109)</sub>
## `print-length`

SCI var that represents SCI's `clojure.core/*print-length*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L104-L104)</sub>
## `print-level`

SCI var that represents SCI's `clojure.core/*print-level*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L105-L105)</sub>
## `print-meta`

SCI var that represents SCI's `clojure.core/*print-meta*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L106-L106)</sub>
## `print-newline`

SCI var that represents SCI's `cljs.core/*print-newline*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L111-L111)</sub>
## `print-readably`

SCI var that represents SCI's `clojure.core/*print-readably*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L107-L107)</sub>
## `read-eval`

SCI var that represents SCI's `clojure.core/*read-eval*`
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L103-L103)</sub>
## `reader`
``` clojure

(reader x)
```


Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L262-L266)</sub>
## `set!`
``` clojure

(set! dynamic-var v)
```


Establish thread local binding of dynamic var
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L39-L42)</sub>
## `stacktrace`
``` clojure

(stacktrace ex)
```


Returns list of stacktrace element maps from exception, if available.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L293-L296)</sub>
## `with-bindings`
``` clojure

(with-bindings bindings-map & body)
```


Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L77-L86)</sub>
## `with-in-str`
``` clojure

(with-in-str s & body)
```


Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L123-L130)</sub>
## `with-out-str`
``` clojure

(with-out-str & body)
```


Macro.


Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L133-L149)</sub>
# sci.lang 





## `->Type`
``` clojure

(->Type --data-impl --namespace-impl --name-impl)
```


Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L20-L67)</sub>
## `IVar`

Marker interface for SCI vars.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L8-L8)</sub>
## `Type`

Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`.
<br><sub>[source](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L20-L67)</sub>
