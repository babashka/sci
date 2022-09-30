# API

Anything in the SCI repository with `impl` in the name should be regarded
implementation detail and may change at any time. Please reach out if you end up
needing something from the dark `impl` side!
# Table of contents
-  [`sci.async`](#sci.async) 
    -  [`await`](#sci.async/await) - Mark promise to be flatteded into top level async evaluation, similar to top level await.
    -  [`await?`](#sci.async/await?) - Check if promise was marked with <code>await</code>.
    -  [`eval-string*`](#sci.async/eval-string*)
    -  [`eval-string+`](#sci.async/eval-string+) - Same as eval-string* but returns map with <code>:val</code>, the evaluation result, and <code>:ns</code>, the last active namespace.
    -  [`require`](#sci.async/require) - Async require that can be substituted for sync require by <code>{:namespaces {'clojure.core {'require scia/require}}}</code>.
-  [`sci.core`](#sci.core)  - The main SCI API namespace.
    -  [`*1`](#sci.core/*1)
    -  [`*2`](#sci.core/*2)
    -  [`*3`](#sci.core/*3)
    -  [`*e`](#sci.core/*e)
    -  [`add-class!`](#sci.core/add-class!) - Adds class (JVM class or JS object) to <code>ctx</code> as <code>class-name</code> (a symbol).
    -  [`add-import!`](#sci.core/add-import!) - Adds import of class named by <code>class-name</code> (a symbol) to namespace named by <code>ns-name</code> (a symbol) under alias <code>alias</code> (a symbol).
    -  [`add-namespace!`](#sci.core/add-namespace!) - Adds namespace map <code>ns-map</code> named by the symbol <code>ns-name</code> to <code>ctx</code>.
    -  [`all-ns`](#sci.core/all-ns) - Returns all SCI ns objects in the <code>ctx</code>.
    -  [`alter-var-root`](#sci.core/alter-var-root) - Atomically alters the root binding of sci var v by applying f to its current value plus any args.
    -  [`assert`](#sci.core/assert) - SCI var that represents SCI's clojure.core/*assert*.
    -  [`binding`](#sci.core/binding) - Macro for binding sci vars.
    -  [`copy-ns`](#sci.core/copy-ns) - Returns map of names to SCI vars as a result of copying public Clojure vars from ns-sym (a symbol).
    -  [`copy-var`](#sci.core/copy-var) - Copies contents from var <code>sym</code> to a new sci var.
    -  [`copy-var*`](#sci.core/copy-var*) - Copies Clojure var to SCI var.
    -  [`create-ns`](#sci.core/create-ns) - Creates namespace object.
    -  [`enable-unrestricted-access!`](#sci.core/enable-unrestricted-access!) - Calling this will enable - Altering core vars using <code>alter-var-root</code> - In CLJS: <code>set!</code> is able to set the value of any var.
    -  [`err`](#sci.core/err) - SCI var that represents SCI's <code>clojure.core/*err*</code>.
    -  [`eval-form`](#sci.core/eval-form) - Evaluates form (as produced by <code>parse-string</code> or <code>parse-next</code>) in the context of <code>ctx</code> (as produced with <code>init</code>).
    -  [`eval-string`](#sci.core/eval-string) - Evaluates string <code>s</code> as one or multiple Clojure expressions using the Small Clojure Interpreter.
    -  [`eval-string*`](#sci.core/eval-string*) - Evaluates string <code>s</code> in the context of <code>ctx</code> (as produced with <code>init</code>).
    -  [`file`](#sci.core/file) - SCI var that represents SCI's <code>clojure.core/*file*</code>.
    -  [`find-ns`](#sci.core/find-ns) - Returns SCI ns object as created with <code>sci/create-ns</code> from <code>ctx</code> found by <code>ns-sym</code>.
    -  [`fork`](#sci.core/fork) - Forks a context (as produced with <code>init</code>) into a new context.
    -  [`format-stacktrace`](#sci.core/format-stacktrace) - Returns a list of formatted stack trace elements as strings from stacktrace.
    -  [`future`](#sci.core/future) - Like clojure.core/future but also conveys sci bindings to the thread.
    -  [`get-column-number`](#sci.core/get-column-number)
    -  [`get-line-number`](#sci.core/get-line-number)
    -  [`in`](#sci.core/in) - SCI var that represents SCI's <code>clojure.core/*in*</code>.
    -  [`init`](#sci.core/init) - Creates an initial sci context from given options <code>opts</code>.
    -  [`intern`](#sci.core/intern) - Finds or creates a sci var named by the symbol name in the namespace ns (which can be a symbol or a sci namespace), setting its root binding to val if supplied.
    -  [`merge-opts`](#sci.core/merge-opts) - Updates a context with opts merged in and returns it.
    -  [`new-dynamic-var`](#sci.core/new-dynamic-var) - Same as new-var but adds :dynamic true to meta.
    -  [`new-macro-var`](#sci.core/new-macro-var) - Same as new-var but adds :macro true to meta as well as :sci/macro true to meta of the fn itself.
    -  [`new-var`](#sci.core/new-var) - Returns a new sci var.
    -  [`ns`](#sci.core/ns) - SCI var that represents SCI's <code>clojure.core/*ns*</code>.
    -  [`ns-name`](#sci.core/ns-name) - Returns name of SCI ns as symbol.
    -  [`out`](#sci.core/out) - SCI var that represents SCI's <code>clojure.core/*out*</code>.
    -  [`parse-next`](#sci.core/parse-next) - Parses next form from reader.
    -  [`parse-string`](#sci.core/parse-string) - Parses string <code>s</code> in the context of <code>ctx</code> (as produced with <code>init</code>).
    -  [`pmap`](#sci.core/pmap) - Like clojure.core/pmap but also conveys sci bindings to the threads.
    -  [`print-dup`](#sci.core/print-dup) - SCI var that represents SCI's <code>clojure.core/*print-dup*</code>.
    -  [`print-err-fn`](#sci.core/print-err-fn) - SCI var that represents SCI's <code>cljs.core/*print-err-fn*</code>.
    -  [`print-fn`](#sci.core/print-fn) - SCI var that represents SCI's <code>cljs.core/*print-fn*</code>.
    -  [`print-length`](#sci.core/print-length) - SCI var that represents SCI's <code>clojure.core/*print-length*</code>.
    -  [`print-level`](#sci.core/print-level) - SCI var that represents SCI's <code>clojure.core/*print-level*</code>.
    -  [`print-meta`](#sci.core/print-meta) - SCI var that represents SCI's <code>clojure.core/*print-meta*</code>.
    -  [`print-newline`](#sci.core/print-newline) - SCI var that represents SCI's <code>cljs.core/*print-newline*</code>.
    -  [`print-readably`](#sci.core/print-readably) - SCI var that represents SCI's <code>clojure.core/*print-readably*</code>.
    -  [`read-eval`](#sci.core/read-eval) - SCI var that represents SCI's <code>clojure.core/*read-eval*</code>.
    -  [`reader`](#sci.core/reader) - Coerces x into indexing pushback-reader to be used with parse-next.
    -  [`set!`](#sci.core/set!) - Establish thread local binding of dynamic var.
    -  [`stacktrace`](#sci.core/stacktrace) - Returns list of stacktrace element maps from exception, if available.
    -  [`var->symbol`](#sci.core/var->symbol) - Returns a fully qualified symbol from a <code>sci.lang.Var</code>.
    -  [`with-bindings`](#sci.core/with-bindings) - Macro for binding sci vars.
    -  [`with-in-str`](#sci.core/with-in-str) - Evaluates body in a context in which sci's *in* is bound to a fresh StringReader initialized with the string s.
    -  [`with-out-str`](#sci.core/with-out-str) - Evaluates exprs in a context in which sci's *out* is bound to a fresh StringWriter.
-  [`sci.ctx-store`](#sci.ctx-store)  - Canonical place for projects to store, update and retrieve a context.
    -  [`get-ctx`](#sci.ctx-store/get-ctx) - Retrieve stored ctx or throw an exception.
    -  [`reset-ctx!`](#sci.ctx-store/reset-ctx!) - Store <code>ctx</code>.
    -  [`swap-ctx!`](#sci.ctx-store/swap-ctx!) - Update <code>ctx</code> using <code>f</code> and <code>args</code>.
    -  [`with-ctx`](#sci.ctx-store/with-ctx) - Bind <code>ctx</code> during execution of body.
-  [`sci.impl.cljs`](#sci.impl.cljs) 
    -  [`cljs-ns-publics`](#sci.impl.cljs/cljs-ns-publics)
-  [`sci.lang`](#sci.lang) 
    -  [`Namespace`](#sci.lang/Namespace) - Representation of a SCI namespace, created e.g.
    -  [`Type`](#sci.lang/Type) - Representation of a SCI custom type, created e.g.
    -  [`Var`](#sci.lang/Var) - Representation of a SCI var, created e.g.

-----

-----

-----
# <a name="sci.async">sci.async</a>






<h2 dir="none"> <a name="sci.async/await">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L123-L128) `await`</a> </h2>
``` clojure

(await promise)
```


Mark promise to be flatteded into top level async evaluation, similar
  to top level await.

<h2 dir="none"> <a name="sci.async/await?">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L130-L133) `await?`</a> </h2>
``` clojure

(await? promise)
```


Check if promise was marked with [`await`](#sci.async/await).

<h2 dir="none"> <a name="sci.async/eval-string*">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L84-L107) `eval-string*`</a> </h2>
``` clojure

(eval-string* ctx s)
```


<h2 dir="none"> <a name="sci.async/eval-string+">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L109-L121) `eval-string+`</a> </h2>
``` clojure

(eval-string+ ctx s)
(eval-string+ ctx s opts)
```


Same as eval-string* but returns map with `:val`, the evaluation
  result, and `:ns`, the last active namespace. The return value can
  be passed back into `opts` to preserve the namespace state.

<h2 dir="none"> <a name="sci.async/require">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L141-L144) `require`</a> </h2>

Async require that can be substituted for sync require by
  `{:namespaces {'clojure.core {'require scia/require}}}`

-----
# <a name="sci.core">sci.core</a>


The main SCI API namespace.




<h2 dir="none"> <a name="sci.core/*1">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L134-L134) `*1`</a> </h2>

<h2 dir="none"> <a name="sci.core/*2">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L135-L135) `*2`</a> </h2>

<h2 dir="none"> <a name="sci.core/*3">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L136-L136) `*3`</a> </h2>

<h2 dir="none"> <a name="sci.core/*e">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L137-L137) `*e`</a> </h2>

<h2 dir="none"> <a name="sci.core/add-class!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L444-L454) `add-class!`</a> </h2>
``` clojure

(add-class! ctx class-name class)
```


Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.

<h2 dir="none"> <a name="sci.core/add-import!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L456-L461) `add-import!`</a> </h2>
``` clojure

(add-import! ctx ns-name class-name alias)
```


Adds import of class named by `class-name` (a symbol) to namespace named by [`ns-name`](#sci.core/ns-name) (a symbol) under alias `alias` (a symbol). Returns mutated context.

<h2 dir="none"> <a name="sci.core/add-namespace!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L463-L468) `add-namespace!`</a> </h2>
``` clojure

(add-namespace! ctx ns-name ns-map)
```


Adds namespace map `ns-map` named by the symbol [`ns-name`](#sci.core/ns-name) to
  `ctx`. Returns mutated context.

<h2 dir="none"> <a name="sci.core/all-ns">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L475-L478) `all-ns`</a> </h2>
``` clojure

(all-ns ctx)
```


Returns all SCI ns objects in the `ctx`

<h2 dir="none"> <a name="sci.core/alter-var-root">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L198-L204) `alter-var-root`</a> </h2>
``` clojure

(alter-var-root v f)
(alter-var-root v f & args)
```


Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.

<h2 dir="none"> <a name="sci.core/assert">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L132-L132) `assert`</a> </h2>

SCI var that represents SCI's clojure.core/*assert*

<h2 dir="none"> <a name="sci.core/binding">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L108-L115) `binding`</a> </h2>
``` clojure

(binding bindings & body)
```


Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.

<h2 dir="none"> <a name="sci.core/copy-ns">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L362-L442) `copy-ns`</a> </h2>
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

<h2 dir="none"> <a name="sci.core/copy-var">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L56-L77) `copy-var`</a> </h2>
``` clojure

(copy-var sym ns)
(copy-var sym ns opts)
```


Macro.


Copies contents from var `sym` to a new sci var. The value [`ns`](#sci.core/ns) is an
  object created with [`sci.core/create-ns`](#sci.core/create-ns). If new-name is supplied, the
  copied var will be named new-name.

<h2 dir="none"> <a name="sci.core/copy-var*">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L79-L94) `copy-var*`</a> </h2>
``` clojure

(copy-var* clojure-var sci-ns)
```


Copies Clojure var to SCI var. Runtime analog of compile time [`copy-var`](#sci.core/copy-var).

<h2 dir="none"> <a name="sci.core/create-ns">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L268-L272) `create-ns`</a> </h2>
``` clojure

(create-ns sym)
(create-ns sym meta)
```


Creates namespace object. Can be used in var metadata.

<h2 dir="none"> <a name="sci.core/enable-unrestricted-access!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L480-L486) `enable-unrestricted-access!`</a> </h2>
``` clojure

(enable-unrestricted-access!)
```


Calling this will enable
  - Altering core vars using [`alter-var-root`](#sci.core/alter-var-root)
  - In CLJS: [`set!`](#sci.core/set!) is able to set the value of any var.

<h2 dir="none"> <a name="sci.core/err">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L120-L120) `err`</a> </h2>

SCI var that represents SCI's `clojure.core/*err*`

<h2 dir="none"> <a name="sci.core/eval-form">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L302-L309) `eval-form`</a> </h2>
``` clojure

(eval-form ctx form)
```


Evaluates form (as produced by [`parse-string`](#sci.core/parse-string) or [`parse-next`](#sci.core/parse-next)) in the
  context of `ctx` (as produced with [`init`](#sci.core/init)). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`

<h2 dir="none"> <a name="sci.core/eval-string">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L217-L240) `eval-string`</a> </h2>
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

  - `:ns-aliases`: a map of aliases to namespaces that are globally valid, e.g. `{'clojure.test 'cljs.test}`

<h2 dir="none"> <a name="sci.core/eval-string*">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L262-L266) `eval-string*`</a> </h2>
``` clojure

(eval-string* ctx s)
```


Evaluates string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).

<h2 dir="none"> <a name="sci.core/file">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L122-L122) `file`</a> </h2>

SCI var that represents SCI's `clojure.core/*file*`

<h2 dir="none"> <a name="sci.core/find-ns">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L470-L473) `find-ns`</a> </h2>
``` clojure

(find-ns ctx ns-sym)
```


Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.

<h2 dir="none"> <a name="sci.core/fork">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L255-L260) `fork`</a> </h2>
``` clojure

(fork ctx)
```


Forks a context (as produced with [`init`](#sci.core/init)) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.

<h2 dir="none"> <a name="sci.core/format-stacktrace">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L316-L319) `format-stacktrace`</a> </h2>
``` clojure

(format-stacktrace stacktrace)
```


Returns a list of formatted stack trace elements as strings from stacktrace.

<h2 dir="none"> <a name="sci.core/future">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L172-L177) `future`</a> </h2>
``` clojure

(future & body)
```


Macro.


Like clojure.core/future but also conveys sci bindings to the thread.

<h2 dir="none"> <a name="sci.core/get-column-number">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L289-L290) `get-column-number`</a> </h2>
``` clojure

(get-column-number reader)
```


<h2 dir="none"> <a name="sci.core/get-line-number">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L286-L287) `get-line-number`</a> </h2>
``` clojure

(get-line-number reader)
```


<h2 dir="none"> <a name="sci.core/in">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L118-L118) `in`</a> </h2>

SCI var that represents SCI's `clojure.core/*in*`

<h2 dir="none"> <a name="sci.core/init">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L242-L248) `init`</a> </h2>
``` clojure

(init opts)
```


Creates an initial sci context from given options `opts`. The context
  can be used with [`eval-string*`](#sci.core/eval-string*). See [`eval-string`](#sci.core/eval-string) for available
  options. The internal organization of the context is implementation
  detail and may change in the future.

<h2 dir="none"> <a name="sci.core/intern">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L206-L215) `intern`</a> </h2>
``` clojure

(intern ctx sci-ns name)
(intern ctx sci-ns name val)
```


Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.

<h2 dir="none"> <a name="sci.core/merge-opts">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L250-L253) `merge-opts`</a> </h2>
``` clojure

(merge-opts ctx opts)
```


Updates a context with opts merged in and returns it.

<h2 dir="none"> <a name="sci.core/new-dynamic-var">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L35-L40) `new-dynamic-var`</a> </h2>
``` clojure

(new-dynamic-var name)
(new-dynamic-var name init-val)
(new-dynamic-var name init-val meta)
```


Same as new-var but adds :dynamic true to meta.

<h2 dir="none"> <a name="sci.core/new-macro-var">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L47-L54) `new-macro-var`</a> </h2>
``` clojure

(new-macro-var name init-val)
(new-macro-var name init-val meta)
```


Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.

<h2 dir="none"> <a name="sci.core/new-var">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L28-L33) `new-var`</a> </h2>
``` clojure

(new-var name)
(new-var name init-val)
(new-var name init-val meta)
```


Returns a new sci var.

<h2 dir="none"> <a name="sci.core/ns">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L121-L121) `ns`</a> </h2>

SCI var that represents SCI's `clojure.core/*ns*`

<h2 dir="none"> <a name="sci.core/ns-name">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L321-L324) `ns-name`</a> </h2>
``` clojure

(ns-name sci-ns)
```


Returns name of SCI ns as symbol.

<h2 dir="none"> <a name="sci.core/out">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L119-L119) `out`</a> </h2>

SCI var that represents SCI's `clojure.core/*out*`

<h2 dir="none"> <a name="sci.core/parse-next">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L292-L300) `parse-next`</a> </h2>
``` clojure

(parse-next ctx reader)
(parse-next ctx reader opts)
```


Parses next form from reader

<h2 dir="none"> <a name="sci.core/parse-string">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L274-L278) `parse-string`</a> </h2>
``` clojure

(parse-string ctx s)
```


Parses string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).

<h2 dir="none"> <a name="sci.core/pmap">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L179-L196) `pmap`</a> </h2>
``` clojure

(pmap f coll)
(pmap f coll & colls)
```


Like clojure.core/pmap but also conveys sci bindings to the threads.

<h2 dir="none"> <a name="sci.core/print-dup">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L128-L128) `print-dup`</a> </h2>

SCI var that represents SCI's `clojure.core/*print-dup*`

<h2 dir="none"> <a name="sci.core/print-err-fn">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L130-L130) `print-err-fn`</a> </h2>

SCI var that represents SCI's `cljs.core/*print-err-fn*`

<h2 dir="none"> <a name="sci.core/print-fn">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L129-L129) `print-fn`</a> </h2>

SCI var that represents SCI's `cljs.core/*print-fn*`

<h2 dir="none"> <a name="sci.core/print-length">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L124-L124) `print-length`</a> </h2>

SCI var that represents SCI's `clojure.core/*print-length*`

<h2 dir="none"> <a name="sci.core/print-level">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L125-L125) `print-level`</a> </h2>

SCI var that represents SCI's `clojure.core/*print-level*`

<h2 dir="none"> <a name="sci.core/print-meta">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L126-L126) `print-meta`</a> </h2>

SCI var that represents SCI's `clojure.core/*print-meta*`

<h2 dir="none"> <a name="sci.core/print-newline">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L131-L131) `print-newline`</a> </h2>

SCI var that represents SCI's `cljs.core/*print-newline*`

<h2 dir="none"> <a name="sci.core/print-readably">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L127-L127) `print-readably`</a> </h2>

SCI var that represents SCI's `clojure.core/*print-readably*`

<h2 dir="none"> <a name="sci.core/read-eval">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L123-L123) `read-eval`</a> </h2>

SCI var that represents SCI's `clojure.core/*read-eval*`

<h2 dir="none"> <a name="sci.core/reader">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L280-L284) `reader`</a> </h2>
``` clojure

(reader x)
```


Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.

<h2 dir="none"> <a name="sci.core/set!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L42-L45) `set!`</a> </h2>
``` clojure

(set! dynamic-var v)
```


Establish thread local binding of dynamic var

<h2 dir="none"> <a name="sci.core/stacktrace">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L311-L314) `stacktrace`</a> </h2>
``` clojure

(stacktrace ex)
```


Returns list of stacktrace element maps from exception, if available.

<h2 dir="none"> <a name="sci.core/var->symbol">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L488-L494) `var->symbol`</a> </h2>
``` clojure

(var->symbol sci-var)
```


Returns a fully qualified symbol from a `sci.lang.Var`

<h2 dir="none"> <a name="sci.core/with-bindings">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L97-L106) `with-bindings`</a> </h2>
``` clojure

(with-bindings bindings-map & body)
```


Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.

<h2 dir="none"> <a name="sci.core/with-in-str">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L143-L150) `with-in-str`</a> </h2>
``` clojure

(with-in-str s & body)
```


Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.

<h2 dir="none"> <a name="sci.core/with-out-str">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L153-L169) `with-out-str`</a> </h2>
``` clojure

(with-out-str & body)
```


Macro.


Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.

-----
# <a name="sci.ctx-store">sci.ctx-store</a>


Canonical place for projects to store, update and retrieve a context.
  This can be used by projects that need to expose their context to
  functions. SCI does not populate this dynamic var itself during
  evaluation. Projects like `sci.configs` assume this var to be set in
  some of their functions.




<h2 dir="none"> <a name="sci.ctx-store/get-ctx">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L27-L33) `get-ctx`</a> </h2>
``` clojure

(get-ctx)
```


Retrieve stored ctx or throw an exception.

<h2 dir="none"> <a name="sci.ctx-store/reset-ctx!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L15-L19) `reset-ctx!`</a> </h2>
``` clojure

(reset-ctx! ctx)
```


Store `ctx`

<h2 dir="none"> <a name="sci.ctx-store/swap-ctx!">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L21-L25) `swap-ctx!`</a> </h2>
``` clojure

(swap-ctx! f & args)
```


Update `ctx` using `f` and `args`

<h2 dir="none"> <a name="sci.ctx-store/with-ctx">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L35-L39) `with-ctx`</a> </h2>
``` clojure

(with-ctx ctx & body)
```


Macro.


Bind `ctx` during execution of body.

-----

-----

-----
# <a name="sci.impl.cljs">sci.impl.cljs</a>






<h2 dir="none"> <a name="sci.impl.cljs/cljs-ns-publics">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/cljs.cljc#L15-L15) `cljs-ns-publics`</a> </h2>

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----

-----
# <a name="sci.lang">sci.lang</a>






<h2 dir="none"> <a name="sci.lang/Namespace">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L234-L254) `Namespace`</a> </h2>

Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly.

<h2 dir="none"> <a name="sci.lang/Type">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L23-L70) `Type`</a> </h2>

Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly.

<h2 dir="none"> <a name="sci.lang/Var">[:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L79-L226) `Var`</a> </h2>

Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly.
