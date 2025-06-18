# API

Anything in the SCI repository with `impl` in the name should be regarded
implementation detail and may change at any time. Please reach out if you end up
needing something from the dark `impl` side!
# Table of contents
-  [`sci.async`](#sci.async) 
    -  [`await`](#sci.async/await) - Mark promise to be flatteded into top level async evaluation, similar to top level await.
    -  [`await?`](#sci.async/await?) - Check if promise was marked with <code>await</code>.
    -  [`eval-form`](#sci.async/eval-form) - Eval single form in ctx.
    -  [`eval-form+`](#sci.async/eval-form+) - Eval single form in ctx, return map of <code>:val</code> and <code>:ns</code>.
    -  [`eval-string*`](#sci.async/eval-string*)
    -  [`eval-string+`](#sci.async/eval-string+) - Same as eval-string* but returns map with <code>:val</code>, the evaluation result, and <code>:ns</code>, the last active namespace.
    -  [`require`](#sci.async/require) - Async require that can be substituted for sync require by <code>{:namespaces {&apos;clojure.core {&apos;require scia/require}}}</code>.
-  [`sci.core`](#sci.core)  - The main SCI API namespace.
    -  [`*1`](#sci.core/*1)
    -  [`*2`](#sci.core/*2)
    -  [`*3`](#sci.core/*3)
    -  [`*e`](#sci.core/*e)
    -  [`add-class!`](#sci.core/add-class!) - Adds class (JVM class or JS object) to <code>ctx</code> as <code>class-name</code> (a symbol).
    -  [`add-import!`](#sci.core/add-import!) - Adds import of class named by <code>class-name</code> (a symbol) to namespace named by <code>ns-name</code> (a symbol) under alias <code>alias</code> (a symbol).
    -  [`add-js-lib!`](#sci.core/add-js-lib!) - Add js library to context, so it can be used with <code>require</code>.
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
    -  [`eval-string+`](#sci.core/eval-string+) - Evaluates string <code>s</code> in the context of <code>ctx</code> (as produced with <code>init</code>).
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
    -  [`normalize-meta`](#sci.core/normalize-meta)
    -  [`ns`](#sci.core/ns) - SCI var that represents SCI's <code>clojure.core/*ns*</code>.
    -  [`ns-name`](#sci.core/ns-name) - Returns name of SCI ns as symbol.
    -  [`out`](#sci.core/out) - SCI var that represents SCI's <code>clojure.core/*out*</code>.
    -  [`parse-next`](#sci.core/parse-next) - Parses next form from reader.
    -  [`parse-next+string`](#sci.core/parse-next+string) - Parses next form from reader.
    -  [`parse-string`](#sci.core/parse-string) - Parses string <code>s</code> in the context of <code>ctx</code> (as produced with <code>init</code>).
    -  [`pmap`](#sci.core/pmap) - Like clojure.core/pmap but also conveys sci bindings to the threads.
    -  [`print-dup`](#sci.core/print-dup) - SCI var that represents SCI's <code>clojure.core/*print-dup*</code>.
    -  [`print-err-fn`](#sci.core/print-err-fn) - SCI var that represents SCI's <code>cljs.core/*print-err-fn*</code>.
    -  [`print-fn`](#sci.core/print-fn) - SCI var that represents SCI's <code>cljs.core/*print-fn*</code>.
    -  [`print-length`](#sci.core/print-length) - SCI var that represents SCI's <code>clojure.core/*print-length*</code>.
    -  [`print-level`](#sci.core/print-level) - SCI var that represents SCI's <code>clojure.core/*print-level*</code>.
    -  [`print-meta`](#sci.core/print-meta) - SCI var that represents SCI's <code>clojure.core/*print-meta*</code>.
    -  [`print-namespace-maps`](#sci.core/print-namespace-maps) - SCI var that represents SCI's <code>clojure.core/*print-namespace-maps*</code>.
    -  [`print-newline`](#sci.core/print-newline) - SCI var that represents SCI's <code>cljs.core/*print-newline*</code>.
    -  [`print-readably`](#sci.core/print-readably) - SCI var that represents SCI's <code>clojure.core/*print-readably*</code>.
    -  [`read-eval`](#sci.core/read-eval) - SCI var that represents SCI's <code>clojure.core/*read-eval*</code>.
    -  [`reader`](#sci.core/reader) - Coerces x into indexing pushback-reader to be used with parse-next.
    -  [`resolve`](#sci.core/resolve)
    -  [`set!`](#sci.core/set!) - Establish thread local binding of dynamic var.
    -  [`source-reader`](#sci.core/source-reader)
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
-  [`sci.lang`](#sci.lang) 
    -  [`Namespace`](#sci.lang/namespace) - Representation of a SCI namespace, created e.g.
    -  [`Type`](#sci.lang/type) - Representation of a SCI custom type, created e.g.
    -  [`Var`](#sci.lang/var) - Representation of a SCI var, created e.g.
    -  [`notify-watches`](#sci.lang/notify-watches)

-----
# <a name="sci.async">sci.async</a>






## <a name="sci.async/await">`await`</a>
``` clojure

(await promise)
```
Function.

Mark promise to be flatteded into top level async evaluation, similar
  to top level await.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L197-L202">Source</a></sub></p>

## <a name="sci.async/await?">`await?`</a>
``` clojure

(await? promise)
```
Function.

Check if promise was marked with [`await`](#sci.async/await).
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L204-L207">Source</a></sub></p>

## <a name="sci.async/eval-form">`eval-form`</a>
``` clojure

(eval-form ctx form)
```
Function.

Eval single form in ctx.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L143-L146">Source</a></sub></p>

## <a name="sci.async/eval-form+">`eval-form+`</a>
``` clojure

(eval-form+ ctx s)
(eval-form+ ctx form opts)
```
Function.

Eval single form in ctx, return map of `:val` and `:ns`.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L148-L155">Source</a></sub></p>

## <a name="sci.async/eval-string*">`eval-string*`</a>
``` clojure

(eval-string* ctx s)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L182-L184">Source</a></sub></p>

## <a name="sci.async/eval-string+">`eval-string+`</a>
``` clojure

(eval-string+ ctx s)
(eval-string+ ctx s opts)
```
Function.

Same as eval-string* but returns map with `:val`, the evaluation
  result, and `:ns`, the last active namespace. The return value can
  be passed back into `opts` to preserve the namespace state.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L186-L195">Source</a></sub></p>

## <a name="sci.async/require">`require`</a>




Async require that can be substituted for sync require by
  `{:namespaces {'clojure.core {'require scia/require}}}`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L220-L223">Source</a></sub></p>

-----
# <a name="sci.core">sci.core</a>


The main SCI API namespace.




## <a name="sci.core/*1">`*1`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L144-L144">Source</a></sub></p>

## <a name="sci.core/*2">`*2`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L145-L145">Source</a></sub></p>

## <a name="sci.core/*3">`*3`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L146-L146">Source</a></sub></p>

## <a name="sci.core/*e">`*e`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L147-L147">Source</a></sub></p>

## <a name="sci.core/add-class!">`add-class!`</a>
``` clojure

(add-class! ctx class-name class)
```
Function.

Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L520-L530">Source</a></sub></p>

## <a name="sci.core/add-import!">`add-import!`</a>
``` clojure

(add-import! ctx ns-name class-name alias)
```
Function.

Adds import of class named by `class-name` (a symbol) to namespace named by [`ns-name`](#sci.core/ns-name) (a symbol) under alias `alias` (a symbol). Returns mutated context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L509-L518">Source</a></sub></p>

## <a name="sci.core/add-js-lib!">`add-js-lib!`</a>
``` clojure

(add-js-lib! ctx name-str js-lib)
```
Function.

Add js library to context, so it can be used with `require`.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L573-L577">Source</a></sub></p>

## <a name="sci.core/add-namespace!">`add-namespace!`</a>
``` clojure

(add-namespace! ctx ns-name ns-map)
```
Function.

Adds namespace map `ns-map` named by the symbol [`ns-name`](#sci.core/ns-name) to
  `ctx`. Returns mutated context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L532-L537">Source</a></sub></p>

## <a name="sci.core/all-ns">`all-ns`</a>
``` clojure

(all-ns ctx)
```
Function.

Returns all SCI ns objects in the `ctx`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L544-L548">Source</a></sub></p>

## <a name="sci.core/alter-var-root">`alter-var-root`</a>
``` clojure

(alter-var-root v f)
(alter-var-root v f & args)
```
Function.

Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L207-L215">Source</a></sub></p>

## <a name="sci.core/assert">`assert`</a>




SCI var that represents SCI's clojure.core/*assert*
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L142-L142">Source</a></sub></p>

## <a name="sci.core/binding">`binding`</a>
``` clojure

(binding bindings & body)
```
Macro.

Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L117-L124">Source</a></sub></p>

## <a name="sci.core/copy-ns">`copy-ns`</a>
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
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L411-L507">Source</a></sub></p>

## <a name="sci.core/copy-var">`copy-var`</a>
``` clojure

(copy-var sym ns)
(copy-var sym ns opts)
```
Macro.

Copies contents from var `sym` to a new sci var. The value [`ns`](#sci.core/ns) is an
  object created with [`sci.core/create-ns`](#sci.core/create-ns).

  Options:

  - :name: The name of the copied var. Defaults to the original var name.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L62-L86">Source</a></sub></p>

## <a name="sci.core/copy-var*">`copy-var*`</a>
``` clojure

(copy-var* clojure-var sci-ns)
```
Function.

Copies Clojure var to SCI var. Runtime analog of compile time [`copy-var`](#sci.core/copy-var).
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L88-L103">Source</a></sub></p>

## <a name="sci.core/create-ns">`create-ns`</a>
``` clojure

(create-ns sym)
(create-ns sym meta)
```
Function.

Creates namespace object. Can be used in var metadata.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L296-L300">Source</a></sub></p>

## <a name="sci.core/enable-unrestricted-access!">`enable-unrestricted-access!`</a>
``` clojure

(enable-unrestricted-access!)
```
Function.

Calling this will enable
  - Altering core vars using [`alter-var-root`](#sci.core/alter-var-root)
  - In CLJS: [`set!`](#sci.core/set!) is able to set the value of any var.
  - In CLJS: instance method calls are not restricted to only `:classes`

  In the future, more unrestricted access may be added, so only use this when you're not using SCI as a sandbox.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L550-L559">Source</a></sub></p>

## <a name="sci.core/err">`err`</a>




SCI var that represents SCI's `clojure.core/*err*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L129-L129">Source</a></sub></p>

## <a name="sci.core/eval-form">`eval-form`</a>
``` clojure

(eval-form ctx form)
```
Function.

Evaluates form (as produced by [`parse-string`](#sci.core/parse-string) or [`parse-next`](#sci.core/parse-next)) in the
  context of `ctx` (as produced with [`init`](#sci.core/init)). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L343-L350">Source</a></sub></p>

## <a name="sci.core/eval-string">`eval-string`</a>
``` clojure

(eval-string s)
(eval-string s opts)
```
Function.

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
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L230-L253">Source</a></sub></p>

## <a name="sci.core/eval-string*">`eval-string*`</a>
``` clojure

(eval-string* ctx s)
```
Function.

Evaluates string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L275-L279">Source</a></sub></p>

## <a name="sci.core/eval-string+">`eval-string+`</a>
``` clojure

(eval-string+ ctx s)
(eval-string+ ctx s opts)
```
Function.

Evaluates string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).

  Options:
  *`:ns` - the namespace to start evaluation in (defaults to the value of `sci/ns`)

  Returns map with:
  * `:val` - the evaluated value
  * `:ns` - the namespace object
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L281-L294">Source</a></sub></p>

## <a name="sci.core/file">`file`</a>




SCI var that represents SCI's `clojure.core/*file*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L131-L131">Source</a></sub></p>

## <a name="sci.core/find-ns">`find-ns`</a>
``` clojure

(find-ns ctx ns-sym)
```
Function.

Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L539-L542">Source</a></sub></p>

## <a name="sci.core/fork">`fork`</a>
``` clojure

(fork ctx)
```
Function.

Forks a context (as produced with [`init`](#sci.core/init)) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L268-L273">Source</a></sub></p>

## <a name="sci.core/format-stacktrace">`format-stacktrace`</a>
``` clojure

(format-stacktrace stacktrace)
```
Function.

Returns a list of formatted stack trace elements as strings from stacktrace.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L357-L360">Source</a></sub></p>

## <a name="sci.core/future">`future`</a>
``` clojure

(future & body)
```
Macro.

Like clojure.core/future but also conveys sci bindings to the thread.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L181-L186">Source</a></sub></p>

## <a name="sci.core/get-column-number">`get-column-number`</a>
``` clojure

(get-column-number reader)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L320-L321">Source</a></sub></p>

## <a name="sci.core/get-line-number">`get-line-number`</a>
``` clojure

(get-line-number reader)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L317-L318">Source</a></sub></p>

## <a name="sci.core/in">`in`</a>




SCI var that represents SCI's `clojure.core/*in*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L127-L127">Source</a></sub></p>

## <a name="sci.core/init">`init`</a>
``` clojure

(init opts)
```
Function.

Creates an initial sci context from given options `opts`. The context
  can be used with [`eval-string*`](#sci.core/eval-string*). See [`eval-string`](#sci.core/eval-string) for available
  options. The internal organization of the context is implementation
  detail and may change in the future.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L255-L261">Source</a></sub></p>

## <a name="sci.core/intern">`intern`</a>
``` clojure

(intern ctx sci-ns name)
(intern ctx sci-ns name val)
```
Function.

Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L217-L228">Source</a></sub></p>

## <a name="sci.core/merge-opts">`merge-opts`</a>
``` clojure

(merge-opts ctx opts)
```
Function.

Updates a context with opts merged in and returns it.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L263-L266">Source</a></sub></p>

## <a name="sci.core/new-dynamic-var">`new-dynamic-var`</a>
``` clojure

(new-dynamic-var name)
(new-dynamic-var name init-val)
(new-dynamic-var name init-val meta)
```
Function.

Same as new-var but adds :dynamic true to meta.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L41-L46">Source</a></sub></p>

## <a name="sci.core/new-macro-var">`new-macro-var`</a>
``` clojure

(new-macro-var name init-val)
(new-macro-var name init-val meta)
```
Function.

Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L53-L60">Source</a></sub></p>

## <a name="sci.core/new-var">`new-var`</a>
``` clojure

(new-var name)
(new-var name init-val)
(new-var name init-val meta)
```
Function.

Returns a new sci var.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L34-L39">Source</a></sub></p>

## <a name="sci.core/normalize-meta">`normalize-meta`</a>
``` clojure

(normalize-meta m)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L394-L397">Source</a></sub></p>

## <a name="sci.core/ns">`ns`</a>




SCI var that represents SCI's `clojure.core/*ns*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L130-L130">Source</a></sub></p>

## <a name="sci.core/ns-name">`ns-name`</a>
``` clojure

(ns-name sci-ns)
```
Function.

Returns name of SCI ns as symbol.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L362-L365">Source</a></sub></p>

## <a name="sci.core/out">`out`</a>




SCI var that represents SCI's `clojure.core/*out*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L128-L128">Source</a></sub></p>

## <a name="sci.core/parse-next">`parse-next`</a>
``` clojure

(parse-next ctx reader)
(parse-next ctx reader opts)
```
Function.

Parses next form from reader
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L323-L331">Source</a></sub></p>

## <a name="sci.core/parse-next+string">`parse-next+string`</a>
``` clojure

(parse-next+string ctx reader)
(parse-next+string ctx reader opts)
```
Function.

Parses next form from reader
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L333-L341">Source</a></sub></p>

## <a name="sci.core/parse-string">`parse-string`</a>
``` clojure

(parse-string ctx s)
```
Function.

Parses string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L302-L306">Source</a></sub></p>

## <a name="sci.core/pmap">`pmap`</a>
``` clojure

(pmap f coll)
(pmap f coll & colls)
```
Function.

Like clojure.core/pmap but also conveys sci bindings to the threads.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L188-L205">Source</a></sub></p>

## <a name="sci.core/print-dup">`print-dup`</a>




SCI var that represents SCI's `clojure.core/*print-dup*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L137-L137">Source</a></sub></p>

## <a name="sci.core/print-err-fn">`print-err-fn`</a>




SCI var that represents SCI's `cljs.core/*print-err-fn*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L140-L140">Source</a></sub></p>

## <a name="sci.core/print-fn">`print-fn`</a>




SCI var that represents SCI's `cljs.core/*print-fn*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L139-L139">Source</a></sub></p>

## <a name="sci.core/print-length">`print-length`</a>




SCI var that represents SCI's `clojure.core/*print-length*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L133-L133">Source</a></sub></p>

## <a name="sci.core/print-level">`print-level`</a>




SCI var that represents SCI's `clojure.core/*print-level*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L134-L134">Source</a></sub></p>

## <a name="sci.core/print-meta">`print-meta`</a>




SCI var that represents SCI's `clojure.core/*print-meta*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L135-L135">Source</a></sub></p>

## <a name="sci.core/print-namespace-maps">`print-namespace-maps`</a>




SCI var that represents SCI's `clojure.core/*print-namespace-maps*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L138-L138">Source</a></sub></p>

## <a name="sci.core/print-newline">`print-newline`</a>




SCI var that represents SCI's `cljs.core/*print-newline*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L141-L141">Source</a></sub></p>

## <a name="sci.core/print-readably">`print-readably`</a>




SCI var that represents SCI's `clojure.core/*print-readably*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L136-L136">Source</a></sub></p>

## <a name="sci.core/read-eval">`read-eval`</a>




SCI var that represents SCI's `clojure.core/*read-eval*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L132-L132">Source</a></sub></p>

## <a name="sci.core/reader">`reader`</a>
``` clojure

(reader x)
```
Function.

Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L308-L312">Source</a></sub></p>

## <a name="sci.core/resolve">`resolve`</a>
``` clojure

(resolve ctx sym)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L569-L570">Source</a></sub></p>

## <a name="sci.core/set!">`set!`</a>
``` clojure

(set! dynamic-var v)
```
Function.

Establish thread local binding of dynamic var
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L48-L51">Source</a></sub></p>

## <a name="sci.core/source-reader">`source-reader`</a>
``` clojure

(source-reader x)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L314-L315">Source</a></sub></p>

## <a name="sci.core/stacktrace">`stacktrace`</a>
``` clojure

(stacktrace ex)
```
Function.

Returns list of stacktrace element maps from exception, if available.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L352-L355">Source</a></sub></p>

## <a name="sci.core/var->symbol">`var->symbol`</a>
``` clojure

(var->symbol sci-var)
```
Function.

Returns a fully qualified symbol from a `sci.lang.Var`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L561-L567">Source</a></sub></p>

## <a name="sci.core/with-bindings">`with-bindings`</a>
``` clojure

(with-bindings bindings-map & body)
```
Macro.

Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L106-L115">Source</a></sub></p>

## <a name="sci.core/with-in-str">`with-in-str`</a>
``` clojure

(with-in-str s & body)
```
Macro.

Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L152-L159">Source</a></sub></p>

## <a name="sci.core/with-out-str">`with-out-str`</a>
``` clojure

(with-out-str & body)
```
Macro.

Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L162-L178">Source</a></sub></p>

-----
# <a name="sci.ctx-store">sci.ctx-store</a>


Canonical place for projects to store, update and retrieve a context.
  This can be used by projects that need to expose their context to
  functions. SCI does not populate this dynamic var itself during
  evaluation. Projects like `sci.configs` assume this var to be set in
  some of their functions.




## <a name="sci.ctx-store/get-ctx">`get-ctx`</a>
``` clojure

(get-ctx)
```
Function.

Retrieve stored ctx or throw an exception.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L27-L33">Source</a></sub></p>

## <a name="sci.ctx-store/reset-ctx!">`reset-ctx!`</a>
``` clojure

(reset-ctx! ctx)
```
Function.

Store `ctx`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L15-L19">Source</a></sub></p>

## <a name="sci.ctx-store/swap-ctx!">`swap-ctx!`</a>
``` clojure

(swap-ctx! f & args)
```
Function.

Update `ctx` using `f` and `args`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L21-L25">Source</a></sub></p>

## <a name="sci.ctx-store/with-ctx">`with-ctx`</a>
``` clojure

(with-ctx ctx & body)
```
Macro.

Bind `ctx` during execution of body.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L35-L39">Source</a></sub></p>

-----
# <a name="sci.lang">sci.lang</a>






## <a name="sci.lang/namespace">`Namespace`</a>




Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L260-L280">Source</a></sub></p>

## <a name="sci.lang/type">`Type`</a>




Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L23-L70">Source</a></sub></p>

## <a name="sci.lang/var">`Var`</a>




Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L89-L252">Source</a></sub></p>

## <a name="sci.lang/notify-watches">`notify-watches`</a>
``` clojure

(notify-watches ref watches old-val new-val)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L79-L87">Source</a></sub></p>
