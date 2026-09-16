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
    -  [`disable-jit`](#sci.core/disable-jit)
    -  [`enable-unrestricted-access!`](#sci.core/enable-unrestricted-access!) - Removed.
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



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L201-L201">Source</a></sub></p>

## <a name="sci.core/*2">`*2`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L202-L202">Source</a></sub></p>

## <a name="sci.core/*3">`*3`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L203-L203">Source</a></sub></p>

## <a name="sci.core/*e">`*e`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L204-L204">Source</a></sub></p>

## <a name="sci.core/add-class!">`add-class!`</a>
``` clojure

(add-class! ctx class-name class)
```
Function.

Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L672-L682">Source</a></sub></p>

## <a name="sci.core/add-import!">`add-import!`</a>
``` clojure

(add-import! ctx ns-name class-name alias)
```
Function.

Adds import of class named by `class-name` (a symbol) to namespace named by [`ns-name`](#sci.core/ns-name) (a symbol) under alias `alias` (a symbol). Returns mutated context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L661-L670">Source</a></sub></p>

## <a name="sci.core/add-js-lib!">`add-js-lib!`</a>
``` clojure

(add-js-lib! ctx name-str js-lib)
```
Function.

Add js library to context, so it can be used with `require`.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L721-L725">Source</a></sub></p>

## <a name="sci.core/add-namespace!">`add-namespace!`</a>
``` clojure

(add-namespace! ctx ns-name ns-map)
```
Function.

Adds namespace map `ns-map` named by the symbol [`ns-name`](#sci.core/ns-name) to
  `ctx`. Returns mutated context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L684-L689">Source</a></sub></p>

## <a name="sci.core/all-ns">`all-ns`</a>
``` clojure

(all-ns ctx)
```
Function.

Returns all SCI ns objects in the `ctx`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L696-L700">Source</a></sub></p>

## <a name="sci.core/alter-var-root">`alter-var-root`</a>
``` clojure

(alter-var-root v f)
(alter-var-root v f & args)
```
Function.

Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L273-L281">Source</a></sub></p>

## <a name="sci.core/assert">`assert`</a>




SCI var that represents SCI's clojure.core/*assert*
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L199-L199">Source</a></sub></p>

## <a name="sci.core/binding">`binding`</a>
``` clojure

(binding bindings & body)
```
Macro.

Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L174-L181">Source</a></sub></p>

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
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L497-L659">Source</a></sub></p>

## <a name="sci.core/copy-var">`copy-var`</a>
``` clojure

(copy-var sym ns)
(copy-var sym ns opts)
```
Macro.

Copies contents from var `sym` to a new sci var. The value [`ns`](#sci.core/ns) is an
  object created with [`sci.core/create-ns`](#sci.core/create-ns).

  When `sym` names a protocol other than `cljs.core/IFn`,
  the sci var holds a protocol entry instead of the raw protocol object.
  Sci code can then implement the protocol on `deftype` and `defrecord`
  types, extend those with `extend-type` and use `satisfies?`. Host code
  calling protocol methods on such instances dispatches into the sci
  implementations.

  Options (ignored for protocols):

  - `:name`: The name of the copied var. Defaults to the original var name.
  - `:copy-meta-from`: A symbol resolving to a var whose metadata (`:doc`,
    `:arglists`, `:file`, `:line`, `:column`) is used instead of `sym`'s.
    Useful for wrapper vars that delegate to another var.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L77-L124">Source</a></sub></p>

## <a name="sci.core/copy-var*">`copy-var*`</a>
``` clojure

(copy-var* clojure-var sci-ns)
```
Function.

Copies Clojure var to SCI var. Runtime analog of compile time [`copy-var`](#sci.core/copy-var).
  Copies JVM protocols as protocol entries.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L126-L160">Source</a></sub></p>

## <a name="sci.core/create-ns">`create-ns`</a>
``` clojure

(create-ns sym)
(create-ns sym meta)
```
Function.

Creates namespace object. Can be used in var metadata.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L370-L374">Source</a></sub></p>

## <a name="sci.core/disable-jit">`disable-jit`</a>



<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L39-L39">Source</a></sub></p>

## <a name="sci.core/enable-unrestricted-access!">`enable-unrestricted-access!`</a>
``` clojure

(enable-unrestricted-access!)
```
Function.

Removed. Use the `:unrestricted` option of [`init`](#sci.core/init) or [`eval-string`](#sci.core/eval-string)
  instead. Throws when called.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L702-L707">Source</a></sub></p>

## <a name="sci.core/err">`err`</a>




SCI var that represents SCI's `clojure.core/*err*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L186-L186">Source</a></sub></p>

## <a name="sci.core/eval-form">`eval-form`</a>
``` clojure

(eval-form ctx form)
```
Function.

Evaluates form (as produced by [`parse-string`](#sci.core/parse-string) or [`parse-next`](#sci.core/parse-next)) in the
  context of `ctx` (as produced with [`init`](#sci.core/init)). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L417-L424">Source</a></sub></p>

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

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:namespaces` have to be explicitly
  enumerated.

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb}).

  - `:ns-aliases`: a map of aliases to namespaces that are globally valid, e.g. `{'clojure.test 'cljs.test}`

  - `:interrupt-fn`: a zero-arg fn called on every interpreted `fn` entry / `loop` entry

  - `:unrestricted`: when `true`, evaluated code may mutate built-in vars
  and CLJS instance interop skips `:classes` checks. Off by default.
  Applies only to this context: a context created during an unrestricted
  evaluation is sandboxed unless it also gets this option.

  - `:bindings`: DEPRECATED - `:bindings x` is the same as `:namespaces {'user x}`.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L296-L326">Source</a></sub></p>

## <a name="sci.core/eval-string*">`eval-string*`</a>
``` clojure

(eval-string* ctx s)
```
Function.

Evaluates string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L349-L353">Source</a></sub></p>

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
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L355-L368">Source</a></sub></p>

## <a name="sci.core/file">`file`</a>




SCI var that represents SCI's `clojure.core/*file*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L188-L188">Source</a></sub></p>

## <a name="sci.core/find-ns">`find-ns`</a>
``` clojure

(find-ns ctx ns-sym)
```
Function.

Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L691-L694">Source</a></sub></p>

## <a name="sci.core/fork">`fork`</a>
``` clojure

(fork ctx)
```
Function.

Forks a context (as produced with [`init`](#sci.core/init)) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L342-L347">Source</a></sub></p>

## <a name="sci.core/format-stacktrace">`format-stacktrace`</a>
``` clojure

(format-stacktrace stacktrace)
```
Function.

Returns a list of formatted stack trace elements as strings from stacktrace.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L431-L434">Source</a></sub></p>

## <a name="sci.core/future">`future`</a>
``` clojure

(future & body)
```
Macro.

Like clojure.core/future but also conveys sci bindings to the thread.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L247-L252">Source</a></sub></p>

## <a name="sci.core/get-column-number">`get-column-number`</a>
``` clojure

(get-column-number reader)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L394-L395">Source</a></sub></p>

## <a name="sci.core/get-line-number">`get-line-number`</a>
``` clojure

(get-line-number reader)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L391-L392">Source</a></sub></p>

## <a name="sci.core/in">`in`</a>




SCI var that represents SCI's `clojure.core/*in*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L184-L184">Source</a></sub></p>

## <a name="sci.core/init">`init`</a>
``` clojure

(init opts)
```
Function.

Creates an initial sci context from given options `opts`. The context
  can be used with [`eval-string*`](#sci.core/eval-string*). See [`eval-string`](#sci.core/eval-string) for available
  options. The internal organization of the context is implementation
  detail and may change in the future.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L328-L335">Source</a></sub></p>

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
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L283-L294">Source</a></sub></p>

## <a name="sci.core/merge-opts">`merge-opts`</a>
``` clojure

(merge-opts ctx opts)
```
Function.

Updates a context with opts merged in and returns it.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L337-L340">Source</a></sub></p>

## <a name="sci.core/new-dynamic-var">`new-dynamic-var`</a>
``` clojure

(new-dynamic-var name)
(new-dynamic-var name init-val)
(new-dynamic-var name init-val meta)
```
Function.

Same as new-var but adds :dynamic true to meta.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L51-L58">Source</a></sub></p>

## <a name="sci.core/new-macro-var">`new-macro-var`</a>
``` clojure

(new-macro-var name init-val)
(new-macro-var name init-val meta)
```
Function.

Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L65-L74">Source</a></sub></p>

## <a name="sci.core/new-var">`new-var`</a>
``` clojure

(new-var name)
(new-var name init-val)
(new-var name init-val meta)
```
Function.

Returns a new sci var.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L42-L49">Source</a></sub></p>

## <a name="sci.core/normalize-meta">`normalize-meta`</a>
``` clojure

(normalize-meta m)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L477-L480">Source</a></sub></p>

## <a name="sci.core/ns">`ns`</a>




SCI var that represents SCI's `clojure.core/*ns*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L187-L187">Source</a></sub></p>

## <a name="sci.core/ns-name">`ns-name`</a>
``` clojure

(ns-name sci-ns)
```
Function.

Returns name of SCI ns as symbol.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L436-L439">Source</a></sub></p>

## <a name="sci.core/out">`out`</a>




SCI var that represents SCI's `clojure.core/*out*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L185-L185">Source</a></sub></p>

## <a name="sci.core/parse-next">`parse-next`</a>
``` clojure

(parse-next ctx reader)
(parse-next ctx reader opts)
```
Function.

Parses next form from reader
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L397-L405">Source</a></sub></p>

## <a name="sci.core/parse-next+string">`parse-next+string`</a>
``` clojure

(parse-next+string ctx reader)
(parse-next+string ctx reader opts)
```
Function.

Parses next form from reader
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L407-L415">Source</a></sub></p>

## <a name="sci.core/parse-string">`parse-string`</a>
``` clojure

(parse-string ctx s)
```
Function.

Parses string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L376-L380">Source</a></sub></p>

## <a name="sci.core/pmap">`pmap`</a>
``` clojure

(pmap f coll)
(pmap f coll & colls)
```
Function.

Like clojure.core/pmap but also conveys sci bindings to the threads.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L254-L271">Source</a></sub></p>

## <a name="sci.core/print-dup">`print-dup`</a>




SCI var that represents SCI's `clojure.core/*print-dup*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L194-L194">Source</a></sub></p>

## <a name="sci.core/print-err-fn">`print-err-fn`</a>




SCI var that represents SCI's `cljs.core/*print-err-fn*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L197-L197">Source</a></sub></p>

## <a name="sci.core/print-fn">`print-fn`</a>




SCI var that represents SCI's `cljs.core/*print-fn*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L196-L196">Source</a></sub></p>

## <a name="sci.core/print-length">`print-length`</a>




SCI var that represents SCI's `clojure.core/*print-length*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L190-L190">Source</a></sub></p>

## <a name="sci.core/print-level">`print-level`</a>




SCI var that represents SCI's `clojure.core/*print-level*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L191-L191">Source</a></sub></p>

## <a name="sci.core/print-meta">`print-meta`</a>




SCI var that represents SCI's `clojure.core/*print-meta*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L192-L192">Source</a></sub></p>

## <a name="sci.core/print-namespace-maps">`print-namespace-maps`</a>




SCI var that represents SCI's `clojure.core/*print-namespace-maps*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L195-L195">Source</a></sub></p>

## <a name="sci.core/print-newline">`print-newline`</a>




SCI var that represents SCI's `cljs.core/*print-newline*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L198-L198">Source</a></sub></p>

## <a name="sci.core/print-readably">`print-readably`</a>




SCI var that represents SCI's `clojure.core/*print-readably*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L193-L193">Source</a></sub></p>

## <a name="sci.core/read-eval">`read-eval`</a>




SCI var that represents SCI's `clojure.core/*read-eval*`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L189-L189">Source</a></sub></p>

## <a name="sci.core/reader">`reader`</a>
``` clojure

(reader x)
```
Function.

Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L382-L386">Source</a></sub></p>

## <a name="sci.core/resolve">`resolve`</a>
``` clojure

(resolve ctx sym)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L717-L718">Source</a></sub></p>

## <a name="sci.core/set!">`set!`</a>
``` clojure

(set! dynamic-var v)
```
Function.

Establish thread local binding of dynamic var
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L60-L63">Source</a></sub></p>

## <a name="sci.core/source-reader">`source-reader`</a>
``` clojure

(source-reader x)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L388-L389">Source</a></sub></p>

## <a name="sci.core/stacktrace">`stacktrace`</a>
``` clojure

(stacktrace ex)
```
Function.

Returns list of stacktrace element maps from exception, if available.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L426-L429">Source</a></sub></p>

## <a name="sci.core/var->symbol">`var->symbol`</a>
``` clojure

(var->symbol sci-var)
```
Function.

Returns a fully qualified symbol from a `sci.lang.Var`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L709-L715">Source</a></sub></p>

## <a name="sci.core/with-bindings">`with-bindings`</a>
``` clojure

(with-bindings bindings-map & body)
```
Macro.

Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L163-L172">Source</a></sub></p>

## <a name="sci.core/with-in-str">`with-in-str`</a>
``` clojure

(with-in-str s & body)
```
Macro.

Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L209-L216">Source</a></sub></p>

## <a name="sci.core/with-out-str">`with-out-str`</a>
``` clojure

(with-out-str & body)
```
Macro.

Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L219-L241">Source</a></sub></p>

-----
# <a name="sci.ctx-store">sci.ctx-store</a>


Canonical place for projects to store, update and retrieve a context.
  This can be used by projects that need to expose their context to
  functions. SCI binds this dynamic var to the evaluating context during
  `eval-form`. Projects like `sci.configs` assume this var to be set in
  some of their functions.




## <a name="sci.ctx-store/get-ctx">`get-ctx`</a>
``` clojure

(get-ctx)
```
Function.

Retrieve stored ctx or throw an exception.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L29-L36">Source</a></sub></p>

## <a name="sci.ctx-store/reset-ctx!">`reset-ctx!`</a>
``` clojure

(reset-ctx! ctx)
```
Function.

Store `ctx`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L15-L20">Source</a></sub></p>

## <a name="sci.ctx-store/swap-ctx!">`swap-ctx!`</a>
``` clojure

(swap-ctx! f & args)
```
Function.

Update `ctx` using `f` and `args`
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L22-L27">Source</a></sub></p>

## <a name="sci.ctx-store/with-ctx">`with-ctx`</a>
``` clojure

(with-ctx ctx & body)
```
Macro.

Bind `ctx` during execution of body.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L38-L42">Source</a></sub></p>

-----
# <a name="sci.lang">sci.lang</a>






## <a name="sci.lang/namespace">`Namespace`</a>




Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L299-L324">Source</a></sub></p>

## <a name="sci.lang/type">`Type`</a>




Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L12-L48">Source</a></sub></p>

## <a name="sci.lang/var">`Var`</a>




Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L71-L290">Source</a></sub></p>

## <a name="sci.lang/notify-watches">`notify-watches`</a>
``` clojure

(notify-watches ref watches old-val new-val)
```
Function.
<p><sub><a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L61-L69">Source</a></sub></p>
