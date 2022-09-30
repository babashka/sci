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






## <a name="sci.async/await">`await`</a>
<a name="sci.async/await"></a>
``` clojure

(await promise)
```


Mark promise to be flatteded into top level async evaluation, similar
  to top level await.
<a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L123-L128"><sub>src</sub></a></img>

## <a name="sci.async/await?">`await?`</a>
<a name="sci.async/await?"></a>
``` clojure

(await? promise)
```


Check if promise was marked with [`await`](#sci.async/await).
<a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L130-L133"><sub>src</sub></a></img>

## <a name="sci.async/eval-string*">`eval-string*`</a>
<a name="sci.async/eval-string*"></a>
``` clojure

(eval-string* ctx s)
```


## <a name="sci.async/eval-string+">`eval-string+`</a>
<a name="sci.async/eval-string+"></a>
``` clojure

(eval-string+ ctx s)
(eval-string+ ctx s opts)
```


Same as eval-string* but returns map with `:val`, the evaluation
  result, and `:ns`, the last active namespace. The return value can
  be passed back into `opts` to preserve the namespace state.
<a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L109-L121"><sub>src</sub></a></img>

## <a name="sci.async/require">`require`</a>
<a name="sci.async/require"></a>

Async require that can be substituted for sync require by
  `{:namespaces {'clojure.core {'require scia/require}}}`
<a href="https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L141-L144"><sub>src</sub></a></img>

-----
# <a name="sci.core">sci.core</a>


The main SCI API namespace.




## <a name="sci.core/*1">`*1`</a>
<a name="sci.core/*1"></a>

## <a name="sci.core/*2">`*2`</a>
<a name="sci.core/*2"></a>

## <a name="sci.core/*3">`*3`</a>
<a name="sci.core/*3"></a>

## <a name="sci.core/*e">`*e`</a>
<a name="sci.core/*e"></a>

## <a name="sci.core/add-class!">`add-class!`</a>
<a name="sci.core/add-class!"></a>
``` clojure

(add-class! ctx class-name class)
```


Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L444-L454"><sub>src</sub></a></img>

## <a name="sci.core/add-import!">`add-import!`</a>
<a name="sci.core/add-import!"></a>
``` clojure

(add-import! ctx ns-name class-name alias)
```


Adds import of class named by `class-name` (a symbol) to namespace named by [`ns-name`](#sci.core/ns-name) (a symbol) under alias `alias` (a symbol). Returns mutated context.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L456-L461"><sub>src</sub></a></img>

## <a name="sci.core/add-namespace!">`add-namespace!`</a>
<a name="sci.core/add-namespace!"></a>
``` clojure

(add-namespace! ctx ns-name ns-map)
```


Adds namespace map `ns-map` named by the symbol [`ns-name`](#sci.core/ns-name) to
  `ctx`. Returns mutated context.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L463-L468"><sub>src</sub></a></img>

## <a name="sci.core/all-ns">`all-ns`</a>
<a name="sci.core/all-ns"></a>
``` clojure

(all-ns ctx)
```


Returns all SCI ns objects in the `ctx`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L475-L478"><sub>src</sub></a></img>

## <a name="sci.core/alter-var-root">`alter-var-root`</a>
<a name="sci.core/alter-var-root"></a>
``` clojure

(alter-var-root v f)
(alter-var-root v f & args)
```


Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L198-L204"><sub>src</sub></a></img>

## <a name="sci.core/assert">`assert`</a>
<a name="sci.core/assert"></a>

SCI var that represents SCI's clojure.core/*assert*
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L132-L132"><sub>src</sub></a></img>

## <a name="sci.core/binding">`binding`</a>
<a name="sci.core/binding"></a>
``` clojure

(binding bindings & body)
```


Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L108-L115"><sub>src</sub></a></img>

## <a name="sci.core/copy-ns">`copy-ns`</a>
<a name="sci.core/copy-ns"></a>
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
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L362-L442"><sub>src</sub></a></img>

## <a name="sci.core/copy-var">`copy-var`</a>
<a name="sci.core/copy-var"></a>
``` clojure

(copy-var sym ns)
(copy-var sym ns opts)
```


Macro.


Copies contents from var `sym` to a new sci var. The value [`ns`](#sci.core/ns) is an
  object created with [`sci.core/create-ns`](#sci.core/create-ns). If new-name is supplied, the
  copied var will be named new-name.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L56-L77"><sub>src</sub></a></img>

## <a name="sci.core/copy-var*">`copy-var*`</a>
<a name="sci.core/copy-var*"></a>
``` clojure

(copy-var* clojure-var sci-ns)
```


Copies Clojure var to SCI var. Runtime analog of compile time [`copy-var`](#sci.core/copy-var).
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L79-L94"><sub>src</sub></a></img>

## <a name="sci.core/create-ns">`create-ns`</a>
<a name="sci.core/create-ns"></a>
``` clojure

(create-ns sym)
(create-ns sym meta)
```


Creates namespace object. Can be used in var metadata.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L268-L272"><sub>src</sub></a></img>

## <a name="sci.core/enable-unrestricted-access!">`enable-unrestricted-access!`</a>
<a name="sci.core/enable-unrestricted-access!"></a>
``` clojure

(enable-unrestricted-access!)
```


Calling this will enable
  - Altering core vars using [`alter-var-root`](#sci.core/alter-var-root)
  - In CLJS: [`set!`](#sci.core/set!) is able to set the value of any var.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L480-L486"><sub>src</sub></a></img>

## <a name="sci.core/err">`err`</a>
<a name="sci.core/err"></a>

SCI var that represents SCI's `clojure.core/*err*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L120-L120"><sub>src</sub></a></img>

## <a name="sci.core/eval-form">`eval-form`</a>
<a name="sci.core/eval-form"></a>
``` clojure

(eval-form ctx form)
```


Evaluates form (as produced by [`parse-string`](#sci.core/parse-string) or [`parse-next`](#sci.core/parse-next)) in the
  context of `ctx` (as produced with [`init`](#sci.core/init)). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L302-L309"><sub>src</sub></a></img>

## <a name="sci.core/eval-string">`eval-string`</a>
<a name="sci.core/eval-string"></a>
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
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L217-L240"><sub>src</sub></a></img>

## <a name="sci.core/eval-string*">`eval-string*`</a>
<a name="sci.core/eval-string*"></a>
``` clojure

(eval-string* ctx s)
```


Evaluates string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L262-L266"><sub>src</sub></a></img>

## <a name="sci.core/file">`file`</a>
<a name="sci.core/file"></a>

SCI var that represents SCI's `clojure.core/*file*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L122-L122"><sub>src</sub></a></img>

## <a name="sci.core/find-ns">`find-ns`</a>
<a name="sci.core/find-ns"></a>
``` clojure

(find-ns ctx ns-sym)
```


Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L470-L473"><sub>src</sub></a></img>

## <a name="sci.core/fork">`fork`</a>
<a name="sci.core/fork"></a>
``` clojure

(fork ctx)
```


Forks a context (as produced with [`init`](#sci.core/init)) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L255-L260"><sub>src</sub></a></img>

## <a name="sci.core/format-stacktrace">`format-stacktrace`</a>
<a name="sci.core/format-stacktrace"></a>
``` clojure

(format-stacktrace stacktrace)
```


Returns a list of formatted stack trace elements as strings from stacktrace.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L316-L319"><sub>src</sub></a></img>

## <a name="sci.core/future">`future`</a>
<a name="sci.core/future"></a>
``` clojure

(future & body)
```


Macro.


Like clojure.core/future but also conveys sci bindings to the thread.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L172-L177"><sub>src</sub></a></img>

## <a name="sci.core/get-column-number">`get-column-number`</a>
<a name="sci.core/get-column-number"></a>
``` clojure

(get-column-number reader)
```


## <a name="sci.core/get-line-number">`get-line-number`</a>
<a name="sci.core/get-line-number"></a>
``` clojure

(get-line-number reader)
```


## <a name="sci.core/in">`in`</a>
<a name="sci.core/in"></a>

SCI var that represents SCI's `clojure.core/*in*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L118-L118"><sub>src</sub></a></img>

## <a name="sci.core/init">`init`</a>
<a name="sci.core/init"></a>
``` clojure

(init opts)
```


Creates an initial sci context from given options `opts`. The context
  can be used with [`eval-string*`](#sci.core/eval-string*). See [`eval-string`](#sci.core/eval-string) for available
  options. The internal organization of the context is implementation
  detail and may change in the future.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L242-L248"><sub>src</sub></a></img>

## <a name="sci.core/intern">`intern`</a>
<a name="sci.core/intern"></a>
``` clojure

(intern ctx sci-ns name)
(intern ctx sci-ns name val)
```


Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L206-L215"><sub>src</sub></a></img>

## <a name="sci.core/merge-opts">`merge-opts`</a>
<a name="sci.core/merge-opts"></a>
``` clojure

(merge-opts ctx opts)
```


Updates a context with opts merged in and returns it.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L250-L253"><sub>src</sub></a></img>

## <a name="sci.core/new-dynamic-var">`new-dynamic-var`</a>
<a name="sci.core/new-dynamic-var"></a>
``` clojure

(new-dynamic-var name)
(new-dynamic-var name init-val)
(new-dynamic-var name init-val meta)
```


Same as new-var but adds :dynamic true to meta.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L35-L40"><sub>src</sub></a></img>

## <a name="sci.core/new-macro-var">`new-macro-var`</a>
<a name="sci.core/new-macro-var"></a>
``` clojure

(new-macro-var name init-val)
(new-macro-var name init-val meta)
```


Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L47-L54"><sub>src</sub></a></img>

## <a name="sci.core/new-var">`new-var`</a>
<a name="sci.core/new-var"></a>
``` clojure

(new-var name)
(new-var name init-val)
(new-var name init-val meta)
```


Returns a new sci var.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L28-L33"><sub>src</sub></a></img>

## <a name="sci.core/ns">`ns`</a>
<a name="sci.core/ns"></a>

SCI var that represents SCI's `clojure.core/*ns*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L121-L121"><sub>src</sub></a></img>

## <a name="sci.core/ns-name">`ns-name`</a>
<a name="sci.core/ns-name"></a>
``` clojure

(ns-name sci-ns)
```


Returns name of SCI ns as symbol.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L321-L324"><sub>src</sub></a></img>

## <a name="sci.core/out">`out`</a>
<a name="sci.core/out"></a>

SCI var that represents SCI's `clojure.core/*out*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L119-L119"><sub>src</sub></a></img>

## <a name="sci.core/parse-next">`parse-next`</a>
<a name="sci.core/parse-next"></a>
``` clojure

(parse-next ctx reader)
(parse-next ctx reader opts)
```


Parses next form from reader
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L292-L300"><sub>src</sub></a></img>

## <a name="sci.core/parse-string">`parse-string`</a>
<a name="sci.core/parse-string"></a>
``` clojure

(parse-string ctx s)
```


Parses string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L274-L278"><sub>src</sub></a></img>

## <a name="sci.core/pmap">`pmap`</a>
<a name="sci.core/pmap"></a>
``` clojure

(pmap f coll)
(pmap f coll & colls)
```


Like clojure.core/pmap but also conveys sci bindings to the threads.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L179-L196"><sub>src</sub></a></img>

## <a name="sci.core/print-dup">`print-dup`</a>
<a name="sci.core/print-dup"></a>

SCI var that represents SCI's `clojure.core/*print-dup*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L128-L128"><sub>src</sub></a></img>

## <a name="sci.core/print-err-fn">`print-err-fn`</a>
<a name="sci.core/print-err-fn"></a>

SCI var that represents SCI's `cljs.core/*print-err-fn*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L130-L130"><sub>src</sub></a></img>

## <a name="sci.core/print-fn">`print-fn`</a>
<a name="sci.core/print-fn"></a>

SCI var that represents SCI's `cljs.core/*print-fn*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L129-L129"><sub>src</sub></a></img>

## <a name="sci.core/print-length">`print-length`</a>
<a name="sci.core/print-length"></a>

SCI var that represents SCI's `clojure.core/*print-length*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L124-L124"><sub>src</sub></a></img>

## <a name="sci.core/print-level">`print-level`</a>
<a name="sci.core/print-level"></a>

SCI var that represents SCI's `clojure.core/*print-level*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L125-L125"><sub>src</sub></a></img>

## <a name="sci.core/print-meta">`print-meta`</a>
<a name="sci.core/print-meta"></a>

SCI var that represents SCI's `clojure.core/*print-meta*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L126-L126"><sub>src</sub></a></img>

## <a name="sci.core/print-newline">`print-newline`</a>
<a name="sci.core/print-newline"></a>

SCI var that represents SCI's `cljs.core/*print-newline*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L131-L131"><sub>src</sub></a></img>

## <a name="sci.core/print-readably">`print-readably`</a>
<a name="sci.core/print-readably"></a>

SCI var that represents SCI's `clojure.core/*print-readably*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L127-L127"><sub>src</sub></a></img>

## <a name="sci.core/read-eval">`read-eval`</a>
<a name="sci.core/read-eval"></a>

SCI var that represents SCI's `clojure.core/*read-eval*`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L123-L123"><sub>src</sub></a></img>

## <a name="sci.core/reader">`reader`</a>
<a name="sci.core/reader"></a>
``` clojure

(reader x)
```


Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L280-L284"><sub>src</sub></a></img>

## <a name="sci.core/set!">`set!`</a>
<a name="sci.core/set!"></a>
``` clojure

(set! dynamic-var v)
```


Establish thread local binding of dynamic var
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L42-L45"><sub>src</sub></a></img>

## <a name="sci.core/stacktrace">`stacktrace`</a>
<a name="sci.core/stacktrace"></a>
``` clojure

(stacktrace ex)
```


Returns list of stacktrace element maps from exception, if available.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L311-L314"><sub>src</sub></a></img>

## <a name="sci.core/var->symbol">`var->symbol`</a>
<a name="sci.core/var->symbol"></a>
``` clojure

(var->symbol sci-var)
```


Returns a fully qualified symbol from a `sci.lang.Var`
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L488-L494"><sub>src</sub></a></img>

## <a name="sci.core/with-bindings">`with-bindings`</a>
<a name="sci.core/with-bindings"></a>
``` clojure

(with-bindings bindings-map & body)
```


Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L97-L106"><sub>src</sub></a></img>

## <a name="sci.core/with-in-str">`with-in-str`</a>
<a name="sci.core/with-in-str"></a>
``` clojure

(with-in-str s & body)
```


Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L143-L150"><sub>src</sub></a></img>

## <a name="sci.core/with-out-str">`with-out-str`</a>
<a name="sci.core/with-out-str"></a>
``` clojure

(with-out-str & body)
```


Macro.


Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.
<a href="https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L153-L169"><sub>src</sub></a></img>

-----
# <a name="sci.ctx-store">sci.ctx-store</a>


Canonical place for projects to store, update and retrieve a context.
  This can be used by projects that need to expose their context to
  functions. SCI does not populate this dynamic var itself during
  evaluation. Projects like `sci.configs` assume this var to be set in
  some of their functions.




## <a name="sci.ctx-store/get-ctx">`get-ctx`</a>
<a name="sci.ctx-store/get-ctx"></a>
``` clojure

(get-ctx)
```


Retrieve stored ctx or throw an exception.
<a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L27-L33"><sub>src</sub></a></img>

## <a name="sci.ctx-store/reset-ctx!">`reset-ctx!`</a>
<a name="sci.ctx-store/reset-ctx!"></a>
``` clojure

(reset-ctx! ctx)
```


Store `ctx`
<a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L15-L19"><sub>src</sub></a></img>

## <a name="sci.ctx-store/swap-ctx!">`swap-ctx!`</a>
<a name="sci.ctx-store/swap-ctx!"></a>
``` clojure

(swap-ctx! f & args)
```


Update `ctx` using `f` and `args`
<a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L21-L25"><sub>src</sub></a></img>

## <a name="sci.ctx-store/with-ctx">`with-ctx`</a>
<a name="sci.ctx-store/with-ctx"></a>
``` clojure

(with-ctx ctx & body)
```


Macro.


Bind `ctx` during execution of body.
<a href="https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L35-L39"><sub>src</sub></a></img>

-----

-----

-----
# <a name="sci.impl.cljs">sci.impl.cljs</a>






## <a name="sci.impl.cljs/cljs-ns-publics">`cljs-ns-publics`</a>
<a name="sci.impl.cljs/cljs-ns-publics"></a>

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






## <a name="sci.lang/Namespace">`Namespace`</a>
<a name="sci.lang/Namespace"></a>

Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly.
<a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L234-L254"><sub>src</sub></a></img>

## <a name="sci.lang/Type">`Type`</a>
<a name="sci.lang/Type"></a>

Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly.
<a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L23-L70"><sub>src</sub></a></img>

## <a name="sci.lang/Var">`Var`</a>
<a name="sci.lang/Var"></a>

Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly.
<a href="https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L79-L226"><sub>src</sub></a></img>
