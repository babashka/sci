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
-  [`sci.impl.bench`](#sci.impl.bench) 
    -  [`print-times`](#sci.impl.bench/print-times)
    -  [`record`](#sci.impl.bench/record)
    -  [`reset-times`](#sci.impl.bench/reset-times)
    -  [`times`](#sci.impl.bench/times)
-  [`sci.impl.copy-vars`](#sci.impl.copy-vars) 
    -  [`cljs-resolve`](#sci.impl.copy-vars/cljs-resolve)
    -  [`copy-core-var`](#sci.impl.copy-vars/copy-core-var)
    -  [`copy-var`](#sci.impl.copy-vars/copy-var)
    -  [`core-sym`](#sci.impl.copy-vars/core-sym)
    -  [`dequote`](#sci.impl.copy-vars/dequote)
    -  [`elide-vars`](#sci.impl.copy-vars/elide-vars)
    -  [`ensure-quote`](#sci.impl.copy-vars/ensure-quote)
    -  [`inlined-vars`](#sci.impl.copy-vars/inlined-vars)
    -  [`macrofy`](#sci.impl.copy-vars/macrofy)
    -  [`macrofy*`](#sci.impl.copy-vars/macrofy*)
    -  [`new-var`](#sci.impl.copy-vars/new-var)
    -  [`var-meta`](#sci.impl.copy-vars/var-meta)
-  [`sci.lang`](#sci.lang) 
    -  [`Namespace`](#sci.lang/Namespace) - Representation of a SCI namespace, created e.g.
    -  [`Type`](#sci.lang/Type) - Representation of a SCI custom type, created e.g.
    -  [`Var`](#sci.lang/Var) - Representation of a SCI var, created e.g.
    -  [`notify-watches`](#sci.lang/notify-watches)

-----
# <a name="sci.async">sci.async</a>






## <a name="sci.async/await">`await`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L124-L129)
<a name="sci.async/await"></a>
``` clojure

(await promise)
```


Mark promise to be flatteded into top level async evaluation, similar
  to top level await.

## <a name="sci.async/await?">`await?`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L131-L134)
<a name="sci.async/await?"></a>
``` clojure

(await? promise)
```


Check if promise was marked with [`await`](#sci.async/await).

## <a name="sci.async/eval-string*">`eval-string*`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L85-L108)
<a name="sci.async/eval-string*"></a>
``` clojure

(eval-string* ctx s)
```


## <a name="sci.async/eval-string+">`eval-string+`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L110-L122)
<a name="sci.async/eval-string+"></a>
``` clojure

(eval-string+ ctx s)
(eval-string+ ctx s opts)
```


Same as eval-string* but returns map with `:val`, the evaluation
  result, and `:ns`, the last active namespace. The return value can
  be passed back into `opts` to preserve the namespace state.

## <a name="sci.async/require">`require`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L142-L145)
<a name="sci.async/require"></a>

Async require that can be substituted for sync require by
  `{:namespaces {'clojure.core {'require scia/require}}}`

-----
# <a name="sci.core">sci.core</a>


The main SCI API namespace.




## <a name="sci.core/*1">`*1`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L140-L140)
<a name="sci.core/*1"></a>

## <a name="sci.core/*2">`*2`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L141-L141)
<a name="sci.core/*2"></a>

## <a name="sci.core/*3">`*3`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L142-L142)
<a name="sci.core/*3"></a>

## <a name="sci.core/*e">`*e`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L143-L143)
<a name="sci.core/*e"></a>

## <a name="sci.core/add-class!">`add-class!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L465-L475)
<a name="sci.core/add-class!"></a>
``` clojure

(add-class! ctx class-name class)
```


Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.

## <a name="sci.core/add-import!">`add-import!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L477-L482)
<a name="sci.core/add-import!"></a>
``` clojure

(add-import! ctx ns-name class-name alias)
```


Adds import of class named by `class-name` (a symbol) to namespace named by [`ns-name`](#sci.core/ns-name) (a symbol) under alias `alias` (a symbol). Returns mutated context.

## <a name="sci.core/add-namespace!">`add-namespace!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L484-L489)
<a name="sci.core/add-namespace!"></a>
``` clojure

(add-namespace! ctx ns-name ns-map)
```


Adds namespace map `ns-map` named by the symbol [`ns-name`](#sci.core/ns-name) to
  `ctx`. Returns mutated context.

## <a name="sci.core/all-ns">`all-ns`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L496-L499)
<a name="sci.core/all-ns"></a>
``` clojure

(all-ns ctx)
```


Returns all SCI ns objects in the `ctx`

## <a name="sci.core/alter-var-root">`alter-var-root`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L204-L212)
<a name="sci.core/alter-var-root"></a>
``` clojure

(alter-var-root v f)
(alter-var-root v f & args)
```


Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.

## <a name="sci.core/assert">`assert`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L138-L138)
<a name="sci.core/assert"></a>

SCI var that represents SCI's clojure.core/*assert*

## <a name="sci.core/binding">`binding`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L113-L120)
<a name="sci.core/binding"></a>
``` clojure

(binding bindings & body)
```


Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.

## <a name="sci.core/copy-ns">`copy-ns`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L383-L463)
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

## <a name="sci.core/copy-var">`copy-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L61-L82)
<a name="sci.core/copy-var"></a>
``` clojure

(copy-var sym ns)
(copy-var sym ns opts)
```


Macro.


Copies contents from var `sym` to a new sci var. The value [`ns`](#sci.core/ns) is an
  object created with [`sci.core/create-ns`](#sci.core/create-ns). If new-name is supplied, the
  copied var will be named new-name.

## <a name="sci.core/copy-var*">`copy-var*`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L84-L99)
<a name="sci.core/copy-var*"></a>
``` clojure

(copy-var* clojure-var sci-ns)
```


Copies Clojure var to SCI var. Runtime analog of compile time [`copy-var`](#sci.core/copy-var).

## <a name="sci.core/create-ns">`create-ns`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L276-L280)
<a name="sci.core/create-ns"></a>
``` clojure

(create-ns sym)
(create-ns sym meta)
```


Creates namespace object. Can be used in var metadata.

## <a name="sci.core/enable-unrestricted-access!">`enable-unrestricted-access!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L501-L510)
<a name="sci.core/enable-unrestricted-access!"></a>
``` clojure

(enable-unrestricted-access!)
```


Calling this will enable
  - Altering core vars using [`alter-var-root`](#sci.core/alter-var-root)
  - In CLJS: [`set!`](#sci.core/set!) is able to set the value of any var.
  - In CLJS: instance method calls are not restricted to only `:classes`

  In the future, more unrestricted access may be added, so only use this when you're not using SCI as a sandbox.

## <a name="sci.core/err">`err`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L125-L125)
<a name="sci.core/err"></a>

SCI var that represents SCI's `clojure.core/*err*`

## <a name="sci.core/eval-form">`eval-form`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L323-L330)
<a name="sci.core/eval-form"></a>
``` clojure

(eval-form ctx form)
```


Evaluates form (as produced by [`parse-string`](#sci.core/parse-string) or [`parse-next`](#sci.core/parse-next)) in the
  context of `ctx` (as produced with [`init`](#sci.core/init)). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`

## <a name="sci.core/eval-string">`eval-string`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L225-L248)
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

## <a name="sci.core/eval-string*">`eval-string*`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L270-L274)
<a name="sci.core/eval-string*"></a>
``` clojure

(eval-string* ctx s)
```


Evaluates string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).

## <a name="sci.core/file">`file`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L127-L127)
<a name="sci.core/file"></a>

SCI var that represents SCI's `clojure.core/*file*`

## <a name="sci.core/find-ns">`find-ns`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L491-L494)
<a name="sci.core/find-ns"></a>
``` clojure

(find-ns ctx ns-sym)
```


Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.

## <a name="sci.core/fork">`fork`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L263-L268)
<a name="sci.core/fork"></a>
``` clojure

(fork ctx)
```


Forks a context (as produced with [`init`](#sci.core/init)) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.

## <a name="sci.core/format-stacktrace">`format-stacktrace`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L337-L340)
<a name="sci.core/format-stacktrace"></a>
``` clojure

(format-stacktrace stacktrace)
```


Returns a list of formatted stack trace elements as strings from stacktrace.

## <a name="sci.core/future">`future`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L178-L183)
<a name="sci.core/future"></a>
``` clojure

(future & body)
```


Macro.


Like clojure.core/future but also conveys sci bindings to the thread.

## <a name="sci.core/get-column-number">`get-column-number`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L300-L301)
<a name="sci.core/get-column-number"></a>
``` clojure

(get-column-number reader)
```


## <a name="sci.core/get-line-number">`get-line-number`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L297-L298)
<a name="sci.core/get-line-number"></a>
``` clojure

(get-line-number reader)
```


## <a name="sci.core/in">`in`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L123-L123)
<a name="sci.core/in"></a>

SCI var that represents SCI's `clojure.core/*in*`

## <a name="sci.core/init">`init`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L250-L256)
<a name="sci.core/init"></a>
``` clojure

(init opts)
```


Creates an initial sci context from given options `opts`. The context
  can be used with [`eval-string*`](#sci.core/eval-string*). See [`eval-string`](#sci.core/eval-string) for available
  options. The internal organization of the context is implementation
  detail and may change in the future.

## <a name="sci.core/intern">`intern`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L214-L223)
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

## <a name="sci.core/merge-opts">`merge-opts`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L258-L261)
<a name="sci.core/merge-opts"></a>
``` clojure

(merge-opts ctx opts)
```


Updates a context with opts merged in and returns it.

## <a name="sci.core/new-dynamic-var">`new-dynamic-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L40-L45)
<a name="sci.core/new-dynamic-var"></a>
``` clojure

(new-dynamic-var name)
(new-dynamic-var name init-val)
(new-dynamic-var name init-val meta)
```


Same as new-var but adds :dynamic true to meta.

## <a name="sci.core/new-macro-var">`new-macro-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L52-L59)
<a name="sci.core/new-macro-var"></a>
``` clojure

(new-macro-var name init-val)
(new-macro-var name init-val meta)
```


Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.

## <a name="sci.core/new-var">`new-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L33-L38)
<a name="sci.core/new-var"></a>
``` clojure

(new-var name)
(new-var name init-val)
(new-var name init-val meta)
```


Returns a new sci var.

## <a name="sci.core/ns">`ns`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L126-L126)
<a name="sci.core/ns"></a>

SCI var that represents SCI's `clojure.core/*ns*`

## <a name="sci.core/ns-name">`ns-name`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L342-L345)
<a name="sci.core/ns-name"></a>
``` clojure

(ns-name sci-ns)
```


Returns name of SCI ns as symbol.

## <a name="sci.core/out">`out`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L124-L124)
<a name="sci.core/out"></a>

SCI var that represents SCI's `clojure.core/*out*`

## <a name="sci.core/parse-next">`parse-next`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L303-L311)
<a name="sci.core/parse-next"></a>
``` clojure

(parse-next ctx reader)
(parse-next ctx reader opts)
```


Parses next form from reader

## <a name="sci.core/parse-next+string">`parse-next+string`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L313-L321)
<a name="sci.core/parse-next+string"></a>
``` clojure

(parse-next+string ctx reader)
(parse-next+string ctx reader opts)
```


Parses next form from reader

## <a name="sci.core/parse-string">`parse-string`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L282-L286)
<a name="sci.core/parse-string"></a>
``` clojure

(parse-string ctx s)
```


Parses string `s` in the context of `ctx` (as produced with
  [`init`](#sci.core/init)).

## <a name="sci.core/pmap">`pmap`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L185-L202)
<a name="sci.core/pmap"></a>
``` clojure

(pmap f coll)
(pmap f coll & colls)
```


Like clojure.core/pmap but also conveys sci bindings to the threads.

## <a name="sci.core/print-dup">`print-dup`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L133-L133)
<a name="sci.core/print-dup"></a>

SCI var that represents SCI's `clojure.core/*print-dup*`

## <a name="sci.core/print-err-fn">`print-err-fn`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L136-L136)
<a name="sci.core/print-err-fn"></a>

SCI var that represents SCI's `cljs.core/*print-err-fn*`

## <a name="sci.core/print-fn">`print-fn`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L135-L135)
<a name="sci.core/print-fn"></a>

SCI var that represents SCI's `cljs.core/*print-fn*`

## <a name="sci.core/print-length">`print-length`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L129-L129)
<a name="sci.core/print-length"></a>

SCI var that represents SCI's `clojure.core/*print-length*`

## <a name="sci.core/print-level">`print-level`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L130-L130)
<a name="sci.core/print-level"></a>

SCI var that represents SCI's `clojure.core/*print-level*`

## <a name="sci.core/print-meta">`print-meta`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L131-L131)
<a name="sci.core/print-meta"></a>

SCI var that represents SCI's `clojure.core/*print-meta*`

## <a name="sci.core/print-namespace-maps">`print-namespace-maps`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L134-L134)
<a name="sci.core/print-namespace-maps"></a>

SCI var that represents SCI's `clojure.core/*print-namespace-maps*`

## <a name="sci.core/print-newline">`print-newline`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L137-L137)
<a name="sci.core/print-newline"></a>

SCI var that represents SCI's `cljs.core/*print-newline*`

## <a name="sci.core/print-readably">`print-readably`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L132-L132)
<a name="sci.core/print-readably"></a>

SCI var that represents SCI's `clojure.core/*print-readably*`

## <a name="sci.core/read-eval">`read-eval`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L128-L128)
<a name="sci.core/read-eval"></a>

SCI var that represents SCI's `clojure.core/*read-eval*`

## <a name="sci.core/reader">`reader`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L288-L292)
<a name="sci.core/reader"></a>
``` clojure

(reader x)
```


Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.

## <a name="sci.core/resolve">`resolve`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L520-L521)
<a name="sci.core/resolve"></a>
``` clojure

(resolve ctx sym)
```


## <a name="sci.core/set!">`set!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L47-L50)
<a name="sci.core/set!"></a>
``` clojure

(set! dynamic-var v)
```


Establish thread local binding of dynamic var

## <a name="sci.core/source-reader">`source-reader`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L294-L295)
<a name="sci.core/source-reader"></a>
``` clojure

(source-reader x)
```


## <a name="sci.core/stacktrace">`stacktrace`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L332-L335)
<a name="sci.core/stacktrace"></a>
``` clojure

(stacktrace ex)
```


Returns list of stacktrace element maps from exception, if available.

## <a name="sci.core/var->symbol">`var->symbol`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L512-L518)
<a name="sci.core/var->symbol"></a>
``` clojure

(var->symbol sci-var)
```


Returns a fully qualified symbol from a `sci.lang.Var`

## <a name="sci.core/with-bindings">`with-bindings`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L102-L111)
<a name="sci.core/with-bindings"></a>
``` clojure

(with-bindings bindings-map & body)
```


Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.

## <a name="sci.core/with-in-str">`with-in-str`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L149-L156)
<a name="sci.core/with-in-str"></a>
``` clojure

(with-in-str s & body)
```


Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.

## <a name="sci.core/with-out-str">`with-out-str`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L159-L175)
<a name="sci.core/with-out-str"></a>
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




## <a name="sci.ctx-store/get-ctx">`get-ctx`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L27-L33)
<a name="sci.ctx-store/get-ctx"></a>
``` clojure

(get-ctx)
```


Retrieve stored ctx or throw an exception.

## <a name="sci.ctx-store/reset-ctx!">`reset-ctx!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L15-L19)
<a name="sci.ctx-store/reset-ctx!"></a>
``` clojure

(reset-ctx! ctx)
```


Store `ctx`

## <a name="sci.ctx-store/swap-ctx!">`swap-ctx!`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L21-L25)
<a name="sci.ctx-store/swap-ctx!"></a>
``` clojure

(swap-ctx! f & args)
```


Update `ctx` using `f` and `args`

## <a name="sci.ctx-store/with-ctx">`with-ctx`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/ctx_store.cljc#L35-L39)
<a name="sci.ctx-store/with-ctx"></a>
``` clojure

(with-ctx ctx & body)
```


Macro.


Bind `ctx` during execution of body.

-----
# <a name="sci.impl.bench">sci.impl.bench</a>






## <a name="sci.impl.bench/print-times">`print-times`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/bench.cljc#L10-L12)
<a name="sci.impl.bench/print-times"></a>
``` clojure

(print-times)
```


## <a name="sci.impl.bench/record">`record`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/bench.cljc#L15-L21)
<a name="sci.impl.bench/record"></a>
``` clojure

(record k & body)
```


## <a name="sci.impl.bench/reset-times">`reset-times`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/bench.cljc#L7-L8)
<a name="sci.impl.bench/reset-times"></a>
``` clojure

(reset-times)
```


## <a name="sci.impl.bench/times">`times`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/bench.cljc#L5-L5)
<a name="sci.impl.bench/times"></a>

-----
# <a name="sci.impl.copy-vars">sci.impl.copy-vars</a>






## <a name="sci.impl.copy-vars/cljs-resolve">`cljs-resolve`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L17-L17)
<a name="sci.impl.copy-vars/cljs-resolve"></a>

## <a name="sci.impl.copy-vars/copy-core-var">`copy-core-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L116-L118)
<a name="sci.impl.copy-vars/copy-core-var"></a>
``` clojure

(copy-core-var sym)
```


## <a name="sci.impl.copy-vars/copy-var">`copy-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L89-L115)
<a name="sci.impl.copy-vars/copy-var"></a>
``` clojure

(copy-var sym ns & [opts])
```


## <a name="sci.impl.copy-vars/core-sym">`core-sym`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L35-L36)
<a name="sci.impl.copy-vars/core-sym"></a>
``` clojure

(core-sym sym)
```


## <a name="sci.impl.copy-vars/dequote">`dequote`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L30-L33)
<a name="sci.impl.copy-vars/dequote"></a>
``` clojure

(dequote x)
```


## <a name="sci.impl.copy-vars/elide-vars">`elide-vars`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L21-L21)
<a name="sci.impl.copy-vars/elide-vars"></a>

## <a name="sci.impl.copy-vars/ensure-quote">`ensure-quote`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L25-L28)
<a name="sci.impl.copy-vars/ensure-quote"></a>
``` clojure

(ensure-quote x)
```


## <a name="sci.impl.copy-vars/inlined-vars">`inlined-vars`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L14-L15)
<a name="sci.impl.copy-vars/inlined-vars"></a>

## <a name="sci.impl.copy-vars/macrofy">`macrofy`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L78-L80)
<a name="sci.impl.copy-vars/macrofy"></a>
``` clojure

(macrofy & args)
```


## <a name="sci.impl.copy-vars/macrofy*">`macrofy*`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L122-L135)
<a name="sci.impl.copy-vars/macrofy*"></a>
``` clojure

(macrofy* f)
(macrofy* sym f)
(macrofy* sym f ns)
(macrofy* sym f ns ctx?)
(macrofy* sym f ns ctx? extra-meta)
```


## <a name="sci.impl.copy-vars/new-var">`new-var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L137-L153)
<a name="sci.impl.copy-vars/new-var"></a>
``` clojure

(new-var sym f)
(new-var sym f ns)
(new-var sym f ns ctx?)
(new-var sym f ns ctx? extra-meta)
```


## <a name="sci.impl.copy-vars/var-meta">`var-meta`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/impl/copy_vars.cljc#L38-L76)
<a name="sci.impl.copy-vars/var-meta"></a>
``` clojure

(var-meta &env sym opts & _a)
```


-----
# <a name="sci.lang">sci.lang</a>






## <a name="sci.lang/Namespace">`Namespace`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L254-L274)
<a name="sci.lang/Namespace"></a>

Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly.

## <a name="sci.lang/Type">`Type`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L23-L70)
<a name="sci.lang/Type"></a>

Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly.

## <a name="sci.lang/Var">`Var`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L89-L246)
<a name="sci.lang/Var"></a>

Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly.

## <a name="sci.lang/notify-watches">`notify-watches`</a> [:page_facing_up:](https://github.com/babashka/sci/blob/master/src/sci/lang.cljc#L79-L87)
<a name="sci.lang/notify-watches"></a>
``` clojure

(notify-watches ref watches old-val new-val)
```

