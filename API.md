<details>


<summary><code> sci.async </code></summary>


## sci.async
<details>


<summary><code>eval-string*</code> </summary>


### `eval-string*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/async.cljs#L8-L8)
</details>


</details>


<hr>
<details>


<summary><code> sci.core </code></summary>


## sci.core
<details>


<summary><code>*1</code> </summary>


### `*1`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L113-L113)
</details>


<details>


<summary><code>*2</code> </summary>


### `*2`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L114-L114)
</details>


<details>


<summary><code>*3</code> </summary>


### `*3`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L115-L115)
</details>


<details>


<summary><code>*e</code> </summary>


### `*e`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L116-L116)
</details>


<details>


<summary><code>add-class!</code> - Adds class (JVM class or JS object) to <i>tx</i>as <i>lass-name</i>(a </summary>


### `add-class!`
> <code>[ctx class-name class]</code><br>

Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L440-L450)
</details>


<details>


<summary><code>add-import!</code> - Adds import of class named by <i>lass-name</i>(a symbol) to namespace named by <i>s- </summary>


### `add-import!`
> <code>[ctx ns-name class-name alias]</code><br>

Adds import of class named by `class-name` (a symbol) to namespace named by `ns-name` (a symbol) under alias `alias` (a symbol). Returns mutated context.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L452-L457)
</details>


<details>


<summary><code>all-ns</code> - Returns all SCI ns objects in the <i>tx` </summary>


### `all-ns`
> <code>[ctx]</code><br>

Returns all SCI ns objects in the `ctx`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L464-L467)
</details>


<details>


<summary><code>alter-var-root</code> - Atomically alters the root binding of sci var v by applying f to its </summary>


### `alter-var-root`
> <code>[v f]</code><br>
> <code>[v f & args]</code><br>

Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L177-L183)
</details>


<details>


<summary><code>assert</code> - SCI var that represents SCI's clojure.core/*assert* </summary>


### `assert`

SCI var that represents SCI's clojure.core/*assert*

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L111-L111)
</details>


<details>


<summary><code>binding</code> - Macro for binding sci vars </summary>


### `binding`
> <code>[bindings & body]</code><br>

Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L87-L94)
</details>


<details>


<summary><code>cljs-ns-publics</code> </summary>


### `cljs-ns-publics`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L341-L341)
</details>


<details>


<summary><code>copy-ns</code> - Returns map of names to SCI vars as a result of copying public </summary>


### `copy-ns`
> <code>[ns-sym sci-ns]</code><br>
> <code>[ns-sym sci-ns opts]</code><br>

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

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L358-L438)
</details>


<details>


<summary><code>copy-var</code> - Copies contents from var <i>ym</i>to a new sci var </summary>


### `copy-var`
> <code>[sym ns]</code><br>
> <code>[sym ns opts]</code><br>

Macro.


Copies contents from var `sym` to a new sci var. The value `ns` is an
  object created with `sci.core/create-ns`. If new-name is supplied, the 
  copied var will be named new-name.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L52-L73)
</details>


<details>


<summary><code>create-ns</code> - Creates namespace object </summary>


### `create-ns`
> <code>[sym]</code><br>
> <code>[sym meta]</code><br>

Creates namespace object. Can be used in var metadata.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L249-L253)
</details>


<details>


<summary><code>err</code> - SCI var that represents SCI's <i>lojure.core/*err*` </summary>


### `err`

SCI var that represents SCI's `clojure.core/*err*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L99-L99)
</details>


<details>


<summary><code>eval-form</code> - Evaluates form (as produced by <i>arse-string</i>or <i>arse-next</i> in the </summary>


### `eval-form`
> <code>[ctx form]</code><br>

Evaluates form (as produced by `parse-string` or `parse-next`) in the
  context of `ctx` (as produced with `init`). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L283-L290)
</details>


<details>


<summary><code>eval-string</code> - Evaluates string <i></i>as one or multiple Clojure expressions using the Small Cloj </summary>


### `eval-string`
> <code>[s]</code><br>
> <code>[s opts]</code><br>

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

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L196-L221)
</details>


<details>


<summary><code>eval-string*</code> - Evaluates string <i></i>in the context of <i>tx</i>(as produced with </summary>


### `eval-string*`
> <code>[ctx s]</code><br>

Evaluates string `s` in the context of `ctx` (as produced with
  `init`).

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L243-L247)
</details>


<details>


<summary><code>file</code> - SCI var that represents SCI's <i>lojure.core/*file*` </summary>


### `file`

SCI var that represents SCI's `clojure.core/*file*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L101-L101)
</details>


<details>


<summary><code>find-ns</code> - Returns SCI ns object as created with <i>ci/create-ns</i>from <i>tx</i>found by <i>s-sy </summary>


### `find-ns`
> <code>[ctx ns-sym]</code><br>

Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L459-L462)
</details>


<details>


<summary><code>fork</code> - Forks a context (as produced with <i>nit</i> into a new context </summary>


### `fork`
> <code>[ctx]</code><br>

Forks a context (as produced with `init`) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L236-L241)
</details>


<details>


<summary><code>format-stacktrace</code> - Returns a list of formatted stack trace elements as strings from stacktrace. </summary>


### `format-stacktrace`
> <code>[stacktrace]</code><br>

Returns a list of formatted stack trace elements as strings from stacktrace.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L297-L300)
</details>


<details>


<summary><code>future</code> - Like clojure.core/future but also conveys sci bindings to the thread. </summary>


### `future`
> <code>[& body]</code><br>

Macro.


Like clojure.core/future but also conveys sci bindings to the thread.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L151-L156)
</details>


<details>


<summary><code>get-column-number</code> </summary>


### `get-column-number`
> <code>[reader]</code><br>

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L270-L271)
</details>


<details>


<summary><code>get-line-number</code> </summary>


### `get-line-number`
> <code>[reader]</code><br>

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L267-L268)
</details>


<details>


<summary><code>in</code> - SCI var that represents SCI's <i>lojure.core/*in*` </summary>


### `in`

SCI var that represents SCI's `clojure.core/*in*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L97-L97)
</details>


<details>


<summary><code>init</code> - Creates an initial sci context from given options <i>pts` </summary>


### `init`
> <code>[opts]</code><br>

Creates an initial sci context from given options `opts`. The context
  can be used with `eval-string*`. See `eval-string` for available
  options. The internal organization of the context is implementation
  detail and may change in the future.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L223-L229)
</details>


<details>


<summary><code>intern</code> - Finds or creates a sci var named by the symbol name in the namespace </summary>


### `intern`
> <code>[ctx sci-ns name]</code><br>
> <code>[ctx sci-ns name val]</code><br>

Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L185-L194)
</details>


<details>


<summary><code>merge-opts</code> - Updates a context with opts merged in and returns it. </summary>


### `merge-opts`
> <code>[ctx opts]</code><br>

Updates a context with opts merged in and returns it.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L231-L234)
</details>


<details>


<summary><code>new-dynamic-var</code> - Same as new-var but adds :dynamic true to meta. </summary>


### `new-dynamic-var`
> <code>[name]</code><br>
> <code>[name init-val]</code><br>
> <code>[name init-val meta]</code><br>

Same as new-var but adds :dynamic true to meta.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L31-L36)
</details>


<details>


<summary><code>new-macro-var</code> - Same as new-var but adds :macro true to meta as well </summary>


### `new-macro-var`
> <code>[name init-val]</code><br>
> <code>[name init-val meta]</code><br>

Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L43-L50)
</details>


<details>


<summary><code>new-var</code> - Returns a new sci var. </summary>


### `new-var`
> <code>[name]</code><br>
> <code>[name init-val]</code><br>
> <code>[name init-val meta]</code><br>

Returns a new sci var.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L24-L29)
</details>


<details>


<summary><code>ns</code> - SCI var that represents SCI's <i>lojure.core/*ns*` </summary>


### `ns`

SCI var that represents SCI's `clojure.core/*ns*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L100-L100)
</details>


<details>


<summary><code>ns-name</code> - Returns name of SCI ns as symbol. </summary>


### `ns-name`
> <code>[sci-ns]</code><br>

Returns name of SCI ns as symbol.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L302-L305)
</details>


<details>


<summary><code>out</code> - SCI var that represents SCI's <i>lojure.core/*out*` </summary>


### `out`

SCI var that represents SCI's `clojure.core/*out*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L98-L98)
</details>


<details>


<summary><code>parse-next</code> - Parses next form from reader </summary>


### `parse-next`
> <code>[ctx reader]</code><br>
> <code>[ctx reader opts]</code><br>

Parses next form from reader

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L273-L281)
</details>


<details>


<summary><code>parse-string</code> - Parses string <i></i>in the context of <i>tx</i>(as produced with </summary>


### `parse-string`
> <code>[ctx s]</code><br>

Parses string `s` in the context of `ctx` (as produced with
  `init`).

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L255-L259)
</details>


<details>


<summary><code>pmap</code> - Like clojure.core/pmap but also conveys sci bindings to the threads. </summary>


### `pmap`
> <code>[f coll]</code><br>
> <code>[f coll & colls]</code><br>

Like clojure.core/pmap but also conveys sci bindings to the threads.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L158-L175)
</details>


<details>


<summary><code>print-dup</code> - SCI var that represents SCI's <i>lojure.core/*print-dup*` </summary>


### `print-dup`

SCI var that represents SCI's `clojure.core/*print-dup*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L107-L107)
</details>


<details>


<summary><code>print-err-fn</code> - SCI var that represents SCI's <i>ljs.core/*print-err-fn*` </summary>


### `print-err-fn`

SCI var that represents SCI's `cljs.core/*print-err-fn*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L109-L109)
</details>


<details>


<summary><code>print-fn</code> - SCI var that represents SCI's <i>ljs.core/*print-fn*` </summary>


### `print-fn`

SCI var that represents SCI's `cljs.core/*print-fn*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L108-L108)
</details>


<details>


<summary><code>print-length</code> - SCI var that represents SCI's <i>lojure.core/*print-length*` </summary>


### `print-length`

SCI var that represents SCI's `clojure.core/*print-length*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L103-L103)
</details>


<details>


<summary><code>print-level</code> - SCI var that represents SCI's <i>lojure.core/*print-level*` </summary>


### `print-level`

SCI var that represents SCI's `clojure.core/*print-level*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L104-L104)
</details>


<details>


<summary><code>print-meta</code> - SCI var that represents SCI's <i>lojure.core/*print-meta*` </summary>


### `print-meta`

SCI var that represents SCI's `clojure.core/*print-meta*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L105-L105)
</details>


<details>


<summary><code>print-newline</code> - SCI var that represents SCI's <i>ljs.core/*print-newline*` </summary>


### `print-newline`

SCI var that represents SCI's `cljs.core/*print-newline*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L110-L110)
</details>


<details>


<summary><code>print-readably</code> - SCI var that represents SCI's <i>lojure.core/*print-readably*` </summary>


### `print-readably`

SCI var that represents SCI's `clojure.core/*print-readably*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L106-L106)
</details>


<details>


<summary><code>read-eval</code> - SCI var that represents SCI's <i>lojure.core/*read-eval*` </summary>


### `read-eval`

SCI var that represents SCI's `clojure.core/*read-eval*`

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L102-L102)
</details>


<details>


<summary><code>reader</code> - Coerces x into indexing pushback-reader to be used with </summary>


### `reader`
> <code>[x]</code><br>

Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L261-L265)
</details>


<details>


<summary><code>set!</code> - Establish thread local binding of dynamic var </summary>


### `set!`
> <code>[dynamic-var v]</code><br>

Establish thread local binding of dynamic var

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L38-L41)
</details>


<details>


<summary><code>stacktrace</code> - Returns list of stacktrace element maps from exception, if available. </summary>


### `stacktrace`
> <code>[ex]</code><br>

Returns list of stacktrace element maps from exception, if available.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L292-L295)
</details>


<details>


<summary><code>with-bindings</code> - Macro for binding sci vars </summary>


### `with-bindings`
> <code>[bindings-map & body]</code><br>

Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L76-L85)
</details>


<details>


<summary><code>with-in-str</code> - Evaluates body in a context in which sci's *in* is bound to a fresh </summary>


### `with-in-str`
> <code>[s & body]</code><br>

Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L122-L129)
</details>


<details>


<summary><code>with-out-str</code> - Evaluates exprs in a context in which sci's *out* is bound to a fresh </summary>


### `with-out-str`
> <code>[& body]</code><br>

Macro.


Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.

[Source](https://github.com/babashka/sci/blob/master/src/sci/core.cljc#L132-L148)
</details>


</details>


<hr>
