<img src="logo/logo-300dpi.png" width="100px">

[![CircleCI](https://circleci.com/gh/babashka/sci/tree/master.svg?style=shield)](https://circleci.com/gh/babashka/sci/tree/master)
[![Clojars Project](https://img.shields.io/clojars/v/org.babashka/sci.svg)](https://clojars.org/org.babashka/sci)
[![Financial Contributors on Open Collective](https://opencollective.com/babashka/all/badge.svg?label=financial+contributors)](https://opencollective.com/babashka)
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://app.slack.com/client/T03RZGPFR/C015LCR9MHD)

**Small Clojure Interpreter**

<blockquote class="twitter-tweet" data-lang="en">
    <p lang="en" dir="ltr">I want a limited dialect of Clojure for a single-purpose, scripted application. SCI will fit nicely.</p>
    &mdash;
    <a href="https://twitter.com/tiagoluchini/status/1193144124142211073">@tiagoluchini</a>
</blockquote>

## Quickstart

### Use from Clojure(Script)

``` clojure
(require '[sci.core :as sci])
(sci/eval-string "(inc 1)") => ;; 2
(sci/eval-string "(inc x)" {:namespaces {'user {'x 2}}}) ;;=> 3
```

Try SCI in your browser at [NextJournal](https://nextjournal.github.io/clojure-mode/).

For usage with GraalVM `native-image` check [here](#graalvm).

## Why

You want to evaluate code from user input, or use Clojure for a DSL inside your
project, but `eval` isn't safe or simply doesn't work.

This library works with:

- Clojure on the JVM
- Clojure compiled with GraalVM native
- ClojureScript, even when compiled with `:advanced`, and JavaScript

## API docs

See [API.md](API.md).

## Projects using SCI

SCI is used in:

- [Babashka](https://github.com/babashka/babashka). A Clojure scripting tool that plays well with Bash.
- [nbb](https://github.com/babashka/nbb). Ad-hoc CLJS scripting on Node.js. (Node.js babashka)
- [scittle](https://github.com/babashka/scittle). Execute Clojure(Script) directly from browser script tags
- [Clerk](https://github.com/nextjournal/clerk). Local-First Notebooks for Clojure.
- [4ever-clojure](https://4clojure.oxal.org/). 4clojure as a static web page.
- [Clj-kondo](https://github.com/borkdude/clj-kondo/). A Clojure linter that sparks joy.
- [Jet](https://github.com/borkdude/jet). CLI to convert between JSON, EDN and Transit.
- [Joyride](https://github.com/BetterThanTomorrow/joyride). Making VS Code Hackable since 2022.
- [Portal](https://github.com/djblue/portal). A clojure tool to navigate through your data.
- [Zprint](https://github.com/kkinnear/zprint). Tool to beautifully format Clojure(script) code and data.
- [TryClojure.org](https://tryclojure.org/). Try Clojure!
- [SICMUtils](https://github.com/littleredcomputer/sicmutils). Computer Algebra System in Clojure, tailored for math and physics investigations.
- [Maria.cloud](https://2.maria.cloud/): a Clojure coding environment for beginners.
- [Datalevin](https://github.com/juji-io/datalevin): A simple, fast and versatile Datalog database
- [Overarch](https://github.com/soulspace-org/overarch): A data driven description of software architecture based on UML and the C4 model.
- [Logseq](https://logseq.com). A local-only outliner notebook which supports both Markdown and Org mode.
- [Malli](https://github.com/metosin/malli). Plain data Schemas for Clojure/Script.

<details>
<summary>Expand for more projects...</summary>

- [Bootleg](https://github.com/retrogradeorbit/bootleg). An HTML templating CLI.
- [Bytefield-svg](https://github.com/Deep-Symmetry/bytefield-svg). NodeJS library to generate byte field diagrams.
- [Cardigan Bay](https://github.com/interstar/cardigan-bay). Wiki engine in Clojure.
- [clj-browser-eval](https://github.com/NickCellino/clj-browser-eval). Turn any HTML input field into a Clojure interpreter.
- [ClojureBlocks](https://codeberg.org/jhandke/ClojureBlocks), A visual editor for Clojure
- [Chlorine](https://github.com/mauricioszabo/atom-chlorine). Socket-REPL and nREPL package for Atom editor.
- [Cq](https://github.com/markus-wa/cq). Clojure Command-line Data Processor for JSON, YAML, EDN, XML and more.
- [Dad](https://github.com/liquidz/dad). A configuration management tool.
- [For-science](https://github.com/pmonks/for-science). Discord bot.
- [Keycloak-clojure](https://github.com/jgrodziski/keycloak-clojure). Clojure library for Keycloak.
- [Lighthouse](https://github.com/barracudanetworks/lighthouse). A data-driven Kubernetes pre-processor.
- [Obsidian Wielder](https://github.com/victorb/obsidian-wielder). Write and evaluate Clojure code directly in your Obsidian vault.
- [PCP](https://github.com/alekcz/pcp). Clojure Processor (PHP replacement).
- [PGMig](https://github.com/leafclick/pgmig). Fast Standalone PostgreSQL Migration Runner.
- [Prose](https://github.com/JeremS/prose). Alternate syntax for Clojure, similar to what Pollen brings to Racket.
- [Spire](https://github.com/epiccastle/spire). Pragmatic provisioning using Clojure.
- [Tesserae](https://github.com/lumberdev/tesserae). A Clojure spreadsheet and more!

</details>

Are you using SCI in your company or projects? Let us know [here](https://github.com/babashka/sci/discussions/662).

## Installation

Use as a dependency:

[![Clojars Project](https://img.shields.io/clojars/v/org.babashka/sci.svg)](https://clojars.org/org.babashka/sci)

## Usage

The main API function is `sci.core/eval-string` which takes a string to evaluate
and an optional options map.

In SCI, `defn` does not mutate the outside world, only the evaluation
context inside a call to `sci/eval-string`.

By default SCI only enables access to most of the Clojure core functions. More
functions can be enabled by using `:namespaces` and `:classes`. Normally you
would use SCI's version of `println` but here, for the purposes of
demonstration, we use Clojure's version of `println` instead:

``` clojure
user=> (require '[sci.core :as sci])
user=> (sci/eval-string "(println \"hello\")" {:namespaces {'clojure.core {'println println}}})
hello
nil
```

It is also possible to provide namespaces which can be required inside a SCI program:

``` clojure
user=> (def opts {:namespaces {'foo.bar {'println println}}})
user=> (sci/eval-string "(require '[foo.bar :as lib]) (lib/println \"hello\")" opts)
hello
nil
```

You can provide a list of allowed symbols. Using other symbols causes an exception:

``` clojure
user=> (sci/eval-string "(inc 1)" {:allow '[inc]})
2
user=> (sci/eval-string "(dec 1)" {:allow '[inc]})
ExceptionInfo dec is not allowed! [at line 1, column 2]  clojure.core/ex-info (core.clj:4739)
```

Providing a list of disallowed symbols has the opposite effect:

``` clojure
user=> (sci/eval-string "(inc 1)" {:deny '[inc]})
ExceptionInfo inc is not allowed! [at line 1, column 2]  clojure.core/ex-info (core.clj:4739)
```

### Macros

Providing a macro as a binding can be done by providing a normal function that:
- has `:sci/macro` on the metadata set to `true`
- has two extra arguments at the start for `&form` and `&env`:

``` clojure
user=> (def do-twice ^:sci/macro (fn [_&form _&env x] (list 'do x x)))
user=> (sci/eval-string "(do-twice (f))" {:namespaces {'user {'do-twice do-twice 'f #(println "hello")}}})
hello
hello
nil
```

Alternatively you can refer to the macro from the Clojure environment via the
var (this only works in a JVM environment):

``` clojure
user=> (defmacro do-twice [x] (list 'do x x))
user=> (sci/eval-string "(do-twice (f))" {:namespaces {'user {'do-twice #'do-twice 'f #(println "hello")}}})
```

Tips:

* To get the name of the namespace the macro is called from at _expansion_ time, use `(str (deref sci.core/ns))`
* Have a look at [Emmy's sci-macro](https://github.com/mentat-collective/emmy/blob/e16b5692b04972f0bc9ea6d07f7ead41edccc066/src/emmy/util.cljc#L141), which behaves like `defmacro` in Clojure, but emits a `:sci/macro` defn with the extra args in cljs

### Vars

To remain safe and sandboxed, SCI programs do not have access to Clojure vars,
unless you explicitly provide that access. SCI has its own var type,
distinguished from Clojure vars.

In a SCI program these vars are created with `def` and `defn` just like in
normal Clojure:

``` clojure
(def x 1)
(defn foo [] x)
(foo) ;;=> 1
(def x 2)
(foo) ;;=> 2
```

Dynamic vars with thread-local bindings are also supported (for vars defined _inside_ your scripts, and thus _evaluated_ by SCI,
or for SCI dynamic vars (see below)):

``` clojure
(def ^:dynamic *x* 1)
(binding [*x* 10] *x*) ;;=> 10
(binding [*x* 10] (set! *x* 12) *x*) ;;=> 12
*x* ;;=> 1
```

Creating SCI vars _from Clojure_, to be exposed to your SCI scipts, can be done using `sci/new-var`:

``` clojure
(def x (sci/new-var 'x 10))
(sci/eval-string "(inc x)" {:namespaces {'user {'x x}}}) ;;=> 11
```

To create a dynamic SCI var from Clojure you can set metadata or use `sci/new-dynamic-var`:

``` clojure
(def x1 (sci/new-var 'x 10 {:dynamic true}))
(sci/eval-string "(binding [*x* 12] (inc *x*))" {:namespaces {'user {'*x* x1}}}) ;;=> 13
(def x2 (sci/new-dynamic-var 'x 10))
(sci/eval-string "(binding [*x* 12] (inc *x*))" {:namespaces {'user {'*x* x2}}}) ;;=> 13
```

These dynamic SCI vars can be bound from Clojure using `sci/binding`:

``` clojure
(def x (sci/new-dynamic-var 'x 10))
(sci/binding [x 11] (sci/eval-string "(inc *x*)" {:namespaces {'user {'*x* x}}})) ;;=> 12
```

Notice that you cannot set _host_ dynamic variables _from your SCI scripts_ - `binding` will only work
on dynamic variables you defined in the script itself, or on SCI dynamic variables exposed from Clojure.
This applies also to :sci/macros (which expand into more code in the scripts). There workaround is to
only bind them from Clojure, i.e. from functions exposed to and _called by_ your scripts:

``` clojure
(def ^:dynamic *x* 1)
(defn with-x [x-val f] (binding [*x* x-val] (f)))
(defn get-x [] *x*)
(def userns (sci/create-ns 'user))
(sci/eval-string "(with-x 42 #(get-x))"
                 {:namespaces {'user {'with-x (sci/copy-var with-x userns)
                                      'get-x (sci/copy-var get-x userns)}}})
;;=> 42
```

If you want to be bind the value from your script, then you can expose a SCI dynamic var to it,
and bind its value to the host dynamic var in Clojure:

```clj
(def ^:dynamic *x* 1)
(def userns (sci/create-ns 'user))
(def sci-x (sci/copy-var *x* userns)) ; ^:dynamic is copied too
(defn get-x [] (binding [*x* @sci-x] *x*)) ; bind host var to SCI dyn var value
(sci/eval-string "(binding [*x* 42] (get-x))"
                 {:namespaces {'user {'get-x (sci/copy-var get-x userns)
                                      '*x* sci-x}}}) ; expose SCI dyn var
;; => 42
```

#### Using `*in*`, `*out*`, `*err*`

The dynamic vars `*in*`, `*out*`, `*err*` in a SCI program correspond to the
dynamic SCI vars `sci/in`, `sci/out` and `sci/err` in the API. These
vars can be rebound as well:

``` clojure
(def sw (java.io.StringWriter.))
(sci/binding [sci/out sw] (sci/eval-string "(println \"hello\")")) ;;=> nil
(str sw) ;;=> "hello\n"
```

A shorthand for rebinding `sci/out` is `sci/with-out-str`:

``` clojure
(sci/with-out-str (sci/eval-string "(println \"hello\")")) ;;=> "hello\n"
```
### Reader conditionals

To tell SCI which branch of a [reader conditional](https://clojure.org/guides/reader_conditionals) to take,
such as the `:cljs` one in `#?(:clj "JVM" :cljs "JS")`, you need to tell it which _features_ it should support:

```clojure
(sci/eval-string "(str \"I'm \" #?(:clj \"JVM\" :cljs \"JS\"))" {:features #{:cljs :bb}}) ;;=> "I'm JS"
```

See [eval-string docs](https://github.com/babashka/sci/blob/master/API.md#sci.core/eval-string) for details.

### Copy a namespace

To copy the public vars of a Clojure namespace and to reify the Clojure vars into
corresponding SCI vars, you can use `ns-publics` in Clojure and the following API functions:

- [`sci/create-ns`](https://github.com/babashka/sci/blob/master/API.md#create-ns)
- [`sci/copy-var`](https://github.com/babashka/sci/blob/master/API.md#copy-var)
- [`sci/copy-var*`](https://github.com/babashka/sci/blob/master/API.md#copy-var-1)
- [`sci/copy-ns`](https://github.com/babashka/sci/blob/master/API.md#copy-ns)

E.g. given the following Clojure namespace:

``` clojure
(ns foobar)

(defmacro do-twice [x] (list 'do x x))

(defn times-two [x]
  (* x 2))

(defn silly-name [x] (* x x))
```

you can re-create that namespace in a SCI context like this:

``` clojure
(require 'foobar)

(def fns (sci/create-ns 'foobar-ns nil))

(def foobar-ns {'do-twice (sci/copy-var foobar/do-twice fns)
                'times-two (sci/copy-var foobar/times-two fns)
                'better-name (sci/copy-var foobar/silly-name fns {:name 'better-name})})

(def ctx (sci/init {:namespaces {'foobar foobar-ns}}))

(sci/binding [sci/out *out*]
  (sci/eval-string* ctx "(foobar/do-twice (prn :x))"))
:x
:x
nil

(sci/eval-string* ctx "(foobar/times-two 2)")
4

(sci/eval-string* ctx "(foobar/better-name 4)")
16
```

To copy an entire namespace without enumerating all vars explicitly with
`sci/copy-var` you can use the following approach using `ns-publics` and
`sci/copy-var*`, which works the same in Clojure and ClojureScript:

``` Clojure
(let [ens (sci/create-ns 'edamame.core)
      publics (ns-publics 'edamame.core)
      sci-ns (update-vals publics #(sci/copy-var* % ens))
      ctx (sci/init {:namespaces {'edamame.core sci-ns}})]
  (prn (sci/eval-string* ctx "(require '[edamame.core :as e]) (e/parse-string \"1\")"))
  ;;=> 1
  )
```

Because part of copying of the namespace could be done at compile time, which in
ClojureScript has the benefit that some vars are not part of the compiled output
and may result in smaller JS output, there is also the `sci/copy-ns` _macro_
which allows you to exclude vars at compile-time:

``` Clojure
(let [ens (sci/create-ns 'edamame.core)
      sci-ns (sci/copy-ns edamame.core ens {:exclude [iobj?]})
      ctx (sci/init {:namespaces {'edamame.core sci-ns}})]
  (prn (sci/eval-string* ctx "(require '[edamame.core :as e]) (e/parse-string \"1\")"))
  ;;=> 1
  )
```

### Stdout and stdin

#### Clojure

To enable printing to `stdout` and reading from `stdin` you can SCI-bind
`sci/out` and `sci/in` to `*out*` and `*in*` respectively:


``` clojure
(sci/binding [sci/out *out*
              sci/in *in*]
  (sci/eval-string "(print \"Type your name!\n> \")")
  (sci/eval-string "(flush)")
  (let [name (sci/eval-string "(read-line)")]
    (sci/eval-string "(printf \"Hello %s!\" name)
                      (flush)"
                     {:namespaces {'user {'name name}}})))
Type your name!
> Michiel
Hello Michiel!
```

When adding a Clojure function to SCI that interacts with `*out*` (or `*in*` or `*err*`), you
can hook it up to SCI's context. For example, a Clojure function that writes to `*out*`
can be Clojure bound to SCI's `out`:

```Clojure
user=> (defn foo [] (println "yello!"))
#'user/foo
user=> ;; without binding *out* to sci's out, the Clojure function will use its default *out*:
user=> (sci/eval-string "(with-out-str (foo))" {:namespaces {'user {'foo foo}}})
yello!
""
;; Let's hook foo up to SCI's context:
user=> (defn wrapped-foo [] (binding [*out* @sci/out] (foo)))
#'user/wrapped-foo
user=> (sci/eval-string "(with-out-str (foo))" {:namespaces {'user {'foo wrapped-foo}}})
"yello!\n"
```

To always enable printing in your SCI environment you can set `sci/out` and `sci/err` to `*out*` and `*err*` respectively, globally:

``` Clojure
(sci/alter-var-root sci/out (constantly *out*))
(sci/alter-var-root sci/err (constantly *err*))
```

#### ClojureScript

Similar to Clojure vs. CLJS, the difference with SCI on Clojure vs. SCI on CLJS
is that in the latter you should use `sci/print-newline` and `sci/print-fn` to
control printing to stdout:

``` Clojure
cljs.user=> (def output (atom ""))
#'cljs.user/output
cljs.user=> (sci/binding [sci/print-newline true sci/print-fn (fn [s] (swap! output str s))] (sci/eval-string "(print :hello) (println :bye)"))
nil
cljs.user=> @output
":hello:bye\n"
```

This is supported since SCI 0.2.7.

To always enable printing in your SCI environment you can set `sci/print-fn` to `*print-fn*` globally:

``` Clojure
(enable-console-print!)
(sci/alter-var-root sci/print-fn (constantly *print-fn*))
(sci/alter-var-root sci/print-err-fn (constantly *print-err-fn*))
```

If you are seeing the error _Attempting to call unbound fn: #'clojure.core/*print-fn*_ then this should fix it.

### Futures

Creating threads with `future` and `pmap` is disabled by default, but can be
enabled by requiring `sci.addons.future` and applying the `sci.addons.future/install` function
to the SCI options:

``` clojure
(ns my.sci.app
  (:require
   [sci.core :as sci]
   [sci.addons.future :as future]))

(sci/eval-string "@(future (inc x))"
                 (-> {:namespaces {'user {'x 1}}}
                     (future/install)))
;;=> 2
```

For conveying thread-local SCI bindings to an external `future` use
`sci/future`:

``` clojure
(ns my.sci.app
  (:require
   [sci.core :as sci]
   [sci.addons.future :as future]))

(def x (sci/new-dynamic-var 'x 10))

@(sci/binding [x 11]
   (sci/future
     (sci/eval-string "@(future (inc x))"
                      (-> {:namespaces {'user {'x x}}}
                          (future/install)))))
;;=> 12
```

### Classes

Adding support for classes is done via the `:classes` option:

``` clojure
(sci/eval-string "(java.util.UUID/randomUUID)"
  {:classes {'java.util.UUID java.util.UUID}})
;;=> #uuid "312ba519-37e2-4109-b164-97fb140b57b0"
```

To make this work with `GraalVM` you will also need to add an entry to your
[reflection
config](https://www.graalvm.org/latest/reference-manual/native-image/dynamic-features/Reflection/)
for this class. Also see [`reflection.json`](reflection.json).

By default, SCI only lets you interop with classes explicitly provided in the
`:classes` config. When a method call returns an instance of a class that is not
in `:classes` you won't be able to interop on that. You can disable this safety
measure with `{:classes {:allow :all}}`.

In JS hosts, to allow interop with anything, use the following config:

``` clojure
{:classes {'js js/globalThis :allow :all}}
```

Note that the value for`'js`, `js/globalThis` is just a JavaScript object. To
control in a more fine-grained manner what "classes" are available in a JS
environment, just limit the keys to the ones you would like to expose:

``` Clojure
{:classes {'js #js {:Promise js/Promise} :allow :all}}
```

The `:allow :all` option takes care that everything reachable via `:classes` is
allowed to be used, it does not mean that you have access to all classes in the
host environment.

### JavaScript libraries

Adding support for JavaScript libraries is done via the `:js-libs` option:

```clojure
(ns sci.examples.js-libs
  (:require ["fs" :as fs]
            [sci.core :as sci]))

(sci/eval-string "
(require '[\"fs\" :as fs])
(fs/existsSync \"README.md\")"
                 {:js-libs {"fs" fs}})
;;=> true
```

Note that JavaScript libraries _must_ be required using a string library name.

[Property notation](https://clojurescript.org/news/2021-04-06-release#_library_property_namespaces) is also supported:

``` clojure
(require '["fs$readFileSync" :as slurp])
(slurp "README.md" "utf-8")
```

JavaScript libraries can be added to an existing SCI context using `sci/add-js-lib!`.

### State

SCI uses a context (internally implemented using an atom) to keep track of state
changes like newly defined namespaces and vars. The contents of the context
should be considered implementation detail. Every call to `eval-string` creates
a fresh context. To preserve state over multiple evaluations, you can create a
context using the same options as those for `sci/eval-string`.

``` clojure
(def opts {:namespaces {'foo.bar {'x 1}}})
(def sci-ctx (sci/init opts))
```

The SCI context can then be re-used over successive invocations of
`sci/eval-string*`:

``` clojure
(sci/eval-string* sci-ctx "foo.bar/x") ;;=> 1
(sci/eval-string* sci-ctx "(ns foo.bar) (def x 2) x") ;;=> 2
(sci/eval-string* sci-ctx "foo.bar/x") ;;=> 2
```

In a multi-user environment it can be useful to give each user their own
context. This can already be achieved with `eval-string`, but for performance
reasons it may be desirable to initialize a shared context once. This shared
context can then be forked for each user so that changes in one user's context
aren't visible to other users:

``` clojure
(def forked (sci/fork sci-ctx))
(sci/eval-string* forked "(def forked 1)")
(sci/eval-string* forked "forked") ;;=> 1
(sci/eval-string* sci-ctx "forked") ;;=> Could not resolved symbol: forked
```

### Implementing require and load-file

SCI supports loading code via a hook that is invoked by SCI's implementation of
`require`. The job of this function is to find and return the source code for
the requested namespace. This passed-in function will be called with a single
argument that is a hashmap with a key `:namespace`. The value for this key will
be the _symbol_ of the requested namespace.

This function should return a map with keys `:file` (containing the filename to
be used in error messages) and `:source` (containing the source code text). SCI
will evaluate that source code to satisfy the call to `require`. Alternatively
the function can return `nil` which will result in SCI throwing an exception
that the namespace could not be found.

The load hook is passed as part of the SCI options via the `:load-fn`:

``` clojure
(defn load-fn [{:keys [namespace]}]
  (when (= namespace 'foo)
    {:file "foo.clj"
     :source "(ns foo) (def val :foo)"}))
(sci/eval-string "(require '[foo :as fu]) fu/val" {:load-fn load-fn})
;;=> :foo
```

Note that internally specified namespaces (either the default namespaces that
SCI provides itself or those provides via the `:namespaces` key) will be
considered first and if found there, `:load-fn` will not be called, unless
`:reload` or `:reload-all` are used:

``` clojure
(sci/eval-string
  "(require '[foo :as fu])
   fu/val"
  {:load-fn load-fn
   :namespaces {'foo {'val (sci/new-var 'val :internal)}}})
;;=> :internal

(sci/eval-string
  "(require '[foo :as fu] :reload)
   fu/val"
  {:load-fn load-fn
   :namespaces {'foo {'val (sci/new-var 'val :internal)}}})
;;=> :foo
```

Another option for loading code is to provide an implementation of
`clojure.core/load-file`. An example is presented here.

``` clojure
(ns my.sci.app
    (:require [sci.core :as sci]
              [clojure.java.io :as io]))

(spit "example1.clj" "(defn foo [] :foo)")
(spit "example2.clj" "(load-file \"example1.clj\")")

(let [load-file (fn [file]
                  (let [file (io/file file)
                        source (slurp file)]
                    (sci/with-bindings
                      {sci/ns @sci/ns
                       sci/file (.getAbsolutePath file)}
                      (sci/eval-string source opts))))
      opts {:namespaces {'clojure.core {'load-file load-file}}}]
  (sci/eval-string "(load-file \"example2.clj\") (foo)" opts))
;;=> :foo
```

## REPL

Implementing a REPL can be done using the following functions:

- `sci/reader`: returns reader for parsing source code, either from a string or `io/reader`
- `sci/parse-next`: returns next form from reader
- `sci/eval-form`: evaluates form returned by `parse-next`.

See [examples](examples/sci/examples) for examples for both Clojure and ClojureScript.
Run instructions are included at the bottom of each example.

To include an nREPL server in your sci-based project, you can use
[babashka.nrepl](https://github.com/babashka/babashka.nrepl).

## GraalVM

For general information about Clojure and GraalVM, check out
[clj-graal-docs](https://github.com/lread/clj-graal-docs) and
[graalvm-clojure](https://github.com/BrunoBonacci/graalvm-clojure/).

### Clojure version

To build native images with GraalVM it is recommended to use Clojure `1.10.3` or
later.

<!-- ## Use from JavaScript -->

<!-- Sci is available on NPM: -->

<!-- ``` shell -->
<!-- $ npm install @borkdude/sci -->
<!-- ``` -->

<!-- The JavaScript API consists of two functions, `evalString` to evaluate Clojure -->
<!-- expressions and `toJS` to convert Clojure data structures back to JavaScript. -->

<!-- ``` javascript -->
<!-- > const { evalString, toJS } = require('@borkdude/sci'); -->
<!-- > x = evalString("(assoc {:a 1} :b 2)") -->
<!-- > toJS(x) -->
<!-- { a: 1, b: 2 } -->
<!-- ``` -->

<!-- The function `evalString` takes an optional second argument to pass -->
<!-- options. Read [here](#Usage) how to use those options. Instead of symbols and -->
<!-- keywords it expects strings. Instead of kebab-case, use camelCase. -->

## Use as native shared library

To use SCI as a native shared library from e.g. C++, Rust, Python, read this [tutorial](doc/libsci.md).

## `eval` in ClojureScript

``` Clojure
(require '[sci.core :as sci])
(def ctx (sci/init {:classes {'js js/globalThis :allow :all}}))
(set! *eval* #(sci/eval-form ctx %))
(assoc {} :a (eval '(+ 1 2 3))) ;;=> {:a 6}
```

## Async evaluation in ClojureScript

See [doc/async.md](doc/async.md).

## Limitations

Currently SCI has limited support for `deftype` and does not support `definterface`.

### This-as

Currently SCI does not support `this-as` in JS hosts. As a workaround you can program in this style:

``` clojure
(def obj
  (let [;; construct the object:
        this #js {:text "foo"}
        ;; construct object functions:
        setText (fn [text] (set! (.-text this) text))
        getText (fn [] (.-text this))]
    ;; attach object functions:
    (set! (.-setText this) setText)
    (set! (.-getText this) getText)
    ;; return object:
    this))

(.setText obj "hello")
(prn (.getText obj)) ;; "hello"
```

<!-- A drop-in replacement that covers a subset of `this-as` usage might be implemented like this: -->

<!-- ``` Clojure -->
<!-- (defmacro ^:private new-var [] -->
<!--   `(def ~(gensym))) -->

<!-- (defmacro this-as [binding & body] -->
<!--   `(let [~binding (new-var) -->
<!--          obj# (do ~@body)] -->
<!--      (alter-var-root ~(list 'var binding) (constantly obj#)) -->
<!--      ;; remove var from namespace -->
<!--      (ns-unmap *ns* (:name (meta ~binding))) -->
<!--      obj#)) -->

<!-- (def obj -->
<!--   (this-as this -->
<!--     #js {:text "" -->
<!--          :setText (fn [t] (set! (.-text this) t)) -->
<!--          :getText (fn [] (.-text this))})) -->

<!-- (.setText obj "hello") -->
<!-- (prn (.getText obj)) ;; "hello" -->
<!-- ``` -->

## Laziness

Forms evaluated by SCI can produce lazy sequences. In Clojure, dynamic vars and
laziness can be a tricky combination and the same goes for dynamic SCI vars.

Consider the following example:

``` clojure
(let [sw     (java.io.StringWriter.)
      result (sci/binding [sci/out sw] (sci/eval-string "(map print (range 10))"))]
  (println "Output:" (str sw))
  (println "Result:" result))
```

If the returned lazy seq was realized within the `sci/binding` scope, the output
would be:

```
Output: 0123456789
Result: (nil nil nil nil nil nil nil nil nil nil)
```

But because the result is only printed outside of `sci/binding` the result is:

```
Execution error (ClassCastException) at sci.impl.io/pr-on (io.cljc:44).
class sci.impl.vars.SciUnbound cannot be cast to class java.io.Writer (sci.impl.vars.SciUnbound is in unnamed module of loader clojure.lang.DynamicClassLoader @4c2af006; java.io.Writer is in module java.base of loader 'bootstrap')
```

This happens because by the time the lazy-seq is realized, the binding scope for
`sci/out` is no longer established, and as a result the lazy-seq can no longer
be realized (due to the delayed calls to `println`, a side-effecting call
dependents on the value of `sci/out`, set by `sci/binding`.

If the result is intended to be serialized as a string, then one could simply
serialize while the binding is still in place:

``` clojure
(let [sw (java.io.StringWriter.)]
  (sci/binding [sci/out sw]
    (let [result (sci/eval-string "(map print (range 10))")]
      (println "Result:" result)
      (println "Output:" (str sw)))))
```

Note that we moved `(println "Result:" result)` before `(println "Output:" (str
sw))`, since the first call takes care of realization.

## Sci.configs

The [sci.configs](https://github.com/babashka/sci.configs) project contains
ready to be used SCI configs for several popular libraries.

## Test

Required: `lein`, the `clojure` CLI and GraalVM.

To successfully run the GraalVM tests, you will have to compile the binary first
with `script/compile`.

To run all tests:

    script/test/all

For running individual tests, see the scripts in `script/test`.

## Dev

### Benchmarking

Use `clojure -M:bench` to benchmark the various phases of sci on the JVM:

``` clojure
$ clojure -M:bench --complete --sexpr "(let [x 1 y 2] (+ x y))" --quick
BENCHMARKING EXPRESSION: (let [x 1 y 2] (+ x y))
PARSE:
-> (let [x 1 y 2] (+ x y))
Evaluation count : 1206396 in 6 samples of 201066 calls.
             Execution time mean : 528,740641 ns
    Execution time std-deviation : 35,961381 ns
   Execution time lower quantile : 495,686332 ns ( 2,5%)
   Execution time upper quantile : 580,676252 ns (97,5%)
                   Overhead used : 1,900699 ns
ANALYSIS:
Evaluation count : 98340 in 6 samples of 16390 calls.
             Execution time mean : 6,837491 µs
    Execution time std-deviation : 454,527892 ns
   Execution time lower quantile : 6,115323 µs ( 2,5%)
   Execution time upper quantile : 7,307643 µs (97,5%)
                   Overhead used : 1,900699 ns
EVALUATION:
-> 3
Evaluation count : 31674576 in 6 samples of 5279096 calls.
             Execution time mean : 16,949801 ns
    Execution time std-deviation : 0,182429 ns
   Execution time lower quantile : 16,796615 ns ( 2,5%)
   Execution time upper quantile : 17,161758 ns (97,5%)
```

Use `--parse`, `--evaluate` and/or `--analyze` to bench individual phases
(`--complete` will bench all of them). Leaving out `--quick` will run
`criterium/bench` instead of `criterium/quick-bench`.

#### GraalVM native-image

To benchmark an expression within GraalVM `native-image`, run `script/compile` and then run:

``` clojure
$ time ./sci "(loop [val 0 cnt 10000000] (if (pos? cnt) (recur (inc val) (dec cnt)) val))"
10000000
./sci    0.65s  user 0.02s system 99% cpu 0.669 total
```

## Troubleshooting

### Shadow-cljs + user.clj

When you require `sci.core` in `user.clj` in a shadow-cljs project, you might see problems with `sci.core/copy-var`.

This can be worked around by loading SCI like this in `user.clj`, instead of loading it in the `ns` form:

``` Clojure
(try (requiring-resolve 'cljs.analyzer.api/ns-resolve) (catch Exception _ nil))
(require '[sci.core :as sci])
```

## Thanks

- [Sponsors](https://github.com/sponsors/borkdude)
- [Contributors](https://github.com/babashka/SCI/graphs/contributors) and other users posting issues with bug reports and ideas

## License

Copyright © 2019-2022 Michiel Borkent

Distributed under the Eclipse Public License 1.0. This project contains code
from Clojure and ClojureScript which are also licensed under the EPL 1.0. See
LICENSE.
