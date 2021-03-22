<img src="logo/logo-300dpi.png" width="100px">

[![CircleCI](https://circleci.com/gh/borkdude/sci/tree/master.svg?style=shield)](https://circleci.com/gh/borkdude/sci/tree/master)
[![Clojars Project](https://img.shields.io/clojars/v/borkdude/sci.svg)](https://clojars.org/borkdude/sci)
[![Financial Contributors on Open Collective](https://opencollective.com/babashka/all/badge.svg?label=financial+contributors)](https://opencollective.com/babashka)
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://app.slack.com/client/T03RZGPFR/C015LCR9MHD)

**Small Clojure Interpreter**

<blockquote class="twitter-tweet" data-lang="en">
    <p lang="en" dir="ltr">I want a limited dialect of Clojure for a single-purpose, scripted application. Sci will fit nicely.</p>
    &mdash;
    <a href="https://twitter.com/tiagoluchini/status/1193144124142211073">@tiagoluchini</a>
</blockquote>

## Quickstart

### Use from Clojure(Script)

``` clojure
(require '[sci.core :as sci])
(sci/eval-string "(inc 1)") => ;; 2
(sci/eval-string "(inc x)" {:bindings {'x 2}}) ;;=> 3
```

Try sci in your browser at [NextJournal](https://nextjournal.github.io/clojure-mode/).

For usage with GraalVM `native-image` check [here](#graalvm).

## Why

You want to evaluate code from user input, or use Clojure for a DSL inside your
project, but `eval` isn't safe or simply doesn't work.

This library works with:

- Clojure on the JVM
- Clojure compiled with GraalVM native
- ClojureScript, even when compiled with `:advanced`, and (as a consequence) JavaScript

## Projects using sci

Sci is used in:

- [Babashka](https://github.com/babashka/babashka). A Clojure scripting tool that plays well with Bash.
- [Bootleg](https://github.com/retrogradeorbit/bootleg). An HTML templating CLI.
- [Bytefield-svg](https://github.com/Deep-Symmetry/bytefield-svg). NodeJS library to generate byte field diagrams.
- [Chlorine](https://github.com/mauricioszabo/atom-chlorine). Socket-REPL and nREPL package for Atom editor.
- [Clj-kondo](https://github.com/borkdude/clj-kondo/). A Clojure linter that sparks joy.
- [Closh](https://github.com/dundalek/closh). Bash-like shell based on Clojure. GraalVM port is work in progress.
- [Dad](https://github.com/liquidz/dad). A configuration management tool.
- [Datalevin](https://github.com/juji-io/datalevin). Durable Datalog database.
- [Firn](https://github.com/theiceshelf/firn). Org-mode static site generator.
- [Jet](https://github.com/borkdude/jet). CLI to convert between JSON, EDN and Transit.
- [Logseq](https://logseq.com). A local-only outliner notebook which supports both Markdown and Org mode.
- [Malli](https://github.com/metosin/malli). Plain data Schemas for Clojure/Script.
- [PCP](https://github.com/alekcz/pcp). Clojure Processor (PHP replacement).
- [PGMig](https://github.com/leafclick/pgmig). Fast Standalone PostgreSQL Migration Runner.
- [Prose](https://github.com/JeremS/prose). Alternate syntax for Clojure, similar to what Pollen brings to Racket.
- [SICMUtils](https://github.com/littleredcomputer/sicmutils). Computer Algebra System in Clojure, tailored for math and physics investigations.
- [Spire](https://github.com/epiccastle/spire). Pragmatic provisioning using Clojure.
- [Zprint](https://github.com/kkinnear/zprint). Tool to beautifully format Clojure(script) code and data.

Are you using sci in your company or projects? Let us know [here](https://github.com/babashka/babashka/issues/254).

## Status

Experimental. Breaking changes are expected to happen at this phase. They will
be documented in the [CHANGELOG.md](CHANGELOG.md).

## Installation

Use as a dependency:

[![Clojars Project](https://img.shields.io/clojars/v/borkdude/sci.svg)](https://clojars.org/borkdude/sci)

## API docs

For Clojure, see the generated [codox](https://borkdude.github.io/sci/doc/codox)
documentation.

## Usage

The main API function is `sci.core/eval-string` which takes a string to evaluate
and an optional options map.

In `sci`, `defn` does not mutate the outside world, only the evaluation
context inside a call to `sci/eval-string`.

By default `sci` only enables access to most of the Clojure core functions.
More functions can be enabled, at your own risk, by using `:bindings`.  Normally
you would use sci's version of `println` but here, for the purposes of
demonstration, we use use Clojure's version of `println` instead:

``` clojure
user=> (require '[sci.core :as sci])
user=> (sci/eval-string "(println \"hello\")" {:bindings {'println println}})
hello
nil
```

It is also possible to provide namespaces which can be required:

``` clojure
user=> (def opts {:namespaces {'foo.bar {'println println}}})
user=> (sci/eval-string "(require '[foo.bar :as lib]) (lib/println \"hello\")" opts)
hello
nil
```

In fact `{:bindings ...}` is just shorthand for `{:namespaces {'user ...}}`.

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

Providing a macro as a binding can be done by providing a normal function that:
- has `:sci/macro` on the metadata set to `true`
- has two extra arguments at the start for `&form` and `&env`:

``` clojure
user=> (def do-twice ^:sci/macro (fn [_&form _&env x] (list 'do x x)))
user=> (sci/eval-string "(do-twice (f))" {:bindings {'do-twice do-twice 'f #(println "hello")}})
hello
hello
nil
```

### Vars

To remain safe and sandboxed, sci evaluated Clojure does not have access to Clojure runtime vars.
Sci has its own var type, distinguished from Clojure vars.

In a sci program these vars are created with `def` and `defn` just like in normal Clojure:

``` clojure
(def x 1)
(defn foo [] x)
(foo) ;;=> 1
(def x 2)
(foo) ;;=> 2
```

Dynamic vars with thread-local bindings are also supported:

``` clojure
(def ^:dynamic *x* 1)
(binding [*x* 10] x) ;;=> 10
(binding [*x* 10] (set! x 12) x) ;;=> 12
x ;;=> 1
```

Pre-creating vars that can be used in a sci program can be done using
`sci/new-var`:

``` clojure
(def x (sci/new-var 'x 10))
(sci/eval-string "(inc x)" {:bindings {'x x}}) ;;=> 11
```

To create a dynamic sci var you can set metadata or use `sci/new-dynamic-var`:

``` clojure
(require '[sci.core] :as sci)
(def x1 (sci/new-var 'x 10 {:dynamic true}))
(sci/eval-string "(binding [*x* 12] (inc *x*))" {:bindings {'*x* x1}}) ;;=> 13
(def x2 (sci/new-dynamic-var 'x 10))
(sci/eval-string "(binding [*x* 12] (inc *x*))" {:bindings {'*x* x2}}) ;;=> 13
```

Pre-created sci vars can also be externally rebound:

``` clojure
(def x (sci/new-dynamic-var 'x 10))
(sci/binding [x 11] (sci/eval-string "(inc *x*)" {:bindings {'*x* x2}})) ;;=> 11
```

The dynamic vars `*in*`, `*out*`, `*err*` in a sci program correspond to the
dynamic sci vars `sci.core/in`, `sci.core/out` and `sci.core/err` in the API. These
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

### Stdout and stdin

To enable printing to `stdout` and reading from `stdin` you can sci bind
`sci.core/out` and `sci.core/in` to `*out*` and `*in*` respectively:


``` clojure
(sci/binding [sci/out *out*
              sci/in *in*]
  (sci/eval-string "(print \"Type your name!\n> \")")
  (sci/eval-string "(flush)")
  (let [name (sci/eval-string "(read-line)")]
    (sci/eval-string "(printf \"Hello %s!\" name)
                      (flush)"
                     {:bindings {'name name}})))
Type your name!
> Michiel
Hello Michiel!
```

When adding a Clojure function to sci that interacts with `*out*` (or `*in*` or `*err*`), you
can hook it up to sci's world. For example, a Clojure function that writes to `*out*`
can be Clojure bound to sci's `out`:

```Clojure
user=> (defn foo [] (println "yello!"))
#'user/foo
user=> ;; without binding *out* to sci's out, the Clojure function will use its default *out*:
user=> (sci/eval-string "(with-out-str (foo))" {:bindings {'foo foo}})
yello!
""
;; let's hook foo up to sci's world:
user=> (defn wrapped-foo [] (binding [*out* @sci/out] (foo)))
#'user/wrapped-foo
user=> (sci/eval-string "(with-out-str (foo))" {:bindings {'foo wrapped-foo}})
"yello!\n"
```

### Futures

Creating threads with `future` and `pmap` is disabled by default, but can be
enabled by requiring `sci.addons.future` and applying the `sci.addons.future/install` function
to the sci options:

``` clojure
(ns my.sci.app
  (:require
   [sci.core :as sci]
   [sci.addons.future :as future]))

(sci/eval-string "@(future (inc x))"
                 (-> {:bindings {'x 1}}
                     (future/install)))
;;=> 2
```

For conveying thread-local sci bindings to an external `future` use
`sci.core/future`:

``` clojure
(ns my.sci.app
  (:require
   [sci.core :as sci]
   [sci.addons.future :as future]))

(def x (sci/new-dynamic-var 'x 10))

@(sci/binding [x 11]
   (sci/future
     (sci/eval-string "@(future (inc x))"
                      (-> {:bindings {'x @x}}
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
config](https://github.com/oracle/graal/blob/master/substratevm/REFLECTION.md)
for this class. Also see [`reflection.json`](reflection.json).

### State

Sci uses an atom to keep track of state changes like newly defined namespaces
and vars. You can carry this state over from one call to another by providing
the atom yourself as the value for the `:env` key:

``` clojure
(def env (atom {})
(sci/eval-string "(defn foo [] :foo)" {:env env})
(sci/eval-string "(foo)" {:env env}) ;;=> :foo
```

The contents of the the `:env` atom should be considered implementation detail.

Using an `:env` atom you are allowed to change options at each invocation of
`eval-string`. If your use case doesn't require this, the recommendation is to
use a sci context instead.

A sci context is derived once from options as documented in
`sci.core/eval-string` and contains the runtime state of a sci session.

``` clojure
(def opts {:namespaces {'foo.bar {'x 1}}})
(def sci-ctx (sci/init opts))
```

Once created, a sci context should be considered final and should not be mutated
by the user. The contents of the sci context should be considered implementation
detail.

The sci context can be re-used over successive invocations of
`sci.core/eval-string*`.

The major difference between `eval-string` and `eval-string*` is that
`eval-string` will call `init` on the passed options and will pass that through
to `eval-string*`. When you create a sci context yourself, you can skip the
extra work that `eval-string` does and work directly with `eval-string*`.

``` clojure
(sci/eval-string* sci-ctx "foo.bar/x") ;;=> 1
(sci/eval-string* sci-ctx "(ns foo.bar) (def x 2) x") ;;=> 2
(sci/eval-string* sci-ctx "foo.bar/x") ;;=> 2
```

In a multi-user environment it can be useful to give each user their own
context. This can already be achieved with `eval-string`, but for performance
reasons it may be desirable to initialize a shared context. This shared context
can then be forked for each user so that changes in one user's context
aren't visible to other users:

``` clojure
(def forked (sci/fork sci-ctx))
(sci/eval-string* forked "(def forked 1)")
(sci/eval-string* forked "forked") ;;=> 1
(sci/eval-string* sci-ctx "forked") ;;=> Could not resolved symbol: forked
```

### Implementing require and load-file

Sci supports implementation of code loading via a function hook that is invoked
by sci's internal implementation of `require`. The job of this function is to
find and return the source code for the requested namespace. This passed-in
function will be called with a single argument that is a hashmap with a key
`:namespace`. The value for this key will be the _symbol_ of the requested
namespace.

This function can return a hashmap with the keys `:file` (containing the
filename to be used in error messages) and `:source` (containing the source code
text) and sci will evaluate that source code to satisfy the
require. Alternatively the function can return `nil` which will result in sci
throwing an exception that the namespace can not be found.

This custom function is passed into the sci context under the `:load-fn` key as
shown below.

``` clojure
(defn load-fn [{:keys [namespace]}]
  (when (= namespace 'foo)
    {:file "foo.clj"
     :source "(ns foo) (def val :foo)"}))
(sci/eval-string "(require '[foo :as fu]) fu/val" {:load-fn load-fn})
;;=> :foo
```

Note that internally specified namespaces (either those within sci itself or
those mounted under the `:namespaces` context setting) will be utilised first
and load-fn will not be called in those cases, unless `:reload` or `:reload-all`
are used:

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

(let [env (atom {})
      opts {:env env}
      load-file (fn [file]
                  (let [file (io/file file)
                        source (slurp file)]
                    (sci/with-bindings
                      {sci/ns @sci/ns
                       sci/file (.getAbsolutePath file)}
                      (sci/eval-string source opts))))
      opts (assoc-in opts [:namespaces 'clojure.core 'load-file] load-file)]
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

## Location metadata

Sci includes location metadata on forms that can carry it:

```
(sci/eval-string "(meta [1 2 3])")
;;=> {:line 1, :column 7, :end-line 1, :end-column 14}
```

This metadata is used by sci for error reporting.

## GraalVM

For general information about Clojure and GraalVM, check out
[clj-graal-docs](https://github.com/lread/clj-graal-docs) and
[graalvm-clojure](https://github.com/BrunoBonacci/graalvm-clojure/).

### Clojure version

To build native images with GraalVM it is recommended to use clojure
`1.10.2-alpha4` or later.

### Java 11

To use sci with GraalVM java11 override the dependency
`[borkdude/sci.impl.reflector "0.0.1"]` to `[borkdude/sci.impl.reflector
"0.0.1-java11]` in your `project.clj` or `deps.edn`.

Also you'll likely need a fix for `clojure.lang.Reflector`:

See
[clj-graal-docs](https://github.com/lread/clj-graal-docs#jdk11-and-clojurelangreflector)
and
[clj-reflector-graal-java11-fix](https://github.com/borkdude/clj-reflector-graal-java11-fix).

### Random numbers

For GraalVM < 20.3.0:

To make the `rand-*` functions behave well when compiling to a GraalVM native binary, use this setting:

``` clojure
--initialize-at-run-time=java.lang.Math\$RandomNumberGeneratorHolder
```


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

To use sci as a native shared library from e.g. C, C++, Rust, read this [tutorial](doc/libsci.md).

## Limitations

Currently sci doesn't support `deftype` and `definterface`.

## Test

Required: `lein`, the `clojure` CLI and GraalVM.

To succesfully run the GraalVM tests, you will have to compile the binary first
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
Evaluation count : 605268 in 6 samples of 100878 calls.
             Execution time mean : 1,105801 µs
    Execution time std-deviation : 209,640200 ns
   Execution time lower quantile : 934,602619 ns ( 2,5%)
   Execution time upper quantile : 1,459172 µs (97,5%)
                   Overhead used : 8,140922 ns

Found 1 outliers in 6 samples (16,6667 %)
	low-severe	 1 (16,6667 %)
 Variance from outliers : 48,1886 % Variance is moderately inflated by outliers
ANALYSIS:
Evaluation count : 82248 in 6 samples of 13708 calls.
             Execution time mean : 8,562699 µs
    Execution time std-deviation : 1,289355 µs
   Execution time lower quantile : 7,557778 µs ( 2,5%)
   Execution time upper quantile : 10,039290 µs (97,5%)
                   Overhead used : 8,140922 ns
EVALUATION:
-> 3
Evaluation count : 1607688 in 6 samples of 267948 calls.
             Execution time mean : 433,635072 ns
    Execution time std-deviation : 67,003007 ns
   Execution time lower quantile : 378,876890 ns ( 2,5%)
   Execution time upper quantile : 512,818448 ns (97,5%)
                   Overhead used : 8,140922 ns
```

Use `--parse`, `--evaluate` and/or `--analyze` to bench individual phases
(`--complete` will bench all of them). Leaving out `--quick` will run
`criterium/bench` instead of `criterium/quick-bench`.

#### GraalVM native-image

To benchmark an expression within GraalVM `native-image`, run `script/compile` and then run:

``` clojure
$ time ./sci "(loop [val 0 cnt 1000000] (if (pos? cnt) (recur (inc val) (dec cnt)) val))"
1000000
./sci    0.92s  user 0.08s system 99% cpu 1.003 total
```

## Thanks

- [adgoji](https://www.adgoji.com/) for financial support.
- [Clojurists Together](https://www.clojuriststogether.org/) for financial support.
- [Lee Read](https://github.com/lread/) for the logo.
- [contributors](https://github.com/borkdude/sci/graphs/contributors) and other users posting issues with bug reports and ideas

## License

Copyright © 2019-2020 Michiel Borkent

Distributed under the Eclipse Public License 1.0. This project contains code
from Clojure and ClojureScript which are also licensed under the EPL 1.0. See
LICENSE.
