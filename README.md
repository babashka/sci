<img src="logo/logo-300dpi.png" width="100px">

[![CircleCI](https://circleci.com/gh/borkdude/sci/tree/master.svg?style=shield)](https://circleci.com/gh/borkdude/sci/tree/master)
[![Clojars Project](https://img.shields.io/clojars/v/borkdude/sci.svg)](https://clojars.org/borkdude/sci)
[![NPM Project](https://img.shields.io/npm/v/@borkdude/sci)](https://www.npmjs.com/package/@borkdude/sci)
[![cljdoc badge](https://cljdoc.org/badge/borkdude/sci)](https://cljdoc.org/d/borkdude/sci/CURRENT)

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

[More](#Usage) on how to use sci from Clojure.
Use from [JavaScript](#use-from-javaScript). Use from [Java](#use-from-java).

## Rationale

You want to evaluate code from user input, or use Clojure for a DSL inside
configuration files, but `eval` isn't safe or simply doesn't work.

This library works with:

- Clojure on the JVM
- Clojure compiled with GraalVM native
- ClojureScript, even when compiled with `:advanced`, and (as a consequence) JavaScript

It is used in:

- [Babashka](https://github.com/borkdude/babashka). A Clojure scripting tool that plays well with Bash.
- [Bootleg](https://github.com/retrogradeorbit/bootleg). An HTML templating CLI.
- [Closh](https://github.com/dundalek/closh). Bash-like shell based on Clojure. GraalVM port is work in progress.
- [Jet](https://github.com/borkdude/jet). CLI to convert between JSON, EDN and Transit.
- [Malli](https://github.com/metosin/malli). Plain data Schemas for Clojure/Script.

## Status

Experimental. Breaking changes are expected to happen at this phase.

## Installation

Use as a dependency:

[![Clojars Project](https://img.shields.io/clojars/v/borkdude/sci.svg)](https://clojars.org/borkdude/sci)
[![NPM Project](https://img.shields.io/npm/v/@borkdude/sci)](https://www.npmjs.com/package/@borkdude/sci)

## API docs

For Clojure, see the generated [codox](https://borkdude.github.io/sci/doc/codox)
documentation. For Java, see the generated [Java
documentation](https://borkdude.github.io/sci/doc/javadoc/index.html)

## Usage

Currently the only API function is `sci.core/eval-string` which takes a string
to evaluate and an optional options map.

In `sci`, `defn` does not mutate the outside world, only the evaluation
context inside a call to `sci/eval-string`.

By default `sci` only enables access to the pure non-side-effecting functions in
Clojure.  More functions can be enabled, at your own risk, using `:bindings`:

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

Preventing forever lasting evaluation of infinite sequences can be achieved with
`:realize-max`:

``` clojure
user=> (sci/eval-string "(vec (range))" {:realize-max 10})
ExceptionInfo Maximum number of elements realized: 10 [at line 1, column 1]  clojure.core/ex-info (core.clj:4739)
```

The preset `:termination-safe`, which is currently `{:deny '[loop recur
trampoline] :realize-max 100}`, is helpful for making expressions terminate:

``` clojure
user=> (sci/eval-string "(loop [] (recur))" {:preset :termination-safe})
ExceptionInfo loop is not allowed! [at line 1, column 2]  clojure.core/ex-info (core.clj:4739)
```

Providing a macro as a binding can be done by providing a normal function that:
- has `:sci/macro` on the metadata set to `true`
- has two extra arguments at the start for `&form` and `&env`:

``` clojure
user=> (def do-twice ^:sci/macro (fn [_&env _&form x] (list 'do x x)))
user=> (sci/eval-string "(do-twice (f))" {:bindings {'do-twice do-twice 'f #(println "hello")}})
hello
hello
nil
```

## Vars

Sci has a var type, distinguished from Clojure vars. In a sci program these vars
are created with `def` and `defn` just like in normal Clojure:

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
dynamic sci vars `sci/in`, `sci/out` and `sci/err` in API. These vars
can be rebound as well:

``` clojure
(def sw (java.io.StringWriter.))
(sci/binding [sci/out sw] (sci/eval-string "(println \"hello\")")) ;;=> nil
(str sw) ;;=> "hello\n"
```

A shorthand for rebinding `sci/out` is `sci/with-out-str`:

``` clojure
(sci/with-out-str (sci/eval-string "(println \"hello\")")) ;;=> "hello\n"
```

## Feature parity

Currently the following special forms/macros are supported: `def`, `fn`,
function literals (`#(inc %)`), `defn`, `quote`, `do`,`if`, `if-let`, `if-not`,
`when`, `when-let`, `when-not`, `cond`, `let`, `and`, `or`, `->`, `->>`, `as->`,
`comment`, `loop`, `lazy-seq`, `for`, `doseq`, `case`, `try/catch/finally`,
`declare`, `cond->`, `cond->>`, `require`, `import`, `in-ns`, `ns`,
`binding`. Sci also supports user defined macros.

More examples of what is currently possible can be found at
[babashka](https://github.com/borkdude/babashka).

If you miss something, feel free to post an issue.

## Caveats

To make the `rand-*` functions behave well when compiling to a GraalVM native binary, use this setting:

``` clojure
--initialize-at-run-time=java.lang.Math\$RandomNumberGeneratorHolder
```

## Use from JavaScript

``` javascript
> const { evalString } = require('@borkdude/sci');
> const opts = {bindings: {f: function() { console.log('hello'); }}};
> evalString("(dotimes [i 2] (f))", opts);
hello
hello
```

Note for JavaScript users: the JS API is similar to the Clojure one. Instead of
symbols and keywords it expects strings. Instead of kebab-case, use
camelCase. Read [here](#Usage) how to use sci from Clojure.

## Use from Java

``` java
import borkdude.sci.*;
import borkdude.sci.options.*;

Namespace fooBar = new Namespace("foo.bar");
fooBar.addVar("x", 1);
Options opts = new Options().addNamespace(fooBar);
Sci.evalString("foo.bar/x", opts); // returns 1
```

Note for Java users: the Java API for is conceptually similar to the Clojure
one, but made more idiomatic for Java users. Check the generated [Java
documentation](https://borkdude.github.io/sci/doc/javadoc/index.html).

## Test

Required: `lein`, the `clojure` CLI and GraalVM.

To succesfully run the GraalVM tests, you will have to compile the binary first
with `script/compile`.

To run all tests:

    script/test/all

For running individual tests, see the scripts in `script/test`.

## Thanks

- [adgoji](https://www.adgoji.com/) for financial support

## License

Copyright © 2019 Michiel Borkent

Distributed under the Eclipse Public License 1.0. This project contains code
from Clojure and ClojureScript which are also licensed under the EPL 1.0. See
LICENSE.
