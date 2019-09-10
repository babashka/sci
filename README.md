# Small Clojure Interpreter

[![CircleCI](https://circleci.com/gh/borkdude/sci/tree/master.svg?style=shield)](https://circleci.com/gh/borkdude/sci/tree/master)
[![Clojars Project](https://img.shields.io/clojars/v/borkdude/sci.svg)](https://clojars.org/borkdude/sci)
[![cljdoc badge](https://cljdoc.org/badge/borkdude/sci)](https://cljdoc.org/d/borkdude/sci/CURRENT)

A tiny implementation of Clojure in Clojure.

## Rationale

You want to evaluate code from user input, but `eval` isn't safe or simply
doesn't work.

This library works with:

- Clojure on the JVM
- Clojure compiled with GraalVM native
- ClojureScript, even when compiled with `:advanced`

It is used as the interpreter for
[babashka](https://github.com/borkdude/babashka).

## Status

Experimental. Breaking changes are expected to happen at this phase.

## Installation

Use as a dependency:

[![Clojars Project](https://img.shields.io/clojars/v/borkdude/sci.svg)](https://clojars.org/borkdude/sci)

## Usage

``` clojure
(require '[sci.core :as sci])
(sci/eval-string "(inc 1)") => ;; 2
(sci/eval-string "(inc x)" {:bindings {'x 2}}) ;;=> 3
```

Currently the following special forms/macros are supported: `def`, `fn`,
function literals (`#(inc %)`), `defn`, `quote`, `do`,`if`, `when`, `cond`,
`let`, `and`, `or`, `->`, `->>`, `as->`, `comment`, `loop`, `lazy-seq`, `for`,
`doseq`.

In `sci`, `defn` does not mutate the outside world, only the evaluation
context inside a call to `sci/eval-string`.

By default `sci` only enables access to the pure non-side-effecting functions in
Clojure.  More functions can be enabled, at your own risk, using `:bindings`:

``` clojure
user=> (sci/eval-string "(println \"hello\")" {:bindings {'println println}})
hello
nil
```

More examples of what is currently possible can be found at
[babashka](https://github.com/borkdude/babashka).

## Caveats

To make the `rand-*` functions behave well when compiling to a GraalVM native binary, use this setting:

``` clojure
--initialize-at-run-time=java.lang.Math\$RandomNumberGeneratorHolder
```

## Test

Required: `lein`, the `clojure` CLI and GraalVM.

To succesfully run the GraalVM tests, you will have to compile the binary first
with `script/compile`.

To run all tests:

    script/test/all

For running individual tests, see the scripts in `script/test`.

## License

Copyright © 2019 Michiel Borkent

Distributed under the Eclipse Public License 1.0. This project contains code
from Clojure and ClojureScript which are also licensed under the EPL 1.0. See
LICENSE.

The directory `inlined` contains source from [`tools.reader`](https://github.com/clojure/tools.reader) which is licensed under the EPL license.
