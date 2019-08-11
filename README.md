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

``` shellsession
[borkdude/sci "0.0.1"]
```

## Usage

``` clojure
(require '[sci.core :as sci])
(sci/eval-string "(inc 1)") => ;; 2
(sci/eval-string "(inc x)" {:bindings {'x 2}} ;;=> 3
```

Anonymous functions literals are allowed with currently up to three positional
arguments. This is an arbitrary limit and may be changed in the future.

``` clojure
(sci/eval-string "(#(+ %1 %2 %3) 1 2 3)") => ;; 6
```

Currently, only the special forms/macros `quote`, `if`, `when`, `and`, `or`,
`->` and `->>` are supported.

By default `sci` only enables access to the pure non-side-effecting functions in
Clojure. More functions can be enabled, at your own risk, using `:bindings`:

``` clojure
user=> (sci/eval-string "(println \"hello\")" {:bindings {'println println}})
hello
nil
```

More examples of what is currently possible can be found at
[babashka](https://github.com/borkdude/babashka).

## Test

Run all tests:

    script/test

This will run tests on the JVM, Node.js and using a GraalVM binary.

Test the native version:

    SCI_TEST_ENV=native script/test

## Build

You will need leiningen and GraalVM.

    script/compile

## License

Copyright © 2019 Michiel Borkent

Distributed under the EPL License, same as Clojure. See LICENSE.
