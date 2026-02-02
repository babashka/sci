# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SCI (Small Clojure Interpreter) is a sandboxed Clojure interpreter that works on JVM, GraalVM native-image, and ClojureScript. It's designed for evaluating user input safely or creating DSLs. SCI powers babashka, nbb, scittle, and many other Clojure tools.

## Build Commands

```bash
# Run JVM tests (tests with Clojure 1.10.3 and 1.11.1)
script/test/jvm

# Run a specific test namespace with lein
lein test :only sci.core-test

# Run a specific test
lein test :only sci.core-test/core-test

# Run ClojureScript tests in Node.js (both :none and :advanced optimizations)
script/test/node

# Compile native binary (requires GRAALVM_HOME)
script/compile

# Run tests against native binary
script/test/native

# Run all tests (JVM, Node, native)
script/test/all

# Benchmark an expression
clojure -M:bench --complete --sexpr "(let [x 1 y 2] (+ x y))" --quick

# Generate API documentation
bb quickdoc

# Generate bundle size report
npx shadow-cljs run shadow.cljs.build-report sci report.html
```

## Architecture

SCI processes Clojure code in three phases:

1. **Parsing** (`sci.impl.parser`) - Uses edamame to parse source code into Clojure data structures
2. **Analysis** (`sci.impl.analyzer`) - Analyzes forms, resolves symbols, expands macros, and produces executable nodes
3. **Evaluation** (`sci.impl.evaluator`, `sci.impl.interpreter`) - Executes analyzed nodes

Key implementation files:
- `src/sci/core.cljc` - Public API (`eval-string`, `init`, `fork`, `copy-var`, etc.)
- `src/sci/impl/namespaces.cljc` - Built-in Clojure core functions implementation
- `src/sci/impl/vars.cljc` - SCI's var implementation with thread-local bindings
- `src/sci/impl/types.cljc` - Node types for the interpreter
- `src/sci/impl/resolve.cljc` - Symbol resolution
- `src/sci/impl/load.cljc` - Namespace loading (`require`, `load-file`)
- `src/sci/lang.cljc` - Core types: `Var`, `Namespace`, `Type` (deftype)

The codebase uses `.cljc` files extensively for cross-platform compatibility (JVM + ClojureScript).

## Testing

Tests are in `test/sci/` and use `.cljc` for cross-platform testing. The test utility namespace `sci.test-utils` provides helpers like `eval*` that work across platforms.

Environment variables:
- `SCI_TEST_ENV=native` - Test against the native binary instead of JVM
- `SCI_FAIL_FAST=true` - Stop on first test failure

## Dependencies

- `borkdude/edamame` - Clojure parser
- `org.babashka/sci.impl.types` - Shared type definitions
- `borkdude/graal.locking` - GraalVM-compatible locking primitives

## Development Notes

- SCI vars are distinct from Clojure vars for sandboxing
- Dynamic vars in SCI scripts work with `binding` only for vars defined in the script or exposed as SCI dynamic vars
- The `:classes` option controls Java interop access
- The `:namespaces` option pre-populates namespaces with vars
- Context (`sci/init`) maintains state across evaluations; use `sci/fork` for isolated copies
