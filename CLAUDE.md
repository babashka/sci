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

# Run specific CLJS test namespace with cljs-test-runner (requires :test alias for deps)
clojure -M:test:cljs-test-runner -d test -n sci.async-await-test

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

### Type system (`sci.lang.Type`)

- `deftype`/`defrecord` do NOT create vars — they store types in the `:types` key of the namespace map, matching Clojure behavior
- `sci.lang.Type` does NOT implement `Named`/`INamed` — this would conflict with `Class.getName()` behavior
- `.getName` works via `HasName` protocol (returns fully qualified name like `"user.Foo"`)
- Symbol resolution checks `:types` after `:refers` (in `sci.impl.resolve`)
- `import` copies types between namespaces via the `:types` key

### Testing guidelines

- `tu/eval*` shells out to the native binary when `SCI_TEST_ENV=native` — use `sci/eval-string` directly for tests that need custom SCI opts (like `:classes`) or that test host-level interop (like `.sym`, `.getName`)
- When iterating on fixes, run only the specific failing test (`lein test :only ns/test-name`) instead of the full suite. Only run `script/test/jvm` or `script/test/node` once at the end to confirm everything passes. Full test runs are slow — don't repeat them unnecessarily.
- Always redirect test output to a file (e.g. `script/test/jvm 2>&1 | tee /tmp/sci-test-jvm.txt`) so the results can be re-read without re-running. When checking previous test results, read the cached file instead of running again.

## Cross-Platform Macro System (`sci.impl.macros`)

The `macros/?` macro decides between CLJ and CLJS code paths by checking `(contains? &env '&env)`. This has a critical implication:

- Inside `defmacro` bodies, `&env` is an implicit local binding, so the check is **true** — `macros/?` emits `(if (:ns &env) <cljs-branch> <clj-branch>)`, a **runtime** check. Both branches are live code.
- The same applies to functions that take `&env` as a parameter (e.g. `var-meta` in `copy_vars.cljc`).
- Do **not** assume one branch is dead code. Changes to either branch affect behavior.
- `macros/deftime` on CLJ expands to `do`. It wraps compile-time-only code (macros, helper functions used by macros).
- `#?(:clj ...)` reader conditionals ARE compile-time — those branches are truly dead on the other platform. Don't confuse `#?(:clj ...)` with `macros/? :clj ...`.

# Clojure REPL Evaluation

The command `clj-nrepl-eval` is installed on your path for evaluating Clojure code via nREPL.

**Discover nREPL servers:**

`clj-nrepl-eval --discover-ports`

**Evaluate code:**

`clj-nrepl-eval -p <port> "<clojure-code>"`

With timeout (milliseconds)

`clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"`

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.
