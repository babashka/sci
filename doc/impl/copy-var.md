# copy-var implementation

`copy-var` creates SCI vars from host (CLJ/CLJS) vars. It has two entry points:

- **Public API** (`sci.core/copy-var`): thin macro that delegates to the internal
  macro with `{:sci.impl/public true}`.
- **Internal** (`sci.impl.copy-vars/copy-var`): used by `sci.impl.namespaces` to
  build the core namespace. Also used via `copy-core-var` which adds
  `{:copy-meta-from clojure.core/sym}`.

Both produce `sci.lang.Var` instances with metadata copied from the host var.

## Key files

- `src/sci/core.cljc` - Public `copy-var` macro (forwards to internal)
- `src/sci/impl/copy_vars.cljc` - Internal `copy-var` macro and `var-meta` function

## How it works

### var-meta

`var-meta` is a function inside `macros/deftime` that resolves a symbol and
extracts its metadata (`:doc`, `:arglists`, `:dynamic`, `:private`, `:macro`,
`:file`, `:line`, `:column`). It takes `&env` as an explicit parameter.

Symbol qualification (turning unqualified symbols into fully-qualified ones):

- **CLJ compilation** (`(:ns &env)` is falsy): uses `resolve` to find the var.
  This handles both core vars (`inc` -> `clojure.core/inc`) and non-core vars
  (`my-fn` -> `some-ns/my-fn`).
- **CLJS compilation** (`(:ns &env)` is truthy, public): qualifies using
  `(:name (:ns &env))` (the current CLJS namespace).
- **CLJS compilation** (non-public): defaults to `clojure.core`.

The resolved var is cached in `resolved-var` and reused for metadata extraction,
so each var is resolved exactly once.

For the public API, `var-meta` auto-detects `:macro` from the resolved var's
metadata, so users don't need to pass `{:macro true}`.

### copy-var macro

The `copy-var` macro calls `var-meta` to get metadata, then emits a
`sci.lang.Var.` constructor call.

Key decisions:

- **init value**: For macros and private vars, emits `(deref (var sym))` instead
  of the bare symbol. Bare symbols fail for macros ("can't take value of a
  macro") and for private vars across namespaces ("is not public").
- **`:sci/built-in true`**: Added for internal vars (not public API).
- **`elide-vars`**: When `SCI_ELIDE_VARS=true`, non-dynamic non-ctx vars skip
  the `Var` wrapper entirely and emit the bare symbol.

### copy-core-var

Shorthand: `(copy-core-var inc)` expands to
`(copy-var inc clojure-core-ns {:copy-meta-from clojure.core/inc})`.

### Options

- `:name` - Display name for the SCI var
- `:macro` - Mark as macro (auto-detected for public API)
- `:dynamic` - Mark as dynamic
- `:copy-meta-from` - Symbol whose metadata to use instead of `sym`'s
- `:init` - Custom init value (default: `sym` itself)
- `:ctx` - SCI context for vars that need it
- `:inlined` - Override for inlined var optimization
- `:sci.impl/public` - Set by the public API macro; changes behavior:
  - Skips `:sci/built-in` metadata
  - Enables macro/private auto-detection from resolved var
  - Uses `&env` namespace for CLJS symbol qualification

## Cross-platform considerations

`var-meta` and `copy-var` live inside `macros/deftime`, which expands to `do` on
CLJ and nothing on CLJS (macros imported via `:require-macros`). This means:

- All code is CLJ at the reader level. `#?(:clj ...)` reader conditionals
  distinguish JVM from self-hosted CLJS compilation.
- `macros/?` checks `(contains? &env '&env)` at runtime. Inside macro bodies
  and functions with `&env` as a parameter, this emits
  `(if (:ns &env) <cljs> <clj>)` -- a **runtime** check. Both branches are live
  code.
- CLJ `resolve` calls must be wrapped in `#?(:clj ...)` to prevent self-hosted
  CLJS from trying to compile them (CLJS `resolve` is a macro requiring a quoted
  symbol).
- CLJS metadata comes from `cljs-resolve` (wrapping `cljs.analyzer.api/resolve`),
  not CLJ `resolve`.
