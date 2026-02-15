# ADR 0004: macroexpand-1 Two-Arity with env

| Status | Date | Related |
|--------|------|---------|
| Accepted | 2026-02-15 | Riddley babashka compatibility |

## Context

Riddley is a Clojure code-walking library used by Specter, Cloverage, and others. It tracks local bindings while walking code and needs to communicate these to `macroexpand-1` so that macros see correct `&env` during expansion.

On JVM Clojure, riddley writes locals to `Compiler/LOCAL_ENV` (a thread-bound Var), and `macroexpand-1` picks them up automatically. In babashka/SCI, `Compiler/LOCAL_ENV` is not available.

## Problem

How should riddley communicate its tracked locals to SCI's `macroexpand-1`?

## Options Considered

### Option A: Two-arity macroexpand-1 (chosen)

Add `(macroexpand-1 env form)` where `env` is a map of local bindings, matching the convention of `cljs.analyzer/macroexpand-1`.

**Pros:**
- Simple to implement
- Matches CLJS convention
- No new dynamic vars or namespaces needed

**Cons:**
- Risk of conflict if Clojure adds a 2-arity with different semantics (low risk — unchanged for 15+ years, and CLJS uses the same `(env form)` signature)

### Option B: Mock Compiler/LOCAL_ENV as a SCI dynamic var

Create a SCI dynamic var, mock `Compiler/LOCAL_ENV` to return it. Riddley would use the same code path on both JVM and bb.

**Pros:**
- Riddley wouldn't need `if-bb` for the macroexpand call in walk.clj
- Other libraries reading `@Compiler/LOCAL_ENV` would work

**Rejected because:**
- Riddley's JVM path also calls `.isBound`, `.set`, and `with-bindings` on `Compiler/LOCAL_ENV`, plus uses `Compiler$ObjMethod`, `Compiler$ObjExpr`, `Compiler/CLEAR_SITES`, `Compiler/METHOD`, and `riddley.Util` Java helpers — too many internals to mock
- Research showed all non-riddley uses of `Compiler/LOCAL_ENV` are either debug macros that could use `&env` instead, or analyzer tools resetting compiler state to `nil` — no compelling bb use cases

### Option C: SCI dynamic var without Compiler/LOCAL_ENV mock

Expose a new dynamic var (e.g., in `sci.impl`) that `macroexpand-1` checks. Riddley would bind this var.

**Rejected because:**
- Creates coupling between riddley and SCI internals
- Requires a new namespace or public var with no Clojure equivalent
- No clear namespace to put it in (`babashka.analyzer-api`? `sci.impl.macroexpand`?)

## Decision

Option A — two-arity `(macroexpand-1 env form)`.

The env-first argument convention matches CLJS. The risk of Clojure conflict is negligible. Riddley's `walk.clj` uses `(cmp/if-bb (macroexpand-1 (or (cmp/locals) {}) x) (macroexpand-1 x))` — the only bb-specific line in the walk logic.

## Research: Compiler/LOCAL_ENV usage in the wild

GitHub code search found ~30 uses. All non-riddley uses fall into:

1. **Debug macros** (~10 repos): `(map key @Compiler/LOCAL_ENV)` — could use `&env` instead
2. **REPL/analyzer bindings** (nrepl, Clojure-Sublimed, jvm.tools.analyzer): `{Compiler/LOCAL_ENV nil}` — resetting compiler state, JVM-only
3. **One-off checks** (ohua, slyphon): `(contains? @Compiler/LOCAL_ENV sym)` — could use `&env`

None require the values in `LOCAL_ENV` (which are `Compiler$LocalBinding` objects). None are relevant to babashka.
