# ADR 0020: rejected JIT call-shape optimizations

Status: accepted (2026-08-12).

## Context

The round of CLJS JIT work in #1070-#1075 followed one pattern: find a shape
the emitter handles worse than the ClojureScript compiler does, and fix it.
Two of those were interpreter escapes (`(:k m)` in #1072, js global reads in
#1073), where a single occurrence also dropped the enclosing fn out of locals
mode. Those paid: 21x on arithmetic-dense code, 8.4x on `instance?` in a loop,
2.7x on js globals.

The same search turned up call shapes that look wrong but measure flat. This
records them so the next reader does not re-derive them.

Measurements are sci-vs-sci A/B on `scratch/scibench` (a 512x16 emitter grid,
8192 calls/frame, `:advanced`), because a sci-vs-compiled-CLJS ratio is not
trustworthy here: V8 deletes the compiled side of a pure loop whose result is
unused, which makes it look infinitely fast.

## Decision

Emit `.call(null, ...)` for a constant callee. Do not special-case it.

## Constant callees

A constant callee arrives as `:call-node` with a `[:const f]` child rather
than as `:call-direct`, so it emits `head.call(null, args)` instead of the
arity call `:call-direct` gets. Destructuring's `seq?` guard is one, which
makes it fire on every `{:keys [...]}`.

Giving it the `:call-direct` treatment works: every `c1.call(null,t0)` in the
emitted templates became `c1(t0)`, and no `.call(null` remained. It buys
nothing.

| | master | branch |
|---|---|---|
| destructuring wrapper | 1.020 ms/frame | 1.031 |
| template bytes over 8 forms | 2564 | 2554 |

V8 optimizes `f.call(null, ...)` as well as `f(...)`. The same result had
already appeared once in this round: binding a static interop method with
`.bind` at compile time instead of calling it unbound measured 1.279 vs 1.285
ms/frame, also flat.

Unlike #1075, which deleted a stale regex that had stopped recognising const
refs after they were renamed, this adds a branch to `emit-call` in exchange
for nothing measurable. Rejected on that basis.

## Compiling `lazy-seq`

`analyze-lazy-seq` builds a node with no ast, so the body escapes. The obvious
fix is to route `lazy-seq` through its own `lazy-seq*` expansion,
`(new cljs.core/LazySeq nil (fn [] ...) nil nil)`, which the jit can compile.
That is 2x SLOWER:

| | current | via fn expansion |
|---|---|---|
| user lazy range | 11.5 ms | 24.1 |
| lazy-seq + take | 13.9 ms | 29.1 |

Constructing a sci fn per realization costs more than interpreting the body
saves. The specialized node is the tuned path, not an oversight.

A raw JS thunk emitted inline (`new LazySeq(null, function(){...}, ...)`)
would avoid the sci fn, but the ceiling is small: an eager control isolates
the escape at sci 5.0 vs lazy 11.5 (native 3.1 vs 5.1), so removing it lands
near 6.6 ms, about 1.7x, on lazy-seq-heavy code only. Array mode is not part
of that: forcing it with a `try` measured 4.6 ms against 4.6 ms for the same
fn in locals mode.

That needs a sub-emitter (the emitter constructs one `EmitterState` and cannot
nest), plus `recur`-inside-`lazy-seq` and binding-aliasing semantics, since
the interpreter's thunk closes over the same mutable bindings array. Not taken
at that price.

## Consequences

- `emit-call` keeps one path for non-`:call-direct` callees.
- "This dispatch happens a lot" remains not evidence, the same conclusion ADR
  0017 reached for fusing children: 45% of `if` conditions were fusable there
  and it bought 1%.
- The remaining sci-vs-CLJS gap on the emitter grid (1.04 vs 0.91 ms/frame) is
  not in the emitted call shapes. Map lookups emit the arity call CLJS emits
  and probe within 3% of it; the body is already ahead (0.48 vs 0.52). What is
  left sits at the sci fn invocation boundary, ~8 ns per call.

## Verification

- `script/test/node`: `:none`, `:advanced`, and jit forced off, 425 tests,
  6458 assertions, 0 failures each.
- clj-kondo over the changed files: clean.
- Emitted templates inspected directly to confirm each change fired before
  timing it, after an earlier measurement in this round turned out to have
  been taken against code that never compiled.
