# ADR 0022: rejected single-call map destructuring coercion

Status: rejected (2026-11-23).

## Context

A table of per-expression costs in interpreted loops listed
`(let [{:keys [a b]} m] ...)` at 50 ns per iteration, against 23 ns for
`(+ acc (:a m) (:b m))`. The table came from one JVM running 40 shapes in
turn, so later rows carried call-site contamination from earlier ones.

Measured again with one JVM per shape (JVM, 1M iterations, best of 9):

| Shape | ns/iter |
|---|---|
| `(let [{:keys [a b]} m] (+ acc a b))` | 35.5 |
| the same expansion written by hand | 36.9 |
| `(let [a (get m :a) b (get m :b)] (+ acc a b))` | 31.8 |
| `(+ acc (get m :a) (get m :b))` | 25.0 |
| `(let [[x y] v] (+ acc x y))` | 31.2 |
| `(let [{:keys [a b] :or {b 2}} m] ...)` | 46.0 |
| fn param `[{:keys [a b]}]` vs `[m]` with two `get` | 33.9 vs 24.4 |

The expansion does not go through vars. `destvec*` and `destmap*` embed
`seq?`, `nth`, `seq`, `first` and `next` as fn objects, and `get` reaches the
call node as an inlined var and compiles to `RT/get`.

Of the 10 ns between destructuring and inline access, about 7 ns is two `let`
slots. About 4 to 5 ns is specific to map destructuring: the second `map__`
binding and the node chain for
`(if (seq? map__) (seq-to-map-for-destructuring map__) map__)`. Vector
destructuring costs the same as hand-written `nth` calls.

## The change that was tried

Branch `destructure-map-coerce`, commit `b9e59ee3`. `destmap*` emits one
binding, `map__ (seq->map v)`, where `seq->map` is an embedded host fn that
does the `ISeq` check and the conversion. That replaces two bindings and three
nodes with one binding and one call. JVM only.

## Measurements

Micro, JVM, three interleaved rounds, one JVM per run, best of three:

| Shape | master | branch |
|---|---|---|
| `(let [{:keys [a b]} m] (+ acc a b))` | 36.4 | 29.8 |
| fn param `[{:keys [a b]}]` | 34.1 | 29.6 |
| `(let [{:keys [a b] :or {b 2}} m] ...)` | 46.3 | 42.3 |
| control, `(+ acc (get m :a) (get m :b))` | 24.9 | 25.0 |

Libraries in JVM bb, four interleaved rounds, best of four, in ms. Warm load
reloads the library 60 times in one JVM with the env reset each time.

| Workload | master | branch |
|---|---|---|
| malli 0.20.3 warm load | 42.4 | 43.2 |
| malli cold load | 276 | 277 |
| honeysql `564f4fd3` warm load | 16.0 | 16.1 |
| honeysql cold load | 156 | 154 |
| malli compiled validator x50000 | 40.1 | 36.6 |
| malli validator-create x200 | 16.3 | 16.1 |
| malli explain and humanize x2000 | 41.8 | 42.1 |
| malli decode x20000 | 6.44 | 6.29 |
| malli uncached validate x200 | 16.0 | 16.6 |
| honeysql format-select x500 | 49.9 | 48.0 |
| honeysql helpers and format x500 | 25.4 | 25.7 |
| honeysql format-insert x300 | 13.8 | 13.7 |
| honeysql format-inline x500 | 47.2 | 47.1 |

Only the malli compiled validator moved outside the noise of about 4%. Its
four rounds did not overlap (36.6 to 39.3 against 40.0 to 41.8).

The same expansion on CLJS, node, `:simple`, ns per iteration:

| | old expansion | single call |
|---|---|---|
| jit off | 293.5 | 276 |
| jit on | 37.7 | 46 to 47 |

With the jit on, which is the default, the single call is 25% slower. The
cause was not found.

No native bb build was measured.

## Decision

Do not merge. The change adds a sci-specific patch to code that tracks
clojure/clojure, makes the `macroexpand` output of a map destructure less
readable, and moves one row out of thirteen in library code.

## Notes for a next attempt

- Isolate a shape in its own JVM before treating a shared-table number as a
  lead. This one shrank from 50 to 35 ns.
- `:or` costs 12 ns more than the plain case for one default. A 3-arg `get`
  on its own is as fast as a 2-arg one (11.6 vs 12.8 ns). Inside a `let` it
  costs about 5 ns more. The rest is unexplained.
- The gain a dedicated destructuring node could add on top of this is bounded
  by the same 4 to 5 ns per map destructure.
