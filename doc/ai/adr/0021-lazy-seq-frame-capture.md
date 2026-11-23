# ADR 0021: lazy-seq copies its frame outside tail position

Status: accepted (2026-11-23).

## Context

`analyze-lazy-seq` builds a host thunk that evaluates the body against the
frame of the enclosing fn or loop. `recur` writes new values into that same
frame. A `lazy-seq` created in one iteration and realized later read the
values of the last iteration:

```clojure
(loop [i 0 acc []]
  (if (< i 3)
    (recur (inc i) (conj acc (lazy-seq [i])))
    (map first acc)))
;; Clojure: (0 1 2)   SCI before: (3 3 3)
```

Since #1145 a `recur` in the `lazy-seq` body reruns that body, which writes
the body's `let` slots into the frame again. A `lazy-seq` nested in such a
body had the same problem.

## Decision

Copy the frame when the `lazy-seq` is created, except when it is in tail
position. `(recur-target? ctx)` tells: the analyzer keeps a recur target only
for forms in tail position of a fn, loop or `lazy-seq` body.

A `lazy-seq` in tail position is the value the body returns. No `recur` can
follow in that invocation, so nothing writes the frame after it. Every other
position gets a copy.

SCI drops the recur target inside `try`, so a `lazy-seq` there is copied even
in tail position. That costs a copy and is correct.

Removing the tail exception passes every test and costs about 25% on code in
the `(defn nums [n] (lazy-seq ...))` style. Keep it.

## Measurements

`(count (nums 0))` for a self-recursive `lazy-seq` of 1e5 elements, JVM, best
of 300 runs after 300 warmup runs, one JVM per variant. Compiled Clojure takes
about 1.03 ms.

| variant | ms | vs Clojure |
|---|---|---|
| master c2dbcc47, shared frame, wrong in a recurring loop | 1.75 - 1.85 | 1.8x |
| copy outside tail position | 1.86 | 1.8x |
| copy if the frame recurs, checked with `.deref` | 1.83 - 1.91 | 1.8x |
| copy if the frame recurs, checked with `@` | 2.05 - 2.09 | 2.0x |
| copy always | 2.30 - 2.32 | 2.2x |
| `(new LazySeq (fn* [] body))` | 2.99 - 3.03 | 2.9x |

Analysis of 100 `lazy-seq` fns stays at 0.66 - 0.73 ms for every variant
except `fn*`, which takes 0.97 - 1.00 ms.

Native bb is not measured.

## Rejected

- `(fn* [] body)`, the way Clojure's macro does it. Correct, and `recur` needs
  no special case. It allocates a closure array, a fn object and a frame per
  `lazy-seq`. Wrapping the old node in `((fn [] ...))` allocates the same.
- Copy always. Correct and the least code. The copy is wasted in tail
  position, which is where generator-style code puts its `lazy-seq`.
- Copy if the frame recurs: a volatile per frame, set by `return-recur`, read
  when the `lazy-seq` is created. As fast as the tail rule, but every new way
  to write a frame is a new case. The `lazy-seq` body's own `recur` was missed
  in review.

## Not done

The tail rule would also let a fn in tail position share the parent frame
and skip its enclosed array. Few closures are in tail position, and the fn
would keep every local of the parent frame alive, which Clojure does not. A
tail `lazy-seq` already keeps its frame alive, as it did before this change.
