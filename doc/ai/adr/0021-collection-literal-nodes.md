# ADR 0021: collection literal nodes on the JVM

Status: accepted (2026-09-01).

## Context

A map, vector or set literal with a non-constant child was analyzed as a
call node: `return-call` with a varargs fn as the callee. For maps that fn
was `#(PersistentArrayMap/createWithCheck (into-array Object %&))`, vectors
called `clojure.core/vector` and sets a `%&` wrapper around
`PersistentHashSet/createWithCheck`.

A CPU profile of a mixed workload (records, destructuring, `cond`, `case`,
string tokenizing, a map literal with six entries per iteration) put 19% of
application samples under `RT.seqToTypedArray`. That is `into-array` with a
class argument, which fills the array with the reflective
`java.lang.reflect.Array.set`. The path before it was not free either: a
twelve-argument `IFn.invoke` on a `RestFn` builds an `ArraySeq` first.

`defrecord` had a related cost. Its positional factory called a second sci
fn, which called `(zipmap [:x :y] [x y])` at run time: a vector literal, a
transient map, two seqs. That was 6.5% of the same profile.

## Decision

On `:clj`, `return-map` and `return-coll` build a `literal-node`: a node
that evaluates the children into an `Object[]` in a loop and passes the
array to a static factory.

| literal | factory |
|---|---|
| map, up to 8 entries | `PersistentArrayMap/createWithCheck` |
| map, more | `PersistentHashMap/createWithCheck` |
| vector | `LazilyPersistentVector/createOwning` |
| set | `PersistentHashSet/createWithCheck` |

All four take `Object[]` and exist in Clojure 1.10.3. The node keeps the
try/catch of a call node, so a duplicate key still reports the location
of the literal, and the `:tag` metadata handling.

`defrecord-macro` now emits the field map as a map literal in the
expansion, `{:x x :y y}`, and the positional factory calls
`->record-impl` directly instead of through the four-arity constructor fn.
The literal analyzes to the node above.

CLJS and cljd keep the call node. The CLJS jit reads the `:call-direct`
ast that `return-call` attaches, and the varargs cost is a JS argument
copy there, not reflection.

## Measurements

JVM, criterium mean, best of three interleaved runs in isolated JVMs,
direct linking on. The mixed workload is 2000 iterations of the profile
above. The csk workload runs camel-snake-kebab 0.4.3 interpreted over 1500
strings and has no collection literal on its hot path. It is the control.

| workload | master | branch | delta |
|---|---|---|---|
| mixed, records and map literals | 4.61 ms | 3.11 ms | -33% |
| camel-snake-kebab, strings | 7.29 ms | 7.43 ms | noise |

Application samples in the profile of the mixed workload fell from 3579 to
2456. `RT.seqToTypedArray` and `zipmap` are gone from it. What remains at
the top is the workload itself: array map lookups, regex matching, and the
`aset` that stores fn arguments into the bindings array.

Native babashka, same mixed workload as a script, minimum of 30 runs,
best of three interleaved runs:

| binary | ms per run |
|---|---|
| bb with sci master | 4.76 |
| bb with this branch | 3.96 |

That is 17%, against 33% on the JVM. Why the native image gains less was
not investigated.

## Verification

- `script/test/jvm`: 395 tests, 1536 assertions, 0 failures on Clojure
  1.10.3 and 1.11.1.
- Error locations for a duplicate key in a map or set literal, and for a
  throw inside a vector literal, are identical to master, including the
  callstack.
- `*warn-on-reflection*` reports nothing new for the analyzer.
- clj-kondo: no new findings.

## Consequences

- A literal node does one `t/eval` per child through a single call site
  in a loop. `return-call` had one site per argument position. The
  `itable stub` samples in the profile did not grow, 161 before and 151
  after, and the loop has no arity ceiling: a literal with twenty or more
  children went through `eval/fn-call` before.
- `analyze-map` still uses `map-fn` on the constant path, where it runs
  once at analysis time.
- The `__->Record__ctor__` fn stays for `new` and the four-arity case.
