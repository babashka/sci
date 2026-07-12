# ADR 0013: Bringing native protocol support to the JVM side

Status: analysis only, nothing implemented. Follows ADR 0011/0012 (CLJS).

## Question

Can sci deftypes participate in host dispatch on the JVM the way they now do
on CLJS (ADR 0011): host code calling `get`/`assoc`/`datafy`/protocol fns on
sci instances dispatching into interpreted impls? Many "protocols" on the
JVM are clojure.lang interfaces, which are closed classes. What can be faked
and at what cost?

## Half 1: real Clojure protocols - fakeable, no bytecode

JVM protocol fns dispatch via an interface fast path and then a runtime
registry keyed by CLASS (`find-protocol-impl`, populated by
`clojure.core/extend` - plain data, GraalVM-safe). The registry is the JVM
analog of the open JS object.

Obstacle: all sci deftypes share the `SciType` class, so per-sci-type
registration is impossible. Fix: transpose the CLJS design.

- `sci.lang.Type` data gets a per-type method table
  (`:sci.impl/jvm-impls`), the analog of `:sci.impl/js-prototype`.
- On first implementation of protocol P by any sci type, install ONE
  class-level bridge: `(extend SciType P {:m (fn [this & args] ...)})`.
  The bridge looks up `(-get-type this)` -> Type table -> sci impl, and
  throws missing-protocol on a miss. Idempotent (one bridge per
  protocol+class).
- `extend-type` is retroactive for free (the bridge reads the table live).
  reify routes through `getMethods`; records through `SciRecord` + the same
  bridge.

Easier than CLJS in one way: JVM protocols are runtime maps with `:sigs`
and `:on-interface` - detection and method info need no analyzer, no macro,
no munging, and there is no `:advanced` renaming story. `copy-var`
detection can be plain runtime code.

Known wart with no clean fix: host-side `satisfies?` over-reports. The
registry is class-keyed, so once any sci type extends P,
`(clojure.core/satisfies? P other-sci-instance)` is true from host code
even when that type did not implement it. Sci's own `satisfies?` can
consult the Type table (like `:satisfies-fn` on CLJS), but host code sees
class granularity. The CLJS marker-property trick has no JVM analog.
Document as a caveat.

Payoff: host `datafy`/`nav` (Datafiable/Navigable), `reduce` via
CollReduce, IKVReduce, and arbitrary library protocols work on sci
instances. Effort estimate: on the order of the CLJS reify+defrecord
rounds combined; reuses the concepts, none of the code.

## Half 2: clojure.lang interfaces - only via build-time class provisioning

`get`/`assoc`/`count`/`seq` dispatch on ILookup/Associative/Counted/Seqable
INTERFACES. Classes are closed; babashka's native image closes class
definition at build time. Options, ranked:

1. Curated stub tier + the same Type-table bridge. Generate at bb build
   (from config) a small lattice of stub classes: map-like
   (IPersistentMap pulls in ILookup/Associative/Counted/Seqable/IObj),
   seq-like, fn-like, deref-like. Stub methods delegate through the
   per-Type table, so extend-type is retroactive and the machinery is
   shared with half 1. This upgrades bb's existing `:deftype-fn`
   pre-compiled-combo approach from fixed methods to a live table.
   Interface inheritance keeps the combo count in the tens, not 2^N.
2. GraalVM dynamic proxies. `java.lang.reflect.Proxy` works in native
   image for interface combos registered at build; the InvocationHandler
   routes to the Type table. Same enumeration limit, but a new combo is a
   JSON config line instead of a handwritten class. Reflective-call
   overhead is noise under interpretation.
3. Plain-JVM embedders (non-native): full generality via runtime classgen
   (insn/ASM) behind the existing `:reify-fn`/`:deftype-fn` hooks,
   possibly as an optional sci-contrib artifact so sci core stays
   dependency-free.

Rejected: a mega-stub implementing every interface (lies to `instance?`,
breaks host code that branches on it - same reasoning as the CLJS
base-type rejection in ADR 0011) and running an embedded JVM (Espresso) to
regain `defineClass`.

## Suggested order if implemented

Half 1 first: self-contained, symmetric with #639, immediate wins in bb
(Datafiable, CollReduce, lib protocols). Then the bb stub-tier upgrade as
a separate babashka-side project sharing the Type-table bridge.
