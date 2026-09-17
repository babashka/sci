# ADR 0013: Bringing native protocol support to the JVM side

Status: half 1 implemented. Half 2 analysis only.
Follows ADR 0011/0012 (CLJS).

Implementation notes for half 1:

- `copy-var`, `copy-var*` and `copy-ns` wrap host protocols in protocol
  entries. `new-var` preserves raw protocol maps.
- Entries contain `:protocol`, `:name`, `:ns`, `:methods`, `:sigs`,
  `:native-methods` and `:satisfies-fn`.
- `:sci.impl/jvm-impls` stores implementations on each sci type, keyed by
  host protocol var and method symbol. Existing instances see later extensions.
- The first implementation installs bridges on `SciType`, `SciRecord` and
  `ICustomType`. The interface covers custom reify factories.
- Copied method functions dereference the host var on each call because
  `clojure.core/extend` replaces method roots.
- For sci types, `satisfies?` checks the type's implementations and host
  fallbacks. `extends?` checks the type's implementations. Host values use
  Clojure's protocol predicates.
- Extending host classes or nil requires `:unrestricted true` and changes
  dispatch throughout the host program.
- Native protocol methods use the same field bindings as sci protocol methods.
- Host-side `clojure.core/satisfies?` answers by class. Once a protocol is
  bridged it returns true for every instance of the bridged classes.
- A sci instance without an implementation uses the host implementation for
  `Object` or an interface. The lookup result is cached per class until the
  protocol root changes.

Measurements for half 1, 1M-call loops, medians of seven runs:

- Host fallback: 3648 ms before the per-class cache, 120 ms after.
- Bridged sci-to-sci call: 69 ns with a varargs bridge, 61 ns with fixed
  arities. A sci protocol call is about half of that. The rest is the lookup
  from instance to type to implementation table behind Clojure's dispatch.
  A JVM class per sci type would remove this lookup. See ADR 0016.

## Question

Can sci deftypes participate in host dispatch on the JVM the way they now do
on CLJS (ADR 0011): host code calling `get`/`assoc`/`datafy`/protocol fns on
sci instances dispatching into interpreted impls? Many "protocols" on the
JVM are clojure.lang interfaces, which are closed classes. What can be faked
and at what cost?

## Half 1: Clojure protocols

JVM protocol functions dispatch through an interface or a registry keyed
by class. `clojure.core/extend` updates this registry without generating
bytecode.

Sci deftypes share the `SciType` class. A class-level bridge dispatches
to each sci type's implementation:

- `sci.lang.Type` data gets a per-type method table
  (`:sci.impl/jvm-impls`), the analog of `:sci.impl/js-prototype`.
- The first implementation installs a bridge on each sci instance class.
  The bridge reads the type's method table, then checks host fallbacks.
  It throws if neither provides an implementation.
- `extend-type` updates the table used by existing instances.
  Reify dispatch uses `getMethods`. Records use the `SciRecord` bridge.

JVM protocols are runtime maps with `:sigs` and `:on-interface`.
Copying a protocol reads its method names from this map.

Host-side `satisfies?` checks the class registry. Once any sci type extends P,
`(clojure.core/satisfies? P other-sci-instance)` is true from host code
even when that type did not implement it. Sci's own `satisfies?` checks
the type's implementations or the reify protocol set, then host fallbacks.

An optional workaround for JVM embedders would replace the root of
`clojure.core/satisfies?` with `alter-var-root`. The replacement would
check the type table for `SciTypeInstance` values and the protocol set
for `ICustomType` values, then host fallbacks. Other values would use
the original function. SCI does not provide this replacement.

The replacement would use `(:var protocol)` to identify the host protocol
without a sci context. Each call through the var would add up to two
`instance?` checks. Direct-linked calls bypass the var, so this workaround
would not affect those calls in babashka.

Host functions such as `datafy` and `nav` can call protocol implementations
on sci instances. JVM interfaces such as `IKVReduce` require separate
support, described below.

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
