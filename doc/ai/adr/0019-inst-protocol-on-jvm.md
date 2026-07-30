# ADR 0019: `clojure.core/Inst` on the JVM

Status: accepted (2026-07-30). Fixes babashka/babashka#1321.

## Context

`inst?` and `inst-ms` were plain `copy-core-var`s, so they only ever saw types
that had extended the host `clojure.core/Inst` protocol. `Inst` itself was not
mapped in, so

```clojure
(defrecord Foo [t] Inst (inst-ms* [_] t))
```

failed with "Protocol not found: Inst", and so did `extend-protocol`.

On CLJS this already worked: `Inst` is in the `protocol-vars` list in
`sci.impl.namespaces` and CLJS protocol entries are installed natively on the
sci type's JS prototype (ADR 0011/0012), so `cljs.core/inst-ms` and
`cljs.core/inst?` dispatch into sci impls without any rerouting.

The JVM has no such mechanism. A general JVM native-protocol tier is sketched
in ADR 0013 and not implemented.

## Decision

Hand-roll `Inst` the way `IDeref` and `IAtom` are hand-rolled in
`sci.impl.core-protocols`:

- `inst-ms*` is a host `defmulti` dispatching on `types/type-impl`, with a
  `:sci.impl.protocols/reified` method for reify and a `:default` method that
  calls `clojure.core/inst-ms`, so host types keep working and the
  no-implementation error message stays Clojure's.
- `inst-protocol` is a sci var holding
  `{:protocol clojure.core/Inst :methods #{inst-ms*} :ns clojure.core}`,
  mapped into `clojure.core` as `Inst`, with `inst-ms*` next to it.
- `clojure.core/inst-ms` is rerouted to the multimethod.
- `clojure.core/inst?` is rerouted to `sci.impl.protocols/satisfies?` on the
  protocol map. That covers host instants through `:protocol` and sci types
  through the multimethod's method table.

This is what makes `defrecord`, `deftype`, `reify`, `extend-type`,
`extend-protocol` and `extend` all work, since each of them ends up emitting
`(defmethod clojure.core/inst-ms* <type> ...)`.

## Second half: extending the host protocol in unrestricted contexts

The multimethod alone only serves sci code. Compiled host code calls
`clojure.core/inst-ms` directly and never sees the sci implementation, so
`(clojure.core/inst-ms sci-record)` throws.

How much this matters in babashka today: nothing observable. The one candidate
consumer, `clojure.spec.alpha/inst-in`, turns out to be loaded as sci source
from a bundled resource, not compiled, so it already worked. The value is for
embedders whose compiled code consumes a protocol, and for the mechanism,
which applies to any host protocol mapped into sci, not just `Inst`.

CLJS solves the same problem by installing the impls on the sci type's own JS
prototype, so `cljs.core/inst-ms` finds them. The JVM analog of "the type's
own prototype" does not exist: every sci record is a
`sci.impl.records.SciRecord` (final, so not even subclassable), every deftype
a `sci.impl.deftype.SciType`, and reify instances come from a shared
pre-compiled pool. Per-type registration in the host is impossible without
per-type classes.

The mechanism, taken from grease
(https://github.com/phronmophobic/grease/blob/main/src/com/phronemophobic/grease/scify.clj),
is to replace the root of the host protocol METHOD var with the multimethod
that already holds the per-type sci impls:

```clojure
(alter-var-root #'clojure.core/inst-ms* (constantly inst-ms*))
```

The previous root is captured first and becomes the multimethod's fallback for
host types (going through `clojure.core/inst-ms` instead would come straight
back to the multimethod). This works on compiled callers because protocol
method vars are rebound via `bindRoot` at runtime by `-reset-methods`, so call
sites are never direct-linked to them - the same property that makes host
`extend` work in direct-linked code. Grease uses this in GraalVM native
images.

`sci.impl.utils/install-host-protocol!` does the swap for protocol maps
carrying a `:host-swap` map of host var to `{:multi multifn :capture-root!
fn}`. It is called from `extend-protocol`, `extend-type`, `extend`,
`analyze-defrecord*`, `analyze-deftype*` and `reify*`, so a program that never
touches a bridged protocol never mutates anything.

Gated on `:unrestricted`. It mutates a var of the embedding program, which a
sandboxed context must not be able to do. babashka sets `:unrestricted true`.

Known interaction: a host-side `extend` of the same protocol calls
`-reset-methods`, which rebinds the method var and undoes the swap. Every sci
implementation path re-asserts the swap (`identical?` check on the root) and
re-captures the new root, so the host extension stays visible through the
fallback and sci dispatch is repaired by the next sci-side implementation of
the protocol. In between, host calls on sci values throw the
no-implementation error, they never silently misdispatch.

### Rejected variant: extending the registry to the shared sci classes

The first cut (commit `bad94a13`) used `clojure.core/extend` on
`SciTypeInstance` and `ICustomType` with a delegating fn map. It dispatches
correctly, but the protocol registry is class-keyed, so once one sci type
implemented `Inst`, host `(inst? x)` answered true for EVERY sci record, type
and reify, and `(extenders Inst)` showed sci internals. The var swap gets the
same dispatch without touching the registry: host `satisfies?` on sci values
answers false across the board. That is still wrong for implementing types,
but under-reporting is the safe direction, host code guarded by `(inst? x)`
skips sci values instead of crashing on them, and it lies about nothing. Sci's
own `inst?` consults the multimethod's method table and stays exact on both
sides of the boundary.

Exact host-side `satisfies?` needs the instance's class to implement the
protocol's backing interface (`clojure.core.Inst`), i.e. per-type or per-combo
classes: the build-time stub tier of ADR 0013, or runtime emission via Crema
(ADR 0016). Out of scope here.

## Caveat: the protocol var and its multimethod are global

`inst-protocol` and `inst-ms*` are `def`s in the sci jar, not per-context
values, and `Inst` is now reachable from every context as `clojure.core/Inst`.
Two consequences for embedders that run more than one context in one JVM:

- `extend-protocol Inst` conj's `(type->str type)` onto `:satisfies` on the
  shared var. Type names are strings, so a `user.Foo` extended in one context
  makes `(satisfies? Inst other-user-Foo)` true in another. `inst-ms` still
  throws there, because the multimethod is keyed by the `sci.lang.Type`
  object, which is identity-compared.
- Extending `Inst` to a host class registers a method under that class, which
  is shared, so the implementation is visible from every context.

Sci types are unaffected: `sci.lang.Type` is a `deftype`, so per-context types
never collide in the method table.

The same would hold for `IDeref` and `IAtom` if they were extendable, but they
live in the private `clojure.lang` namespace and `extend-protocol` cannot
resolve `(var clojure.lang.IDeref)`, so today `Inst` is the only globally
mutable protocol on the JVM side.

Making it per-context means creating the multimethod inside the context, which
is the ADR 0013 half 1 project, not this fix.

## Pre-existing, not addressed

`extends?` is `(some #(get-method % atype) (:methods protocol))` and
`get-method` falls back to `:default`, so `(extends? P AnyType)` is true for
every sci protocol that has a `:default` method. That is every protocol
created with `defprotocol` in sci, not something `Inst` introduces.
