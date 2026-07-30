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

## Usage research (2026-07-30)

Before settling on a design, every implementation of `Inst` reachable locally
was classified: all 8148 jars in `~/.m2` plus `~/dev` and `~/.gitlibs`,
searched with grasp for `extend-protocol`/`extend-type`/`extend`/`defrecord`/
`deftype`/`reify` forms involving `Inst` or `inst-ms*`. Findings:

- Dominant shape, by far: `extend-protocol Inst <host class>`. clojure core
  (Date, Instant), clojurescript core plus four CLJS libs (js/Date), clj-time
  (org.joda.time.ReadableInstant), clojure-future-spec (Date, Instant),
  promesa (Duration).
- The ONLY library implementing Inst on its own type is promesa:
  `deftype Task [pt/ICancellable clojure.core/Inst IFn]` and
  `deftype ExecutorBulkheadTask [clojure.core/Inst Runnable]`. Both are combo
  shapes with host interfaces, which babashka's deftype cannot host anyway.
- Nobody uses defrecord-inline, reify, extend-type, extend or metadata
  extension with Inst.

Consequences: a per-protocol sibling class (`SciRecordInst`) was rejected
before this research on scaling grounds (2^N combos), and the research
confirms the only real sci-type usage is exactly a combo it cannot express.
More importantly, the dominant shape targets HOST classes, where the registry
is keyed on a real, distinct class - so a genuine `clojure.core/extend` on
that class gives exact dispatch AND exact host `satisfies?`, with no
granularity problem at all. The granularity wart only ever applied to
sci-defined types sharing SciRecord/SciType.

## Approach E (implemented): split by extension target

Branch `issue-1321-inst-target-split`. On top of C:

- Extension target is a host class (`extend-protocol Inst java.time.Duration`
  in sci): `sci.impl.protocols/-extend-host-class!` writes a genuine host
  registry entry on that exact class, unrestricted contexts only. The entry
  delegates through `get-method` on the multimethod so sci-side redefinition
  stays live, and throws when the method was removed (falling through to the
  multimethod default would recurse into the host protocol fn).
  `clojure.core/extend` calls `-reset-methods`, which rebinds the method
  vars, so when the var swap was active it is re-asserted afterwards.
- Extension target is a sci type: C's method var swap, unchanged. Programs
  that only extend host classes never install the swap, so the Date fast path
  stays untouched for the dominant shape.
- `-bridge-host-protocol!` picks the branch; it is emitted into
  extend-protocol/extend-type expansions and called from `extend`. The
  deftype/defrecord/reify paths keep calling `install-host-protocol!`
  directly - their targets are always sci types.
- Protocols opt in with `:host-class-impls` (method keyword -> multimethod)
  next to `:host-swap`. Both generic: babashka's Datafiable/Navigable can
  adopt them.

## Decision matrix

A = sci-only multimethod (`issue-1321-inst-protocol`), B = registry extend on
the shared sci classes (`issue-1321-inst-host-protocol`), C = method var swap
(`issue-1321-inst-var-swap`), E = target-split
(`issue-1321-inst-target-split`, implemented). All build on A, sci-side
behavior is identical throughout. Shape frequency from the research above.

| Criterion | A | B | C | E |
|---|---|---|---|---|
| sci-side: all implement forms, exact `inst?`/`satisfies?` | GOOD | GOOD | GOOD | GOOD |
| host `inst-ms` on sci-type impls (rare) | bad | GOOD | GOOD | GOOD |
| host `inst-ms` on host-class extensions (dominant) | bad | bad | GOOD | GOOD |
| host `inst?` on host-class extensions (dominant) | bad (false) | bad (false) | bad (false) | GOOD (exact) |
| host `inst?` on an implementing sci type (rare) | bad (false) | GOOD | bad (false) | bad (false) |
| host `inst?` on a non-implementing sci value | GOOD | bad (lies) | GOOD | GOOD |
| registry holds only genuine entries | GOOD | bad | GOOD | GOOD |
| survives a later host-side `extend` | n/a | GOOD | bad (window) | GOOD for host classes, self-healing window for sci types |
| host-type call path (Date) unchanged | GOOD | GOOD | bad (hop) | GOOD until a sci type implements the protocol |
| no mutation of host state | GOOD | bad | bad | bad |
| moving parts | GOOD | bad | bad | bad |

The one bad cell E keeps - host `inst?` false for an implementing sci type -
corresponds to one library's pattern in the entire corpus, which babashka
cannot run for independent reasons. Every shape that occurs in the wild lands
all-GOOD. Exact host `satisfies?` for sci types would need per-type classes:
the build-time stub tier of ADR 0013 or Crema (ADR 0016).

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
