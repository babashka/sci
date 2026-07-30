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
