# ADR 0012: Migrate CLJS core protocols to the native mechanism

Status: implemented (branch `adr-0012-core-protocols`). Builds on ADR 0011.

Implementation notes (delta from the plan below):
- The protocol-entry machinery moved from sci.core to `sci.impl.copy-vars`
  (deftime, `protocol-entry` macro) so `core_protocols.cljc` can build the
  IDeref/ISwap/IReset/IPrintWithWriter entries without a dependency cycle;
  sci.core's copy-var/copy-ns use it from there.
- The method vars sci exposes (`-deref`, `-swap!`, `-reset!`, `-pr-writer`)
  now hold the real cljs.core protocol fns instead of the multimethods.
- `deref`/`swap!`/`reset!` on CLJS are plain `copy-core-var` copies; the
  `deref*`/`swap!*`/`reset!*` re-routing wrappers are CLJ/cljd only now.
- Deleted on CLJS: the `-deref`/`-swap!`/`-reset!` defmultis with their
  `:sci.impl.protocols/reified` and `:default` methods, the
  `types/sci-pr-writer` `:reified` method and `-pr-writer*`, and the
  IDeref/ISwap/IReset rows in `satisfies?`'s condp (IRecord and IFn stay).
- Host-side payoff verified in tests: `@`, `swap!`, `reset!` and `pr-str`
  on sci deftype/reify instances now work from compiled CLJS.

## Context

ADR 0011 added native CLJS protocol support: a sci `deftype` implementing a
protocol entry (copy-var on a protocol) gets the protocol's method slots installed on a
per-type JS prototype, so host `get`/`assoc`/`count`/`satisfies?` dispatch
directly into the interpreted impls.

Before that, the only host protocols sci could implement were the ones
hand-wired in `core_protocols.cljc`: IDeref, ISwap, IReset,
IPrintWithWriter, IFn. Each is a defmulti keyed on `type-impl`, with a
`:sci.impl.protocols/reified` method that reads `getMethods`, a `:default`
that falls back to `clojure.core`, and a wrapper (`deref*`, `swap!*`,
`reset!*`) that fast-paths host atoms. This machinery is CLJS+CLJ shared via
reader conditionals.

The native mechanism is both cleaner and more capable than the hand-wired
multimethods on CLJS. Proof of concept (exposing IDeref via a protocol entry
instead of the hand-wired multimethod): `@instance` dispatches into the
interpreted `-deref` both inside sci AND from host CLJS code, and
`satisfies? IDeref` works. The current multimethod path does not give
host-side `@` on sci instances at all.

## Decision

Migrate the CLJS side of the hand-wired core protocols to native
protocol entries, delete the CLJS multimethod machinery, and let all
CLJS protocol participation go through per-type prototypes. Keep the JVM side
on multimethods (no JS prototypes on the JVM).

IFn is excluded: copy-var protocol detection skips it and `SciType`/`Reified`
implement IFn directly at the class level. It stays as-is.

## Prerequisites

1. Native reify: DONE (branch `native-reify`). reify* installs implemented
   slots directly on the Reified instance via
   `-install-native-protocol-on!` (specify!-style, no per-type prototype:
   reify results are not extend-type targets). The reify macro passes a
   per-method implemented-arities map so only implemented arities get slots.
2. Native defrecord: DONE (branch `native-defrecord`). Every record type
   gets a prototype chaining to `SciRecord.prototype` (`-create-type` picks
   the base class by the `:sci.impl/record` flag); `->record-impl`
   constructs via `Object.create` + `SciRecord.call`. Own-prototype slots
   shadow the class-level map protocols, so a record can override e.g.
   ICounted while other records keep the default. Key wrinkle found:
   `-assoc`/`-dissoc`/`-with-meta`/`-clone` used `(new SciRecord ...)`,
   which would DROP the per-record prototype; they now go through
   `clone-record*` (`Object.create (getPrototypeOf this)` + ctor `.call`)
   so derived instances keep their native slots. extend-type on record
   types is allowed and retroactive.

Both prerequisites are done: the migration steps below are unblocked.

## Migration steps (CLJS only), in order

1. Land native reify.
2. Land native defrecord.
3. Replace the `clojure.core` entries for IDeref, ISwap, IReset,
   IPrintWithWriter in `namespaces.cljc` with protocol entries (CLJS
   branch).
4. Delete the CLJS branches of the `-deref`/`-swap!`/`-reset!` defmultis,
   their `:sci.impl.protocols/reified` methods, and the `getMethods`
   indirection in `core_protocols.cljc`. Keep the `#?(:clj ...)` branches.
5. Collapse `deref*`/`swap!*`/`reset!*` on CLJS toward plain
   `cljs.core/deref`/`swap!`/`reset!`, keeping the host-`Atom`/`IAtom`
   fast path only if it measures.

## Payoff

- Deletes a large block of `core_protocols.cljc` CLJS branches.
- One uniform CLJS model: every protocol participation via prototypes.
- Host-side `@`, `swap!`, `pr-str`, and protocol-fn calls start working on
  sci instances, not just sci-internal routing.

## Watch items before flipping

- Confirm plain `cljs.core/deref` works on `sci.lang/Var` and any internal
  type that currently rides the `-deref` `:default` -> `clojure.core/deref`
  fallback.
- `extend-protocol IDeref` / `IPrintWithWriter` to host base types via the
  old defmethod path is dropped on CLJS (native supports sci types only).
  Confirm no embedder relies on it, or keep those specific protocols
  hand-wired.
- IPrintWithWriter: a per-type native `-pr-writer` shadows the existing
  `sci-pr-writer` `:default` defmethod. Confirm default printing still holds
  for types that do not implement it.
