# ADR 0011: Native CLJS protocol implementations on sci deftypes (issue #639)

Status: implemented (branch `issue-639-native-cljs-protocols`), CLJS only.

## Problem

SCI protocols are multimethods. A sci `deftype` implementing e.g. `ILookup`
was previously (a) impossible for real cljs.core protocols (they weren't
exposed at all) and (b) invisible to the host: `(get sci-instance :k)` in
compiled CLJS code never dispatched into sci. Issue #639 asks for host
protocol participation without the multimethod overhead, gated on the
embedder opting in.

## Design

### Opt-in via `sci/copy-var` on a protocol

```clojure
(def cljs-core-ns (sci/create-ns 'cljs.core))

(sci/init
 {:namespaces {'clojure.core {'ILookup (sci/copy-var ILookup cljs-core-ns)}}})
```

No new API: on CLJS, `copy-var` detects protocol vars (`:protocol-symbol`
analyzer info) and emits the protocol entry (wrapped in a sci var) instead
of copying the raw protocol object. Embedders that already `copy-var`
protocols (nbb copies ILookup, IMeta, ISeq, INamed, ...) upgrade without a
code change. The raw copy was inert anyway: implementing, extending or
`satisfies?`-ing such a protocol errored. `cljs.core/IFn` skips detection
and stays a raw copy (implemented on SciType at the class level, and its
arity/variadic call convention does not fit per-arity slot setters). An
earlier revision exposed this as a separate `copy-protocol` macro, folded
into `copy-var` before release to avoid a second spelling of "copy a var".

Providing the entry is the opt-in, no `:classes` gate. The feature mutates
no host objects: method properties land on sci-owned per-type prototypes,
the protocol object, protocol fns and host prototypes are untouched. Method
impls are sci fns and stay sandboxed. Host `get`/`assoc` running interpreted
code for sci-created values is ordinary polymorphism, equivalent to handing
the host a sci fn as callback. An earlier revision gated on
`{:classes {:allow :all}}`, rejected: it forced embedders to open all JS
interop for a capability that needs none.

Like `copy-var`, the ns is an explicit argument (a `sci/create-ns` object),
shared across entries so metadata reflects on one object instead of each
expansion minting its own `Namespace`.

For a protocol, `copy-var` expands, in the **embedder's build**, to a
protocol entry map (wrapped in a sci var):

```clojure
{:protocol ILookup
 :name 'cljs.core/ILookup
 :ns cljs-core-ns                                   ; the ns argument, verbatim
 :methods #{}                                       ; compat with sci protocol maps
 :satisfies-fn (fn [x] (satisfies? ILookup x))      ; compiled satisfies?
 :marker-setter (fn [o] (set! (.-cljs$core$ILookup$ o) cljs.core/PROTOCOL_SENTINEL))
 :native-methods {'-lookup {:setters {2 (fn [o f] (set! (.-cljs$core$ILookup$_lookup$arity$2 o) f))
                                      3 ...}}}}
```

Key point: the setters are `set!` **property-access forms compiled in the
    embedder's build**, so Closure renames them consistently with cljs.core
itself. This is what makes the feature work under `:advanced` — computing
`"cljs$core$ILookup$_lookup$arity$2"` as a runtime string would silently
break there. Method names/arities come from the analyzer at macro time
(`(:protocol-info (cljs.analyzer.api/resolve &env psym))`); property names
mirror `cljs.core/protocol-prefix` + `(munge (name method))` + `$arity$N`.

This extends the pre-existing `{:protocol IDeref ...}` entry shape in
`core_protocols.cljc` / the `satisfies-host-protocol` test; nothing outside
sci used `:protocol` entries (checked nbb, scittle, sci.configs), so the
shape was free to grow. `cljs.core/IFn` is rejected (sci's `SciType`
already implements `IFn`; IFn slots have a different convention incl.
`.call`/`.apply`).

### Per-type JS prototypes

All sci deftype instances share the single `SciType` class, so protocol
properties cannot go on its prototype. Instead `-create-type`
(namespaces.cljc) now gives every CLJS deftype its own prototype:
`(js/Object.create (.-prototype SciType))`, stored in the `sci.lang.Type`
data under `:sci.impl/js-prototype`. `->type-impl` constructs instances via
`js/Object.create` of that prototype + `(.call SciType obj ...)` so SciType's
field assignments (and inherited IFn/print/ICustomType behavior) still apply.

Consequences:
- native method install is per **type**, once, zero per-instance cost;
- `extend-type` after the fact is retroactive (matches CLJS semantics),
  which is why the prototype is created eagerly for every deftype, not
  lazily on first native install;
- records (`SciRecord`) are untouched; native protocols on `defrecord`
  throw "not yet supported".

### Slot calling convention (gotcha)

CLJS invokes protocol slots as `o.slot(null, k)` (`^not-native` sites) or
`o.slot(o, k)` (protocol-fn fast path): the receiver is the JS `this`, and
the first positional arg may be null. Compiled deftype methods use `this-as`
internally. Sci impl fns take `this` as a regular first arg, so
`-install-native-protocol!` wraps each impl in a `with-js-this` adapter
(`(fn [_ a] (this-as self (impl self a)))`) before installing.

### Analyzer integration

- `analyze-deftype*` (CLJS branch): a protocol entry with `:marker-setter`
  takes the native path — method bodies get the same field-binding
  transform as the defmethod path (shared `transform-bodies`), and the
  emitted form is `(sci.impl.deftype/-install-native-protocol! user.Foo
  ILookup {method {:arities #{2 3} :impl (fn ...)}})`.
- The Type object is created at ANALYSIS time by `analyze-deftype*` and
  registered in `:types`; `-create-type` adopts it at eval time (setVal
  merges in the constructor var and, on CLJS, the JS prototype). This is
  what lets method bodies and the install form reference the type symbol
  (`user.Foo`, `(instance? Foo x)`) during eager analysis while staying
  identical to the object instances carry. An earlier revision instead
  relied on `lazy-seq` to defer analysis until after `-create-type` had
  evaluated in the emitted top-level `do` — rejected as an implicit
  ordering dependency; a fresh Type at eval time also breaks identity for
  anything analyzed earlier (this replaced the previous non-top-level
  placeholder Type, which had exactly that divergence).
- `extend-type` / `extend-protocol` (protocols.cljc): native entries emit
  `(sci.impl.protocols/-extend-native! Type proto impls)`; sci-type targets
  only for now (throws otherwise, records included). `extend` (the fn)
  throws for native entries. Like Clojure/CLJS, extend-type method bodies do
  NOT get field bindings. Since sci deftype fields live in the ext-map, not
  as JS properties, `(.-field this)` in a native extend-type body does not
  reach a field. Field access is available in the deftype body only.
- `satisfies?` uses `:satisfies-fn` when present, generalizing the
  hardcoded IDeref/ISwap/IReset/IRecord/IFn condp.

## Verification

`test/sci/native_protocols_test.cljs`: host-side get/assoc/count/contains?/
satisfies?/implements?, sci-side equivalents, fields + mutable fields
(`set!`), multi-arity methods, retroactive extend-type/extend-protocol,
sandboxed ctx (no :classes opt), coexistence with sci defprotocol,
defrecord error.
Full node suite passes under **both `:none` and `:advanced`** (the advanced
run is the proof of the compiled-setter munging story), JVM suite and
clj-kondo clean vs master.

## Not covered (future work)

- defrecord and reify throw "not yet supported" for native protocol
  entries (reify would otherwise succeed silently with broken dispatch).
- base types (`extend-type string ...` via goog.typeOf
  string keys on protocol/method-fn objects — advanced-safe, would need
  per-method `:fn` refs in the entry, deliberately not emitted today to
  keep bundles DCE-friendly), JS classes via `:classes`.
- Direct method vars (`(-lookup x k)` in sci code) — embedder can add
  `'-lookup -lookup` to `:namespaces` manually.
- Self-hosted CLJS: macro has the branch (mirrors `copy-ns`), untested.

## Open question: should `copy-ns` include host protocols?

`copy-ns` already copies protocol vars, but as sci-style entries (the
`:protocol-symbol` path), not native protocol entries. Open question
whether `(copy-ns 'cljs.core ...)` should turn protocols into native
entries, automatically or via an option.

Perspective: not automatically, but the reason is narrower than it first
looks. A `copy-ns`'d protocol is NOT a functional protocol today: `copy-ns`
copies the protocol var (the map object) but wires up no method multifns and
does not expose the method symbols. Verified: copy-ns a `defprotocol`, then
`(deftype T [] P (m [_] ...))` fails with "Unable to resolve symbol: m",
extend-type to a base type errors, and `satisfies?` on an implementer cannot
even be written. The only host protocols that support the full sci machinery
(extend-type, base-type extension, `:default` fallback) are the hand-wired
ones in `core_protocols.cljc` (IDeref, ISwap, IReset, IPrintWithWriter,
IFn), which carry real sci multifns. So making copy-ns protocols native
would ADD capability, not regress it. Two reasons still argue against doing
it automatically:

1. Bundle size. cljs.core has dozens of protocols (ISeq, ICollection,
   IIndexed, IMap, ISet, IReduce, IKVReduce, ...). Auto-native emits
   per-method setter fns plus a satisfies closure for each, into every
   `copy-ns`, even when the embedder never implements them natively. Same
   DCE concern that dropped `:fn` from the entry.
2. Collisions. protocol detection skips IFn; IDeref, ISwap, IReset,
   IPrintWithWriter, IFn are already hand-wired in `core_protocols.cljc`.
   Auto-native would double-map them, so a sweep would have to skip or
   reconcile the hand-wired set.

Both reasons are cljs.core-specific. Bundle size only bites when sweeping a
huge namespace, and embedders do not `copy-ns 'cljs.core` anyway (cljs.core
is the interpreter's own core). The collision set is also cljs.core-only.

TODO (post-merge, user namespaces): make `copy-ns` emit protocol entries for
protocol publics on CLJS. For a user namespace this is a pure, no-collision
addition. Today copy-ns half-does it: it already copies the protocol method
fns (`m` from `(defprotocol P (m [_]))`) as the real host fns, but no sci
instance carries the slots, so `(m sci-instance)` fails and the protocol is
inert (implement/extend/satisfies all error, verified). Emitting
the protocol entry for the `P` var adds the slots to implementing sci deftypes,
so the already-copied host `m` dispatches into the interpreted impl. The two
halves finally meet. Mechanics: in the copy-ns CLJS branch, detect
`:protocol-symbol` publics and emit the protocol entry expansion (shared with `copy-var`) for the
protocol var, leaving method vars as plain copies. Default-on is defensible
(no collision, strictly additive, DCE not a concern for copy-ns). Confirm no
protocol shape trips the macro first (extend-via-metadata, non-defprotocol
`:protocol-symbol` vars; marker/zero-method protocols already work).

For cljs.core specifically, if ergonomics are wanted, a `copy-ns` option
`:native-protocols [ILookup IAssociative]` (or `:all`), default off, opting
named protocols into native entries while the rest keep current semantics,
and reconciled with ADR 0012 (which migrates the hand-wired core protocols).
`protocol-entry-form` stays the primitive it builds on. nbb wires the flagship
protocols explicitly today (`ILookup`, `IAssociative`, `ICounted` via
`copy-var`), which is enough to validate the mechanism.
