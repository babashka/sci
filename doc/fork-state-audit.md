# Forkable runtime state audit

This audit describes the experimental fork implementation as of `09f1174`.
It distinguishes heap/world state from dynamic control state and external
resources. A value being reachable through an SCI Var does not by itself make
all of its internal state fork-local.

## Current state model

An SCI context currently has three relevant layers:

1. The context environment is an atom containing persistent namespace maps.
   `sci/fork` creates a new atom with the same map value.
2. A lineage registry maps stable handles to dense slots. Var roots and
   metadata use two slots; SCI-created atoms and volatiles use one value slot.
3. Dynamic binding frames are control state outside the world. On the JVM they
   live in a `ThreadLocal`; on CLJS and ClojureDart they live in a process-local
   volatile.

A Var read therefore has this precedence:

```text
binding frame contains Var? ── yes ──> mutable TBox value
             │
             no
             ↓
primary evaluation? ── yes ──> direct Var root
             │
             no
             ↓
forked world's resolved dense slot
```

The environment and dense world are forked. The binding frame and host object
internals generally are not.

## Dynamic bindings

SCI closely follows Clojure's `Var.java` representation:

- A `Frame` contains a complete persistent map from Var identity to `TBox` and
  a pointer to the previous frame.
- `push-thread-bindings` starts with the current complete map, associates a new
  box for each rebound Var, and pushes the new frame. `pop-thread-bindings` is
  O(1).
- A `TBox` contains the creating JVM thread and a mutable value. `set!` mutates
  the box only on that thread. Setting a conveyed binding from a future is
  rejected, matching Clojure.
- A Var has a shared `thread-bound` fast-path bit. Once any binding has existed,
  reads check the current frame before reading a root.
- `bound-fn*` captures binding values and creates fresh boxes when invoked.
  `binding-conveyor-fn`, used by SCI futures, clones the current frame header
  and shares its boxes, like Clojure's conveyor behavior.

### Present fork semantics

`sci/fork` is a world snapshot, not an execution or continuation snapshot. It
does not capture a dynamic frame. It also cannot be called from inside an
active evaluation of the source world, because the evaluation holds that
world's read permit and the fork requires its write permit.

Binding frames are keyed only by stable Var identity, not by world identity.
Consequently a binding active on one thread overrides that Var while the thread
performs a nested evaluation in another world from the same lineage. This is
coherent with ordinary Clojure, which has one root world, but interacts badly
with fork-local metadata: a Var made `^:dynamic` only in a child can be bound in
the child and the binding is then observable by a nested parent evaluation,
even though the parent still considers the Var non-dynamic. The roots remain
isolated after the binding exits.

The audit also found an async correctness gap: SCI's future wrapper conveyed
the dynamic frame, but did not install the captured context's `ReadWorld` or
take the world's evaluation permit. In the original probe, the dynamic binding
was conveyed correctly while `swap!` changed the parent atom realization:
parent `1`, child `0`. `binding-conveyor-fn` now captures the SCI context and
runs the callback with that world installed and permitted. A running Future is
still a non-copyable host resource.

CLJS and ClojureDart use one volatile binding-frame stack rather than an
async-local facility. Interleaved asynchronous evaluations can therefore
replace one another's frames even without forking.

### Recommended control-state model

Keep two APIs and make their distinction explicit:

- `fork` remains a quiescent heap/world snapshot and captures no running
  bindings.
- A future `fork-execution` operates only at a suspension point and snapshots
  `(world, binding-frame, continuation, resource capabilities)`.

Dynamic frames should belong to an evaluation/fiber token rather than an
unqualified process thread. A practical persistent representation is a small
frame header containing a full persistent `Var -> value` map plus the previous
frame. `set!` replaces the current header's map with an `assoc`; a fiber fork
can share the immutable map in O(1). An ownership/conveyance flag can retain
Clojure's rule that a binding conveyed to another thread cannot be assigned.

Before changing representation, we must choose whether an explicit host
binding is lineage-wide or world-specific. SCI `(binding ...)` should at least
be evaluation/world-specific so child-only dynamic metadata cannot affect a
nested parent evaluation accidentally.

## Primitive and runtime-state inventory

| Facility | Current fork behavior | Required classification or change |
|---|---|---|
| Var root and metadata | Dense world-local slots | Implemented |
| Namespace maps, aliases, imports | Persistent map in a new environment atom | Implemented; contained handles remain shared intentionally |
| Global hierarchy value | Immutable map held in a world-local Var root | Implemented |
| SCI-created atom value | Stable host handle, world-local value slot | Implemented |
| SCI-created volatile value | Stable host handle, world-local value slot | Implemented |
| Atom validator, watches, metadata | Stored on shared host atom | Partial: registrations and metadata leak across worlds |
| Var watches | Stored in the shared Var handle | Partial: registrations leak across worlds |
| Namespace metadata | Mutable field on shared Namespace handle | Shared unintentionally |
| Type metadata and mutable deftype fields | Mutable fields on shared Type/SciType handles | Shared unintentionally |
| `*loaded-libs*` | Built-in Var root points to one host Ref/atom | Shared unintentionally despite fork-local namespace maps |
| Delay | One host Delay object and realization cache | Shared; forcing in one world suppresses computation in another |
| Lazy sequence realization | One host lazy cell/cache | Shared and world selection during deferred realization is unreliable |
| `memoize` cache | Host function closes over an untracked atom | Shared; a child cache hit can suppress parent computation |
| Promise | One host delivery cell | Shared; child delivery is immediately visible in parent |
| Future / async task | Work launched through the binding conveyor runs in its captured world; the host task itself remains shared | Not copyable; needs an explicit fork policy |
| Transient collection | Same affine host object | Shared; mutation in child is visible in parent |
| Array / JS object / mutable host collection | Same host object | Shared unless it implements `Forkable` or `:fork-fn` copies it |
| Multimethod tables and caches | Mutable internals of one MultiFn root | Shared; a child `defmethod` changes parent dispatch |
| Protocol/type extension registries | Mutable Type/protocol realization objects | Shared in several host-specific paths |
| Record hash caches | Shared memoized hash only | Benign derived state if records remain immutable |
| Watches with external effects | Shared callback registrations | Must be copied, shared, or prohibited explicitly by capability |
| RNG, `gensym`, clock, UUID | Host/global nondeterministic source | External capability; not reproducibly forked |
| Readers, writers, streams, iterators, regex matchers | Mutable/consuming host resource | Affine or explicitly shared; never generically copied |
| Files, sockets, executors, locks | External host resource | Explicitly shared, virtualized, or prohibited |

The table concerns state created or exposed by SCI itself. Application-owned
containers remain responsible for their nested graph through
`sci.fork/Forkable`; only direct world-cell values are inspected by that
protocol.

## Reproduced cross-world effects

A JVM probe against the current implementation produced these results:

- forcing a child delay returned its value in the parent without running the
  delayed body in the parent;
- delivering a child promise delivered the parent promise;
- mutating a child transient or array changed the object seen by the parent;
- using a memoized function in the child populated the cache used by parent;
- a child `defmethod` installed the method in parent;
- child changes to namespace metadata and atom metadata appeared in parent;
- a watch installed in child fired for a parent atom mutation;
- a child-only dynamic binding was visible in a nested parent evaluation;
- before the conveyor correction, a future launched in child mutated the
  parent atom realization.

These are realization leaks, not failures of the dense value slots themselves.
They arise because a world-local slot often contains a stable object whose own
mutable internals have not yet been decomposed into world state.

## Dense-registry liveness

The lineage registry currently holds strong keys for every registered Var,
atom, and volatile, and `:next-slot` only increases. This has two consequences:

1. A local `(atom ...)` remains strongly reachable for the lifetime of the
   lineage even after user code drops every reference to it.
2. Vars or refs created only in a discarded child reserve slots in the shared
   lineage registry. Later allocation in a parent can expand across those
   branch-local holes, increasing all subsequent copy-on-fork costs.

Dense slots therefore need a liveness strategy. The main choices are:

- custom SCI state handles carrying their lineage/slot descriptor, with weak
  lineage bookkeeping for enumeration at fork;
- weak identity registration plus slot generations and a free list;
- branch-local registry overlays whose descriptors are promoted only when
  values escape or branches join;
- safepoint compaction that rewrites descriptors and dense arrays while all
  worlds in a lineage are quiescent.

Self-describing handles are attractive for SCI-owned atoms, promises, delays,
and multimethods because they also remove descriptor-map lookup from their hot
paths. Host values still require the capability protocol or an identity side
table.

## State-machine designs beyond Vars

### Atom and volatile

Keep stable identity but move all logical state into the world realization:
value, metadata, validator, and watch map. Validators can normally be shared
functions; their registration is world-local. Watch callbacks are effects, so
copying the watch map preserves Clojure behavior inside each branch but still
requires the callback's external capabilities to be honest.

### Delay

Represent a delay as a handle to a world-local state machine:

```text
pending(thunk) -> running(owner) -> success(value)
                              \-> failure(exception)
```

A quiescent world fork copies `pending`, `success`, or `failure`. `running`
should be impossible for an evaluation-owned delay because fork waits for the
active form; a delay running in an unmanaged host task is an external resource
and must block or reject the fork.

### Promise

`delivered(value)` can be copied. A pending promise also owns a waiter set,
which is control/resource state rather than a plain heap value. A world-only
fork should either create independent pending cells with no inherited waiters
or reject pending promises. An execution fork may duplicate suspended waiter
continuations only after they are represented inside the managed interpreter.

### Future and asynchronous computation

A running host Future cannot be copied honestly. The immediate correction is
to convey the selected SCI world and hold its evaluation permit whenever the
future body runs. Fork policy can then choose among explicit sharing, waiting
for completion and copying the result, restartable computation, cancellation,
or rejection. Full continuation forking requires managed CPS/fiber state.

### Multimethod

Keep the MultiFn identity stable and move method/preference tables to world
slots. Dispatch caches are derived state and can be copied or discarded on
fork. The hierarchy is already reached through a fork-local Var root. This is
a relatively contained next primitive and exercises multi-slot handles well.

### Memoization and lazy computation

SCI should provide a world-aware memoize implementation whose cache is a
tracked state handle. Lazy sequences are harder: their realization cache is a
small continuation, and arbitrary lazy bodies may perform effects. They belong
with the future execution/fiber model rather than a generic object copier.

### Transients and mutable aggregates

Transients are affine. Sharing them violates isolation, while copying them
silently violates owner and invalidation semantics. They should reject a fork
when reachable from a world cell unless an application wrapper defines a safe
policy. Arrays, mutable deftype fields, and application collections may be
duplicable, but need self-describing SCI handles or `Forkable` cooperation.

## Recommended implementation order

1. Preserve the corrected asynchronous world conveyance and extend it to any
   new managed callback boundary before expanding the primitive set.
2. Decide and specify world-versus-fiber dynamic-binding semantics, including
   nested cross-world evaluation and CLJS async-local behavior.
3. Address lineage-registry liveness so adding more tracked handles does not
   create an unbounded retention problem.
4. Finish split identity/state already present: atom/Var watches, atom
   metadata/validators, namespace/type metadata, and `*loaded-libs*`.
5. Make multimethod tables and memoization caches world-local.
6. Add managed Delay and choose a pending-Promise policy.
7. Reject or explicitly classify transients, running futures, streams, and
   other affine/external resources.
8. Introduce execution/fiber snapshots for lazy continuations and the future
   lambda-join/Simmis runtime.

Content addressing, if added later, should identify immutable analyzed code,
definition revisions, and causal namespace history. It does not replace any of
the mutable state-machine or resource decisions in this audit.
