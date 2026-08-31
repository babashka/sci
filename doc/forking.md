# Forkable SCI worlds

`sci/fork` creates a branch of an interpreter's runtime state. Namespace maps,
SCI Var roots and metadata, SCI-created atom state, and values of SCI-created
volatiles are part of that state. Existing Vars, refs, aliases, and interpreted
functions keep their identity; their meaning is selected by the world in which
code is evaluated.

This is deliberately a value/realization split. Stable handles receive dense,
lineage-local integer slots. Each world realizes those slots in an array; a
fork briefly quiesces the source world and copies the array. Var reads in the
ordinary primary context keep their direct SCI realization, while descendants
select their fork-local slot. Stateful primitives update one slot at a time,
so unrelated atoms do not contend on a shared world root.

This shape combines four useful precedents:

- Clojure Vars retain a direct mutable root and thread-local binding frames.
- Julia keeps a fast latest-world realization separate from historical world
  lookup.
- Yggdrasil and Spindel make branching state and externally owned resources
  explicit.
- lambda-join treats the world itself as a persistent value while preserving
  stable per-Var cells as a realization detail.

## Semantics

- A function defined before a fork resolves Vars and SCI refs in the world
  where it is invoked, provided that world belongs to the same context
  lineage.
- Dynamic binding frames remain thread/evaluation scoped. A fork snapshots
  root bindings; it does not capture a running continuation or binding frame.
- Interpreter bindings are scoped to their world. Managed asynchronous
  callbacks convey and restore both the selected world and binding frame.
- Writes before the first fork have ordinary SCI/Clojure mutable behavior.
  The first fork materializes the primary values into dense slots; later
  writes update the selected world's slots.
- On the JVM, evaluation of a form holds a shared permit for its world and a
  fork takes the exclusive permit. A concurrent fork therefore waits for
  active forms in that source world and gets a quiescent snapshot. Different
  worlds have independent gates. Calling `fork` recursively from inside an
  evaluation of that same world is rejected instead of deadlocking.

## Host cooperation

SCI cannot infer how arbitrary host objects should branch. Host types can
implement `sci.fork/Forkable`; its `fork-value` method determines the value
stored in the child world. SCI calls the method once per identical value during
one fork and preserves aliases to its result.

| Resource class | Fork behavior |
|---|---|
| Immutable or persistent value | Share it by returning `this`, or leave it unclassified |
| Duplicable application state | Return an independent copy from `fork-value` |
| Deliberately shared external resource | Return `this` |
| Affine or movable authority | Reject the non-destructive fork, or keep authority outside the SCI value world |
| Prohibited resource | Throw an explanatory exception from `fork-value` |

Only values directly stored in world cells are inspected. A host container that
holds mutable children must implement the protocol and copy its own graph.
Unclassified values retain the compatibility behavior of being shared.

`sci/init` and the two-argument `sci/fork` also accept `:fork-fn`, a legacy
one-argument fallback applied to unclassified values stored in world cells. It
may copy application-owned state and should return immutable values unchanged.
Unlike the protocol path, callback alias preservation remains the
application's responsibility, for example with an identity-keyed memo table.

## Current boundary

Var roots/metadata, all logical SCI atom state, SCI-created volatile values,
and JVM/CLJS SCI multimethod tables, preferences, and dispatch caches are
fork-local. SCI's `memoize` uses a fork-local atom for its cache. Atom metadata,
validator, and watch maps are inherited by a fork and can subsequently diverge;
effects performed by a watch are ordinary user capabilities and are not made
reversible automatically. Promises, delays, futures, transients, mutable host
objects, mutable deftype fields, and effects outside SCI remain shared unless
the embedding application models or copies them.

On the JVM and CLJS an SCI-created atom is now a dedicated `SciAtom`, not the
host's concrete `clojure.lang.Atom`/`cljs.core.Atom` class. It implements the
normal dereference, atom, metadata, validator, and watch protocols, but code
that tests the host concrete class observes this intentional representation
difference. The split is necessary for one stable identity to select different
state in different worlds.

SCI-created multimethods use the same representation tradeoff on JVM and CLJS:
the stable `SciMultiFn` handle selects a fork-local host dispatch engine. SCI's
`methods`, `get-method`, `prefers`, `prefer-method`, `remove-method`, and
`remove-all-methods` accept both SCI and native host multimethods, while host
code that tests specifically for the concrete host `MultiFn` class can observe
the difference.

Forking is O(number of allocated slots), including spare array capacity, while
slot reads and writes are constant time. This intentionally favors the common
case where reads and mutations greatly outnumber forks. A page-level
copy-on-write representation remains a possible next step if workloads with
large worlds and frequent forks justify its extra read/write indirection.

Self-describing SCI state handles are held weakly by the lineage registry. A
fork sweeps the source world's slots for handles that have become unreachable,
so an abandoned local atom does not retain its last value, metadata, validator,
or watches indefinitely. Slot numbers and array capacity are not currently
reclaimed; sweep bounds retained payloads but does not yet reduce the cost of
forking a lineage that allocated many short-lived handles. A value or watch
that refers back to its own atom also forms a world-to-value ownership cycle
and cannot be discovered by the weak-handle sweep alone.

See [Forkable runtime state audit](fork-state-audit.md) for the dynamic-binding
model, stateful primitive inventory, observed isolation gaps, and proposed
implementation order.
