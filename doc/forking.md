# Forkable SCI worlds

`sci/fork` creates a branch of an interpreter's runtime state. Namespace maps,
SCI Var roots and metadata, and values of SCI-created atoms and volatiles are
part of that state. Existing Vars, refs, aliases, and interpreted functions
keep their identity; their meaning is selected by the world in which code is
evaluated.

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

This first implementation makes Var roots/metadata and the values of
SCI-created atoms and volatiles fork-local. Atom validator and watch
registrations still live on the shared stable host handle. Promises, delays,
futures, transients, mutable host objects, mutable deftype fields, and effects
outside SCI remain shared unless the embedding application models or copies
them. A complete effect inventory and an explicit fork protocol are the next
step before describing an interpreter as fully forkable.

Forking is O(number of allocated slots), including spare array capacity, while
slot reads and writes are constant time. This intentionally favors the common
case where reads and mutations greatly outnumber forks. A page-level
copy-on-write representation remains a possible next step if workloads with
large worlds and frequent forks justify its extra read/write indirection.
