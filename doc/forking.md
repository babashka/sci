# Forkable SCI worlds

`sci/fork` creates a branch of an interpreter's runtime state. Namespace maps,
SCI Var roots and metadata, and values of SCI-created atoms and volatiles are
part of that state. Existing Vars, refs, aliases, and interpreted functions
keep their identity; their meaning is selected by the world in which code is
evaluated.

This is deliberately a value/realization split. The forkable value is a pair
of persistent maps keyed by stable handles. The ordinary SCI context is the
fast mutable realization until its first fork. That fork materializes current
Var and ref values once and changes the original context to persistent
copy-on-write operation. Later forks are O(1) unless host copying is requested.

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
  The first fork snapshots them. Writes after that point path-copy the selected
  world's maps.
- Forking is intended to happen between evaluations. Concurrently mutating and
  forking the same primary context does not currently promise a linearizable
  snapshot.

## Host cooperation

SCI cannot infer how arbitrary host objects should branch. `sci/init` and the
two-argument `sci/fork` accept `:fork-fn`, a one-argument function applied to
values stored in world cells. It may copy application-owned state and should
return immutable values unchanged. If multiple values alias the same host
object, the function must preserve that relationship when it matters, for
example with an identity-keyed memo table.

## Current boundary

This first implementation makes Var roots/metadata and the values of
SCI-created atoms and volatiles fork-local. Atom validator and watch
registrations still live on the shared stable host handle. Promises, delays,
futures, transients, mutable host objects, mutable deftype fields, and effects
outside SCI remain shared unless the embedding application models or copies
them. A complete effect inventory and an explicit fork protocol are the next
step before describing an interpreter as fully forkable.

The primary performance cost that remains is selecting the active world on
each world-sensitive operation. A custom persistent collection could improve
branched writes, but it would not remove that dispatch cost; optimizing the
world-selection/call-site path should come first.
