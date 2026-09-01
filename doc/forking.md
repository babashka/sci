# Forkable SCI worlds

Full runtime forking is enabled with
`(sci/init {:runtime-mode :forkable})`. In the default `:standard` mode SCI
retains its direct execution paths and `sci/fork` preserves its historical
behavior of copying only the namespace environment.

In forkable mode, `sci/fork` creates a branch of an interpreter's runtime state. Namespace maps,
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
- An interpreted function returned by `eval-string*`, `eval-string+`, or
  `eval-form` is bound to the context that returned it when the host invokes it
  directly. When invoked from evaluation in a related descendant, it selects
  that active descendant instead. This context association is kept in a weak
  identity registry and does not change the function's metadata.
- Dynamic binding frames remain thread/evaluation scoped. A fork snapshots
  root bindings; it does not capture a running continuation or binding frame.
- Hosts that suspend interpreted code can explicitly capture its dynamic
  binding frame with `sci/capture-continuation-context`. After forking the SCI
  context, `sci/retarget-continuation-context` creates an independent binding
  frame for the child world, and `sci/continuation-context-fn` resumes a host
  continuation with that world and frame installed. Retargeting is restricted
  to the same SCI lineage. Ordinary `sci/fork` still does not implicitly copy
  running continuations.
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

An embedding that lets interpreted code construct a new, independent SCI
interpreter should perform that construction through
`sci/with-detached-context`. This temporarily leaves the caller's selected
world and dynamic binding frame, runs the supplied host thunk, and restores the
caller afterwards. Evaluating an existing SCI context recursively does not need
this boundary; it is specifically for creating a separate context lineage.

Stable SCI Vars used directly by host code select the context in which they
were created. Use `sci/call-with-context` to select a particular descendant
before dereferencing or mutating an inherited Var. A Var deliberately installed
in unrelated interpreter lineages has no unambiguous implicit home and likewise
requires this explicit boundary. `sci/alter-var-root`, `sci/alter-var-meta!`,
and `sci/reset-var-meta!` preserve this selection on all supported targets;
the latter two are the portable host APIs because native ClojureScript
metadata operations can bypass SCI's world slots.

Only values directly stored in world cells are inspected. A host container that
holds mutable children must implement the protocol and copy its own graph.
Unclassified values retain the compatibility behavior of being shared, except
for known affine or mutable host primitives that SCI can identify safely.
Direct host atoms, refs, agents, volatiles, multimethods, transient
collections, lazy sequences, pending tasks, Promises, and known consuming or
external resources are rejected unless an explicit policy handles them. A
completed JVM Future or realized host Delay may be shared because its lifecycle
is immutable. Mutable Java collections and common atomic/synchronization
holders are rejected as well; Clojure persistent collections are unaffected.

`sci/init` and the two-argument `sci/fork` also accept `:fork-fn`, a legacy
one-argument fallback applied to unclassified values stored in world cells. It
may copy application-owned state and should return immutable values unchanged.
The per-fork identity memo applies to protocol, callback, and built-in copying,
so identical direct cell values are transformed once and remain aliases in the
child.

SCI-owned stable handles remain world-selecting when captured by interpreted
closures, including mutable SCI deftype instances. Arbitrary mutable host
values hidden in a host closure or container are not reflectively discovered;
wrap them in a `Forkable` handle before capture. On CLJS, an instance visible
only through an opaque closure also retains its creation native-protocol
prototype, although its managed mutable fields remain fork-local.

## Current boundary

Var roots, metadata, and watch maps; namespace metadata and loaded-library
state; all logical SCI atom state; SCI-created volatile values and delays; SCI
type descriptor data; and JVM/CLJS SCI multimethod tables, preferences, and
dispatch caches are fork-local. ClojureDart's exact-dispatch SCI multimethod
tables are fork-local as well. SCI deftypes that declare mutable fields route
their persistent field map through a managed world slot, so state diverges
even when an instance is captured by a closure or nested in a persistent
container. Immutable-field types retain direct storage.
On the JVM, SCI-created promises are fork-local as well. SCI's `memoize` uses a
fork-local atom for its cache. Effects performed by any watch are ordinary user
capabilities and are not made reversible automatically. Managed lazy
continuations, nested mutable host objects, and effects outside SCI remain
future work unless the embedding application models or copies them.
Direct host lazy sequences are rejected rather than silently sharing their
realization cache. CLJS native-protocol
prototypes are cloned with Type data before child deftype/record values are
copied, so later child extensions do not change the parent's instances. A
transient collection directly stored in
world state is rejected as affine by default; an explicit `:fork-fn` can
instead impose an application-specific copy, share, or rejection policy.
Directly stored JVM arrays and CLJS JavaScript arrays are shallow-copied by
default with aliases preserved. Known unmanaged host reference primitives are
also rejected by default, requiring `Forkable` or `:fork-fn` cooperation.

On the JVM and CLJS an SCI-created atom is now a dedicated `SciAtom`, not the
host's concrete `clojure.lang.Atom`/`cljs.core.Atom` class. It implements the
normal dereference, atom, metadata, validator, and watch protocols, but code
that tests the host concrete class observes this intentional representation
difference. The split is necessary for one stable identity to select different
state in different worlds. Value commits and validator replacement are
serialized per stable atom, so a validator cannot be installed concurrently
with a value it rejects. Backing-array growth is coordinated with commits on
the JVM so a concurrent slot mutation cannot be lost during resizing.

SCI-created multimethods use the same representation tradeoff on JVM and CLJS:
the stable `SciMultiFn` handle selects a fork-local host dispatch engine. SCI's
`methods`, `get-method`, `prefers`, `prefer-method`, `remove-method`, and
`remove-all-methods` accept both SCI and native host multimethods, while host
code that tests specifically for the concrete host `MultiFn` class can observe
the difference.

ClojureDart has no host hierarchy/cache engine to clone. Its stable
`SciMultiFn` instead routes the persistent exact-dispatch method map through a
dense world slot; built-in runtime multimethods created outside an SCI world
retain their process-global tables.

SCI delays also have stable interpreter-owned identity. Forking a pending delay
creates an independent pending realization in the child; forking a completed
delay inherits its cached value or exception according to the host's delay
semantics. In particular, JVM Clojure caches a thrown exception while
ClojureScript leaves that delay pending and retryable. The JVM `promise`
follows the same world split: a delivered value is inherited, while a pending
child receives an independent delivery state and no source-world waiters. Its
shared wake-up monitor is only a scheduling signal: a waiter always rechecks
the state of its own world. Host code that tests for the concrete host delay
or promise class can observe these representation differences.

Forking is O(the lineage's allocated logical slots); unused backing-array
capacity is not copied. Slot reads and writes are constant time. This
intentionally favors the common
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
