# 0007 - Instance and static member control

AI-generated design record. Read as AI prose.

## Context

SCI's `:classes` config gates which host classes interop can touch. It had two
levels: a class is allowed or not, and `:static-methods` could override a static
method. There was no way to override or deny an individual instance method, an
instance field, or a static field. Use cases: sandboxing untrusted code by
replacing a dangerous member with a safe one or denying it outright, and fixing
reflection-driven performance by supplying a direct fn.

This ADR adds four per-class member sections - `:instance-methods`,
`:static-methods`, `:instance-fields`, `:static-fields` - each mapping a member
name to a fn (override) or `true` (allow via reflection), plus `:closed` to turn
a class or a section into an allowlist that denies anything unlisted.

The hard constraint driving the design: **a program that does not use member
overrides must pay no per-call cost versus the pre-feature code.**

## The core problem

For static access (`Integer/parseInt`, `Integer/SIZE`) and qualified instance
access (`String/.length`, `Class/FIELD`), the class is part of the call form, so
config is resolved once at analysis. Zero runtime cost.

Plain instance access (`(.foo x)`, `(.-x y)`) is different: the analyzer knows
the member name but not `x`'s runtime class. The config decision depends on the
runtime class, so it cannot in general be made at analysis.

## Decisions

### 1. One node, config resolved on a cache miss

A single node, `eval-instance-method-invocation`, handles every plain instance
site. On a call it computes the runtime class and, on a cache miss, resolves the
allowlist and the member config (override / deny / `:closed`), then overrides,
throws, or reflects. The zero-per-call-cost constraint is met not by avoiding
this work but by the inline cache (decision 2): a hit skips all of it.

An earlier revision split this into a lean node and a feature node, gated at
analysis by whether any member config touched the site's method name
(`instance-member-names`, `instance-closed?`). Once the inline cache existed the
gate bought nothing: a hit skips config work regardless of node, and running the
config resolution on a no-config site is a proven no-op that reflects (no class
lists the name, nothing is closed, so the `cond` always reaches `:else`). The
split and its bookkeeping were removed. Collapsing to one node also fixed a
staleness bug: because config is now resolved on every miss rather than baked at
analysis, `merge-opts` tightening takes effect on already-analyzed code (the
cache invalidates, see decision 2).

### 2. Per-call-site inline cache

The node carries a per-site `volatile!` created at analysis, holding an
`object-array` of `[class->opts instance-class methods]`. On each call it
computes the runtime class and, if the cached `class->opts` and class both match
by identity, reflects directly on the cached method list. A hit skips the
allowlist lookup, `:public-class` resolution, the config `cond`, and `meth-cache`
(a `getName` plus env deref plus four-level nested map lookup). Almost all call
sites are monomorphic, so the common case is two identity checks and the invoke.

Caching the method list is the large win. Pre-feature interop called `meth-cache`
on every invocation to fetch the overload list. The line-55 note in
`interop.cljc` records that caching the single resolved method in the global map
was slower than re-dispatching, but that was a map lookup; a per-site array
`aget` is far cheaper, so the inline cache wins where the global attempt lost.
`invoke-matching-method` also shortcuts overload resolution when the list has one
element, so a monomorphic hit is close to a bare reflective `.invoke`. Measured,
plain `.length` drops from ~105ns (pre-feature) to ~45ns.

Keying on `class->opts` identity as well as class is what makes `merge-opts`
correct: it swaps in a fresh `class->opts` map, so every live site misses and
re-resolves under the new config for free. Override and deny outcomes are not
cached; they re-resolve each call, which is fine since they are not the hot
fallthrough.

The cache also stores the resolved target class, so a `:public-class` mapping is
cached: a monomorphic site skips the `:public-class` call itself on a hit,
serving the cached target directly. This holds only if `:public-class` is a pure
function of the instance's class. Custom types (`sci.impl.types.ICustomType`)
break that: a reified object carries its interfaces as per-instance data while
sharing one JVM class, so `:public-class` maps two same-class instances to
different targets. Caching a custom type would serve one instance's target for
another. So a `:public-class` result is cached only when the instance is not an
`ICustomType`; custom types re-resolve every call, as before. The contract for a
user `:public-class` fn is therefore: depend only on the instance's class, not
its value, for non-custom types.

The read is safe under concurrent execution of the node: `cached` is derefed
once into a local and all three fields are read from that one immutable snapshot,
and writes publish a fresh array through the volatile, never mutating in place.
So a reader never sees a torn `[class, methods]` pair. The cost is a per-site
volatile and, at a megamorphic site (one `.foo` seeing many receiver classes), a
fresh array allocation on each miss. Such sites are rare; the design accepts the
churn there in exchange for the monomorphic win everywhere else.

### 3. Static and qualified access resolved at analysis

For static access (`Integer/parseInt`, `Integer/SIZE`) and qualified instance
access (`String/.length`, `Class/FIELD`), the class is explicit in the form, so
config is resolved once at analysis: zero runtime cost, and enforced including
`:closed`. This closed a hole where `String/.length` previously bypassed
`:classes` control entirely. `:static-methods`/`:static-fields` therefore need no
runtime machinery, which matters for babashka, which already ships
`:static-methods`.

### 4. Precedence and trust

`:closed` and overrides beat `:allow :all`: the node resolves the member config
independently of the allow decision, so a closed class denies even under
`:allow :all`.

Type hints on receivers are not trusted to skip config. SCI is lenient with hints
(a wrong `^String` hint falls back to the real class at runtime), so trusting a
hint at analysis would let sandboxed code dodge a control by lying about a
receiver's type. Only `Class/.method`, where the class is explicit and is the
dispatch class, resolves config from a name at analysis.

## Consequences

- No override, no closed: a monomorphic site is two identity checks plus the invoke, faster than the pre-feature code thanks to the inline cache.
- Overrides and `:closed`: resolved on a cache miss, at the same monomorphic hot-path cost as unconfigured interop.
- Static and qualified `Class/.method` access: resolved at analysis. Zero runtime cost, and enforced including `:closed`.
- `merge-opts` tightening takes effect on already-analyzed code: the fresh `class->opts` map invalidates the cache, so the next call re-resolves under the new config and denies. No analysis-time staleness.

## Numbers

Min-of-15, 1M-call loops, interleaved with master:

| path | pre-feature | this branch |
|---|---|---|
| plain `.length`, no config | ~105 | ~45 (inline cache) |
| static `Integer/parseInt` | ~30 | ~30 |
| qualified `String/.length` | ~46 | ~46 |
| `.length` on X, unrelated class closed | ~101 | ~45 (inline cache) |

Plain interop lands at ~45ns whether or not any class is configured: a
monomorphic hit is two identity checks plus the reflective invoke, skipping the
allowlist lookup and `meth-cache` that the pre-feature code ran every call.
Confirmed in a native babashka binary: direct interop ~1.6x faster than master.
`:public-class` interop, which babashka uses heavily (Process, Path, streams),
also caches now: a JVM microbench of a `:public-class`-mapped call dropped ~3x
once the target class is cached and the `:public-class` fn is skipped on hits.
