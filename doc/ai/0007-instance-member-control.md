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

### 1. Two nodes, chosen at analysis

`eval-instance-method-invocation` is the lean path. `eval-instance-method-invocation+configs`
is the feature path. The analyzer picks one node per call site, once. The two
share the same fast path (see decision 5) and differ only on a cache miss: the
feature node runs the config `cond` (override/deny/`:closed`), the lean node
runs only the allowlist check. A program that uses no member config hits the
lean node, which is now faster than the pre-feature code, not byte-identical to
it: it carries the per-site cache.

### 2. Name-based gate for overrides

`normalize-classes` collects `instance-member-names`, the set of member names
any class overrides or allowlists. A `(.foo x)` site takes the feature node only
when `foo` is in that set. Overriding `.append` on one class leaves every
`.length`/`.charAt` site lean. Overrides are name-granular and cost nothing on
unrelated interop.

### 3. `:static-methods`/`:static-fields` never widen the gate

Static members are resolved at analysis (class known), so they do not contribute
to `instance-member-names` or the closed flag. This matters for babashka, which
already ships `:static-methods`: adopting this branch adds no instance-interop
cost.

### 4. `:closed` and the global flag

`:closed` is fundamentally different from an override. An override changes some
names and leaves a "reflect regardless of class" bucket for the rest - the lean
node encodes exactly that bucket. `:closed` empties the bucket: every name is
either allowlisted (allow/override) or denied. There is no "reflect regardless"
behavior left, so no name can use the lean node.

Concretely, if any instance section is closed anywhere, an arbitrary `(.foo x)`
might hit that closed class at runtime with `foo` unlisted, which must throw.
The name is unknown-to-config and the runtime class is unknown at analysis, so
every plain instance site must route to the feature node. `instance-closed?` is
therefore a global flag. It is a correctness necessity for plain interop, not a
shortcut. It is set only by `:closed` on an instance section (class-level, or
`:instance-methods`/`:instance-fields` section-level); closing only static
sections leaves plain interop lean.

### 5. Per-call-site inline cache

Both nodes carry a per-site `volatile!` created at analysis, holding an
`object-array` of `[class->opts instance-class methods]`. On each call the node
computes the runtime class and, if the cached `class->opts` and class both match
by identity, reflects directly on the cached method list. A hit skips the
allowlist lookup, `:public-class` resolution, the config `cond`, and `meth-cache`
(a `getName` plus env deref plus four-level nested map lookup). Almost all call
sites are monomorphic, so the common case is two identity checks and the invoke.

Caching the method list is the large win. Pre-feature interop, and this branch
before the inline cache, called `meth-cache` on every invocation to fetch the
overload list. The line-55 note in `interop.cljc` records that caching the
single resolved method in the global map was slower than re-dispatching, but that
was a map lookup; a per-site array `aget` is far cheaper, so the inline cache
wins where the global attempt lost. `invoke-matching-method` also shortcuts
overload resolution when the list has one element, so a monomorphic hit is close
to a bare reflective `.invoke`. Measured, plain `.length` drops from ~105ns
(pre-feature) to ~45ns, and the same for the closed-class-elsewhere path.

The cache is keyed on `class->opts` identity as well as class, so a `merge-opts`
config change (which swaps in a fresh `class->opts` map) invalidates every live
site for free. It is populated only when the resolved target is the instance's
own class (`identical? target-class instance-class`), i.e. the direct-reflect
path. The `:public-class` mapping is left uncached, since a `:public-class` fn
receives the instance and could map differently per instance. Override and deny
outcomes are not cached; they re-resolve each call, which is fine since they are
not the hot fallthrough.

The cost is a per-site volatile and, at a megamorphic site (one `.foo` seeing
many receiver classes), a fresh array allocation on each miss. Such sites are
rare; the design accepts the churn there in exchange for the monomorphic win
everywhere else.

## Consequences

- No override, no closed: interop takes the lean node, now faster than the pre-feature code thanks to the inline cache.
- Overrides: only the overridden member names take the feature node. Name-granular.
- Static and qualified `Class/.method` access: resolved at analysis. Zero runtime cost, and enforced (including `:closed`), closing a sandbox hole where `String/.length` previously bypassed `:classes` control entirely.
- Any instance `:closed`: all plain instance interop takes the feature node, at the same monomorphic cost as the lean node.
- Type hints on receivers are not trusted to skip config. SCI is lenient with hints (a wrong `^String` hint falls back to the real class at runtime), so trusting a hint at analysis would let sandboxed code dodge a control by lying about a receiver's type. Only `Class/.method`, where the class is explicit and is the dispatch class, resolves config from a name at analysis.
- Member gating is resolved at analysis, so tightening config on a live context via `merge-opts` does not govern code already analyzed. A `(.foo x)` compiled while no member config applied is a lean node that stays lean, even if a later `merge-opts` closes `x`'s class. This is the same analysis-time behavior master already has for `:static-methods`. The normal sandbox flow is immune: set `:closed`/overrides at `init`, then eval untrusted code, so that code is analyzed with the config present and takes the feature node. The staleness needs the backwards order - eval, then tighten. Making post-analysis tightening take effect would require a runtime gate on every instance interop call, which breaks the zero-cost-when-unused constraint, so it is documented rather than paid for. The per-site cache is separately kept correct across `merge-opts` by keying on `class->opts` identity: `merge-opts` swaps in a fresh map, so a live `+configs` node misses its cache and re-resolves.

## Numbers

Min-of-15, 1M-call loops, interleaved with master:

| path | pre-feature | this branch |
|---|---|---|
| plain `.length`, no config | ~105 | ~45 (lean, inline cache) |
| static `Integer/parseInt` | ~30 | ~30 |
| qualified `String/.length` | ~46 | ~46 |
| `.length` on X, unrelated class closed | ~101 | ~45 (inline cache) |

The lean and closed-elsewhere paths both land at ~45ns: a monomorphic hit is two
identity checks plus the reflective invoke, skipping the allowlist lookup and
`meth-cache` that the pre-feature code ran every call.
