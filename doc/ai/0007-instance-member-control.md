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

`eval-instance-method-invocation` is the lean path, byte-identical to the
pre-feature code. `eval-instance-method-invocation+configs` is the feature path.
The analyzer picks one node per call site, once. A program hitting the lean node
runs exactly the old code.

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

### 5. Per-call-site class cache

The feature node carries a per-site volatile. On each call it computes the
runtime class (which the lean node computes too), and if it matches the last
class that resolved to plain reflection, it reflects directly, skipping the
allowlist lookup, `:public-class` resolution, and the config `cond`. Almost all
call sites are monomorphic, so the common case is a class-identity check plus the
same reflective invoke.

This does more than recover the feature overhead: it beats the pre-feature code,
because the lean path (like the old code) re-does the class allowlist lookup on
every call - a `getName` plus symbol intern plus map lookup - while a cache hit
skips it. Measured, plain interop on an unrelated class under a closed-class
config drops from ~101ns (pre-feature) to ~60ns.

The cache is populated only when the resolved target is the instance's own class
(`identical? target-class instance-class`), i.e. the direct-config reflect path.
The `:public-class` mapping is left uncached, since a `:public-class` fn receives
the instance and could in principle map differently per instance. Override and
deny outcomes are not cached; they re-resolve each call, which is fine since they
are not the hot fallthrough.

## Consequences

- No override, no closed: interop runs the old code. Zero cost.
- Overrides: only the overridden member names take the feature node. Name-granular.
- Static and qualified `Class/.method` access: resolved at analysis. Zero runtime cost, and enforced (including `:closed`), closing a sandbox hole where `String/.length` previously bypassed `:classes` control entirely.
- Any instance `:closed`: all plain instance interop takes the feature node, but the per-site cache makes the monomorphic case faster than the pre-feature code.
- Type hints on receivers are not trusted to skip config. SCI is lenient with hints (a wrong `^String` hint falls back to the real class at runtime), so trusting a hint at analysis would let sandboxed code dodge a control by lying about a receiver's type. Only `Class/.method`, where the class is explicit and is the dispatch class, resolves config from a name at analysis.

## Numbers

Min-of-15, 1M-call loops, interleaved with master:

| path | pre-feature | this branch |
|---|---|---|
| plain `.length`, no config | ~104 | ~104 (lean, byte-identical) |
| static `Integer/parseInt` | ~30 | ~30 |
| qualified `String/.length` | ~46 | ~46 |
| `.length` on X, unrelated class closed | ~101 | ~60 (cache) |

Residual sub-noise differences on the lean path come from the JIT compiling the
byte-identical fn differently with the feature fn present in the same namespace,
not from added work.
