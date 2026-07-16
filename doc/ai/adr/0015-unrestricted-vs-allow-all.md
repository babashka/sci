# ADR 0015: `:unrestricted` and `:classes {:allow :all}` are orthogonal

Status: accepted (2026-07-15).

## Context

`sci.core/init` takes two options that both loosen sandboxing, and it is
tempting to think one implies the other:

- `:unrestricted true` (ADR: the ctx-scoped flag that replaced the global
  `*unrestricted*` var, #1065). It permits mutation of built-in vars on all
  platforms, and on CLJS it also routes instance interop through the
  unchecked node, skipping the `:classes` permission check.
- `:classes {:allow :all}`. It marks every reachable class as allowed for
  interop.

They are NOT the same, and `:unrestricted` must not imply `:allow :all`.

## What each one actually gates

`:allow :all` is read on BOTH platforms, at
`sci.impl.evaluator/eval-instance-method-invocation`:

```
allowed?     (or ... (get class->opts :allow))
target-class (if allowed? instance-class
                 (when-let [f (:public-class env)] (f instance-expr*)))
```

On the JVM, `:unrestricted` has no effect on this path (the JVM analyzer arm
never consults it); interop permission is governed entirely by `:allow` and
per-class registration. Verified: `{:unrestricted true}` alone still throws
`Method getName on class java.lang.Class not allowed!` for an unregistered
class, while `{:classes {:allow :all}}` permits it.

On CLJS the two overlap only because `:unrestricted` bypasses the interop
check upstream (the unchecked node), which is what makes `:allow :all` look
redundant there. That redundancy is CLJS-specific and does not generalize.

## Why implying `:allow :all` would break babashka

babashka does not set `:allow :all`. It curates interop with `:public-class`
(`babashka.impl.classes`): a fn that maps a concrete instance to a public
superclass or interface (`java.lang.Process`, `java.util.Map`, ...) and
reflects THROUGH that type.

In the evaluator snippet above, with `:allow` not `:all`, `allowed?` is false
for an unregistered concrete class, so `target-class` falls through to
`(:public-class env)` and reflection uses the curated interface. If
`:unrestricted` implied `:allow :all`, `allowed?` would be true for every
class, `target-class` would become the concrete `instance-class`, and the
`:public-class` routing would be bypassed.

That matters because babashka's curated interfaces are the types compiled
into the native image; the concrete classes' methods often are not. Bypassing
the routing would turn working interop into missing-method reflection failures
in the native binary. So babashka sets `:unrestricted` (for built-in var
mutation) and deliberately leaves `:allow :all` off.

## Decision

Keep the two options independent:

- `:unrestricted` -> built-in var mutation (all platforms) plus the CLJS
  interop-check bypass.
- `:allow :all` -> "any reachable class is allowed," which flips
  `target-class` to the concrete class and disables `:public-class` routing.

Do not fold one into the other. A CLJS host with no `:public-class` and no
`:closed` carve-outs may drop `:allow :all` when it sets `:unrestricted`, but
that is a host-specific simplification, not a general equivalence.

## Addendum (2026-07-15): member overrides under `:unrestricted` on CLJS

`:classes` is a permission gate and also behavior customization via
`:instance-methods`, `:instance-fields` and `:static-methods` override fns.
CLJS `:unrestricted` silently dropped the override part. The unchecked
instance node consulted no config at all. The CLJS static path bound
`(unchecked-get class name)` without consulting `:static-methods`. Config
was accepted and ignored. The JVM honored overrides regardless of
`:unrestricted`. This predates the jit tier.

Fixed on branch `cljs-interop-overrides`:

- Static method overrides bind at analysis time on CLJS, in all ctxs, for
  members directly on a registered class:
  `{'process {:class js/process :static-methods {'exit f}}}` with
  `(process/exit 0)`. Dotted access under a root class, like
  `js/process.exit`, does not consult overrides. Register the direct
  parent instead. Zero runtime cost. The jit `:jsstatic` arm binds the
  override like any resolved method.
- Instance interop is gated per member name. A callsite routes through the
  config-aware node only when its method or field name has an override in
  some `:classes` entry. See `sci.impl.interop/instance-override-names`, a
  name set cached on class->opts identity. The runtime class then decides
  whether the override applies. The `allowed?` flag still skips the
  permission check there, so `:unrestricted` permission semantics are
  unchanged. All other names keep the unchecked fast path and full jit
  interop emission. Overriding `exit` costs nothing anywhere else.
  Overridden-name callsites run interpreted. The jit never compiles
  config-aware interop, so overrides cannot be bypassed.

Known limitations:

- Static field value reads, like `js/process.pid` in value position,
  resolve at analysis and are not interceptable.
- `sci/add-class!` after a callsite was analyzed does not retrofit that
  callsite. Same staleness family as ADR 0014 backlog 8d.
- Static override keys use munged member names.
