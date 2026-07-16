# ADR 0016: Runtime JVM class emission via Crema (deftype interfaces, bytecode JIT)

Status: experiment validated end to end, parked on branches pending a
product decision on image size. Written by Claude in pair-programming
sessions with Michiel; kept on master so the findings survive even if the
branches rot.

## Branches

- sci: `jvm-jit` (integration `5acc2d6`, spike code preserved in `110cd41`) — the emitter (`sci.impl.jvm-type-emit`)
  and the deftype integration.
- babashka: `crema-deftype` (tip `2387a525`) — build flags, interop
  allowlist hook, sci submodule pointed at the sci branch. Spike code for
  the fn-body bytecode JIT and the wrapper-class prototype is on the sci
  branch under `scratch/src/sci_jvm_jit/` (force-added past gitignore in
  `110cd41`).

## Context

GraalVM native image historically forbids runtime class definition, which
is why bb interprets and why sci deftype rejects real JVM interfaces
("defrecord/deftype currently only support protocol implementations").
GraalVM 25.1 ships Crema (`-H:+RuntimeClassLoading`, experimental): a
bytecode interpreter for runtime-loaded classes inside the native binary,
plus Ristretto (`-H:+GraalJITCompileAtRuntime`): a JIT that compiles those
classes after an invocation threshold. `defineClass` becomes the JVM
analog of the CLJS jit tier's `js/Function` (ADR 0014).

## What was validated (2026-07-14, GraalVM 25.1.3, macOS aarch64)

### deftype with JVM interfaces, in native bb

    (deftype T [x] Runnable (run [_] ...))           ; real Thread runs it
    (deftype Cmp [] java.util.Comparator ...)        ; sort calls it
    (deftype M [^:volatile-mutable c] P ... Runnable ...)

All green in the native binary: Java-side callbacks (Thread, sort),
sci-side direct interop `(.run m)`, mixed protocol + interface, mutable
fields via `set!`, printing and identity equality identical to plain sci
deftype. Startup unchanged (10ms). Full sci JVM suite green.

### fn-body bytecode JIT (spike only, not integrated)

Emitting sci-shaped fn bodies (boxed `clojure.lang.Numbers` calls) as
bytecode and running them under Ristretto, warm, ms:

| workload          | bb interp | Crema interp | emitted + Ristretto | Clojure compiler + Ristretto | JVM C2 |
|-------------------|-----------|--------------|---------------------|------------------------------|--------|
| 10M loop          | 190       | 1880         | 116                 | 25                           | ~3     |
| fib 27            | 27        | 257          | 16.2                | 16.5                         | ~2     |

- fib: boxed emission already matches the real Clojure compiler under
  Ristretto and beats bb 1.7x.
- loop: boxing costs 4.6x under Ristretto (no escape analysis; 10M Long
  allocations). Primitive-long inference for loop registers closes 116 ->
  ~25 = 7.5x over bb. That is the top emitter optimization.
- Ristretto compiles per fn INVOCATION COUNT
  (`-XX:JITCompilerInvocationThreshold=N`), no OSR: a hot loop inside one
  call never tiers up. The CLJS jit's lazy per-arity stub design maps onto
  this exactly.

### Cost (the reason this is parked)

Same GraalVM, same config, only flags differ:

| build                                        | size     |
|----------------------------------------------|----------|
| bb baseline                                  | 71.2 MB  |
| + RuntimeClassLoading + 2 sci Preserves      | 186.5 MB |
| + Preserve=clojure.lang (NOT needed)         | 200.5 MB |

+115MB is Crema's interpreter and open-world metadata, not reducible on
our side. Ristretto adds roughly another 50MB (measured in cream). Crema
also makes its own interpreter ~50% slower with the JIT flag on, but that
tax only applies to runtime-loaded bytecode; AOT'd sci keeps full native
speed, so for bb the tier is pure upside perf-wise. Options: a separate bb
flavor, or wait for Crema to leave EA and slim down.

## Design decisions

1. **Wrapper, not subclass.** `SciType` is final (Clojure deftype always
   emits final classes). The emitted class holds a `sci_delegate` field
   (a real SciType), implements SciType's six machinery interfaces
   (ICustomType, IBox, SciTypeInstance, IFn, SciPrintMethod, IType) by
   pure delegation, and the user's JVM interfaces via stubs that call
   sci-interpreted fns from a `public static IFn[] sci_impls` table.
   Method bodies keep working unchanged because they access fields
   through `-inner-impl`/ext-map, which delegation preserves.
2. **Marker interface** `IJvmSciType` (definterface, one method
   `sci_delegate`): `equals` unwraps the other operand when it is also a
   wrapper; `print-method` gets one defmethod on the interface;
   babashka's `:public-class` fn maps wrapper instances to their own
   class so the interop allowlist admits them (their methods are the
   user's own sci fns, sandbox-neutral).
3. **Capability probe** (`class-defining-available?`, a delay): emit and
   define a trivial wrapper once, catch Throwable. JVM: always true.
   Native without Crema: false, and everything behaves exactly as today.
   Mirrors the CLJS `js-eval-available` probe.
4. **`:deftype-fn` `:error` wins over auto-implementation.** An embedder
   returning `{:error ...}` deliberately steers users (bb points ILookup
   users at IPersistentMap); the partition into JVM-interface vs protocol
   impls is gated on `(nil? error-hint)`. Embedders relax their
   deftype-fn when they gain Crema.
5. **Emission via `clojure.asm`** (Clojure's bundled shaded ASM, zero new
   deps). Gotcha: the shaded `Opcodes` has no `V17` field; use `V1_8`.

## Crema-specific findings (hard-won, save yourself the rebuilds)

- Methods called FROM runtime-loaded bytecode need
  `-H:Preserve=package=...` even when AOT-reachable: AOT call sites may
  be inlined, leaving no dispatchable entry ("Unable to call AOT
  method"). bb needs exactly `sci.impl.deftype` and `sci.impl.types`.
  `clojure.lang` is NOT needed (IFn dispatch already works) and costs
  14MB.
- In cream, `clojure.asm.ClassWriter` needed
  `-H:Preserve=package=clojure.asm` only because the emitter there was
  runtime-loaded; in bb the emitter is AOT'd, so the analysis keeps ASM
  naturally.
- Ristretto flag is build-time `-H:+GraalJITCompileAtRuntime`; without it
  Crema interprets only. Runtime threshold:
  `-XX:JITCompilerInvocationThreshold=N`.

## Reconstruction directions (if the branches are lost)

1. `sci.impl.jvm-type-emit` (clj-only, ~250 lines): definterface
   IJvmSciType; box/unbox helpers over `clojure.lang.RT` casts; a
   ClassWriter (`COMPUTE_FRAMES`, `V1_8`) that emits: ctor with SciType's
   4-arg shape `(rec-name type type-meta ext-map)` constructing the
   delegate, the accessor, toString/hashCode delegation, unwrapping
   equals, delegation stubs for `(.getInterfaces SciType)` methods
   (signature-preserving, user impls win on collision), user stubs
   `sci_impls[i].invoke(this, boxedArgs...)` with return adaptation
   (void -> POP/RETURN, prims via RT casts, else CHECKCAST). Registry
   atom rec-type -> {:class :ctor}; `define-type-class!` reflects the
   user interfaces for methods named in the impl map (dedup by
   name+descriptor); `construct` mirrors `->type-impl` args. Fresh
   DynamicClassLoader per definition so re-evaluation works.
2. `deftype.cljc` `standard-scitype-path`: resolve each protocol name
   once; partition JVM interfaces (`class?`, `.isInterface`, not IFn)
   behind the probe and `(nil? error-hint)`; analyze interface method
   bodies with the same field-binding transformation as protocol impls
   (extract it; bodies arities of one name merge into one `(fn ...)`),
   into a `{name-str analyzed-fn}` map; prepend a
   `(sci.impl.deftype/-define-jvm-type! 'rec-type [ifaces] impls)` node
   to the protocol-impl nodes; factory calls `-construct-jvm-type`
   instead of `->type-impl`. Register both helpers in the
   `sci.impl.deftype` namespace map in namespaces.cljc.
3. babashka: `:public-class` cond gains
   `(instance? IJvmSciType v) (class v)` as the FIRST clause;
   classes.clj requires the emit ns; script/compile gains the three
   `-H:` flags above.
4. The fn-body JIT spike (not on any branch): emit `AFunction` subclasses
   whose invoke mirrors sci semantics with boxed Numbers static calls;
   see ADR 0014 for the CLJS architecture to port (lazy per-arity stubs,
   wrap-guarantee stacks, differential fuzzing). The sci analyzer's ast
   attachment is CLJS-only today; a JVM tier needs a side table or a node
   field on `:clj`.

## Traps hit during the work

- `git submodule update --init` without a path resets the sci submodule
  to the pinned sha, silently discarding the branch checkout.
- The global `:babashka/dev` alias pins `babashka/babashka` to the main
  checkout; testing a bb worktree requires a `-Sdeps` local/root
  override or you silently test master.
- Differential test note: deftype equality is identity-based and printing
  is `#object[...]`; both LOOK broken next to defrecord but are exact
  parity with plain sci deftype.
