# ClojureDart (cljd) port - status

Branch: `cljd`. Goal: compile SCI to Dart via ClojureDart so Clojure can be
evaluated on the Dart CLI and eventually a Flutter mobile app. SCI is the eval
engine; cljd just compiles SCI's .cljc source to Dart. cljd's lack of runtime
`eval` is NOT a blocker - SCI provides its own eval.

## Status: first namespace (sci.impl.types) compiles and tests pass on cljd.

Depends on edamame 1.6.40+ (has cljd support).

## Bootstrap / how to build

Test runner: `script/test/cljd` (or `bb test:cljd`). Uses `clojure -M:cljd
test <namespaces>`. The namespace list in the script is the selector: extend
`default_namespaces` as more of SCI is ported. Args override the list:
`script/test/cljd some.ns other.ns`.

`:cljd` alias in deps.edn. `:cljd-local` composes a local ClojureDart
checkout: `clojure -M:cljd:cljd-local test ...`.

CI: `test-cljd` job in .github/workflows/ci.yml.

Warning: `clojure -M:cljd init` (run by the script when pubspec.yaml is
missing) overwrites README.md, CHANGELOG.md and .gitignore with Dart
templates. Restore them with git after first init.

## Done

- Routed all 5 clojure.tools.reader.reader-types requires to
  edamame.impl.reader-types on cljd (core.cljc, impl/parser, impl/read,
  impl/interpreter, impl/load) + the IndexingReader instance? check in load.
- edamame 1.6.40 (cljd support).
- sci.impl.types ported, compiles and passes tests on cljd
  (test/sci/cljd_smoke_test.cljc).
- cljd test runner + selector + CI job.
- Parser chain ported: sci.impl.parser, interop (class resolution only,
  invocation fns stubbed with ex-info throws), utils, vars, sci.lang.
  parse-string works on cljd (test/sci/parse_test.cljc, sci.core tests
  gated with `#?(:cljd nil :default ...)`).
- Reader routing renamed to edamame.impl.cljd-reader-types (name in released
  edamame 1.6.40).
- Analyzer shallow deps ported: callstack, faster, fns, destructure.
  sci.cljd-smoke-test requires them as a compilation canary.
- TEMPORARY test scaffolding, remove once eval-string tests run on cljd:
  test/sci/impl/destructure_test.cljc (destructure is otherwise covered
  indirectly via core_test/error_test), test/sci/cljd_smoke_test.cljc
  (canary requires + smoke tests).
- .cljd test shadow trick (for big test files like core_test): same ns in a
  .cljd file shadows the .cljc on cljd only. JVM/CLJS never read .cljd. For
  small files prefer gating in the .cljc directly. Gotcha either way: the
  reader resolves ::alias/kw at read time even in skipped branches and in #_
  forms, so use fully qualified keywords when the alias is cljd-gated.

## Remaining (the bulk - revisit here)

1. sci.lang - Var / Namespace / Type deftypes (heavy platform code). START
   HERE. Earlier attempt crashed the cljd compiler on sci.impl.callstack
   ("Cannot invoke IFn.invoke because this.source is null"), traced to the
   `^sci.lang.Namespace` type hint / sci.lang deftype interop layer.
2. sci.impl.vars - dynamic binding uses Java ThreadLocal; Dart is single
   isolate -> replace with a global frame (no Thread/currentThread checks).
3. sci.impl.namespaces (~2245 lines) - map clojure.core host fns to Dart.
4. analyzer / evaluator / reify / deftype.
5. io (StringWriter -> StringBuffer, read-line), exceptions, locking (no-op).
6. Then: CLI eval milestone, then SCI test suite under cljd, then mobile app.

## cljd gotchas (learned during edamame port)

- cljd reads .cljc with `#?(:cljd ...)`. Two passes: target features #{:cljd};
  ns/macro pass also has :clj (so #?(:clj [lib]) REQUIRES get pulled - gate
  with `#?@(:cljd [] :clj [[lib]])`, :cljd first). clj/cljs BODIES vanish on
  target but a clj-only deftest still gets discovered - gate with
  `#?(:cljd nil :clj (deftest ...))`.
- (Object.) and 0-field deftypes are const-folded into one shared instance
  (all identical?). Use `^:unique (Object.)` for unique sentinels.
- keywords not reference-equal -> use = not identical?.
- cljd ExceptionInfo is NOT a Dart Exception subtype: `thrown? Exception`
  won't catch ex-info; use cljd.core/ExceptionInfo.
- no array-map (use hash-map). StringBuffer.write returns void (use
  `(doto sb (.write ch))` where the builder value is needed). type hints to
  java/clojure.lang types break - drop or :cljd-branch them.

## cljd gotchas (learned during sci.impl.types port)

- Put the :cljd clause FIRST in EVERY reader conditional, expression-level
  included. The host pass reads with both :cljd and :clj active and the first
  matching clause wins, so `#?(:clj A :cljd B)` host-compiles A. A :clj-first
  clause can even leak into Dart emission: `#?(:clj (defprotocol Eval ...))`
  emitted a partial protocol (Eval$iprot without Eval$iface) and broke at
  Dart load.
- deftype/defrecord impl specs are resolved by the cljd resolver even during
  host eval: Java interfaces (clojure.lang.IObj etc) in a spec fail with
  "Can't resolve X" although the pass runs on the JVM. Protocols defined in
  the same file work because the host eval defines them.
- Platform survey for the types port: no `type` fn (use `.-runtimeType`), no
  `class`. `satisfies?` instead of `instance?`/`implements?` on protocols.
  `aget` exists (Dart List). `fallback` is the extend-protocol catch-all
  (Object on clj, default on cljs). IMeta/IWithMeta/IFn use cljs-style
  `-meta`/`-with-meta`/`-invoke` names. defmulti/defmethod supported.
- cljd follows the :cljs code path for sci: NodeR defrecord + eval fn instead
  of the Eval protocol, constants self-evaluate (`->constant` returns x).
- Emission pass features are #{:cljd} only (host pass has :cljd AND :clj). A
  `#?(:clj A :cljs B)` without :cljd or :default clause emits NOTHING: in
  expression position that means nil, `(throw #?(...))` becomes a bare Dart
  `rethrow`. Every conditional in ported code needs a :cljd or :default arm.
- cljd host eval does not create host vars for plain defns, so `:refer` of a
  function from a ported ns breaks the host pass. Use an alias instead.
  `:refer` from clojure.test works.
- No object-array on cljd: use `(List/filled n nil)`. aset/aget work on Dart
  List.
- Compiling a SUBSET of test namespaces can regenerate cljd.core without IFn
  arity mixins other namespaces need, leaving stale .dart files that fail to
  load. Run the full selector when output looks inconsistent.
