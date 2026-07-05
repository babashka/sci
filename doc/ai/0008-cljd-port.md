# ClojureDart (cljd) port - status

Branch: `cljd`. Goal: compile SCI to Dart via ClojureDart so Clojure can be
evaluated on the Dart CLI and eventually a Flutter mobile app. SCI is the eval
engine; cljd just compiles SCI's .cljc source to Dart. cljd's lack of runtime
`eval` is NOT a blocker - SCI provides its own eval.

## Status: PUBLIC API WORKS. sci.core compiles on cljd. sci/init, parse-next,
eval-form, eval-string, def, defn all pass (test/sci/parse_test.cljc runs
ungated on all platforms). future is absent on cljd. copy-ns works at
compile time: the :cljd/clj-host arm reads publics from the cljd compiler
registry (cljd.compiler/nses), skips $-named protocol infrastructure and
:private vars, scrubs compiler-internal meta keys (:macro-host-fn, :inline)
and emits the cljs-shaped -copy-ns call with :val symbols that compile to
Dart references. Runtime use throws.

Host interop on cljd goes through member-control override fns (merged from
master, #1048). Dart has no reflection, so the cljd arm of
eval-instance-method-invocation gets the receiver's `.-runtimeType` and looks
up its config, then applies the override fn from :instance-methods /
:instance-fields. Every member is effectively closed: unlisted members throw,
the `true` sentinel and :allow :all are meaningless (nothing to reflect). See
cljd_interop_test.cljd. Static overrides resolve at analysis through the same
member-disposition path.

Config is keyed on the `:class` Type object, not its name. Dart
`Type.toString()` is not stable under AOT obfuscation (Flutter release), so
matching on a stringified type name would silently break. Instead
normalize-classes builds a `:type->opts` index on cljd (from each entry's
`:class` value) and the eval branch dispatches on `(.-runtimeType obj)` by Type
identity. The config shape is unchanged from other platforms (`:class` already
exists there for reflection); only the internal index differs, so a `:classes`
map written for the JVM works on cljd if it carries `:class`.

This survives tree-shaking: an override body that calls a real Dart method
(`(fn [s] (.toUpperCase s))`) compiles to a direct call site, so AOT keeps the
method. Proven with examples/sci_interop/main.cljd compiled to a native exe
(`clojure -M:cljd compile sci-interop.main` then `dart compile exe`): the
binary prints "HELLO FROM SCI" and denies an unlisted member. This is why
overrides are the right interop model for Dart, which has no runtime
reflection. Next: static/field interop coverage, then wider host types.

Non-record deftype works on cljd: the cljs arm
of analyze-deftype* became :default (SciType path, platform-neutral).
SciRecord gained -contains-key? (under ILookup on cljd, not IAssociative)
and IPrint for record-style printing. to-string in deftype.cljc and
records.cljc are SciMultiFns on cljd (Object method impls register at
runtime). demunge on cljd maps underscores to dashes for cross-ns type
imports. Test sweep: selector runs core, error, vars, namespaces, io, repl,
impl, multimethods, protocols, core-protocols, defrecords-and-deftype and
reify tests on cljd (271 assertions). reify runs the pure-protocol tests;
host-interop reify tests stay `#?(:clj ...)` and elide. read-test is JVM-only
(host readers), skipped. Remaining unported test namespaces are host- or
thread-specific: interop-test (host interop), interrupt-fn-test (threads, Dart
is single-isolate), hierarchies-test (no hierarchies on cljd).

Multimethods and protocols work on cljd via a sci-owned SciMultiFn
(multimethods.cljc): method table in an atom, exact-match dispatch plus
:default, no hierarchies (like the host), so prefer-method stays
unsupported. Built-in protocol multifns (-deref, -swap!, -reset!,
sci-invoke) are SciMultiFns seeded at construction, Dart cannot add methods
to host defmultis at runtime. Class dispatch values normalize through
:class in multi-fn-add-method-impl, Object extension maps to :default.
Dart core types are registered as dart.core.String etc with import
aliases, bare int/double/bool compile to cast fns on cljd so the registry
captures runtimeType Type objects instead. Reified has a cljd IFn mixin
calling its method map directly, like cljs. Multimethods and protocols work
(defmulti/defmethod/defprotocol/reify), see multimethods.cljc. No hierarchies,
so prefer-method and derive-based dispatch stay unsupported.

## Real test suite on cljd

- core_test.cljc runs on cljd directly (shadow was merged back, single file).
  Run: `clojure -M:cljd test sci.core-test` (add to script selector when
  green). Status: ALL PASS with a patched local ClojureDart
  (`clojure -M:cljd:cljd-local test ...`). With the unpatched git dep 3 tests
  fail (assert-test, def-location-test, defn-test), all caused by an upstream
  ClojureDart bug: cljd.core/list is compiled from a `^PersistentList ()`
  literal (clj/src/cljd/core.cljd:3436) whose reader meta {:line 3436 :column
  36 :tag PersistentList} is baked into the empty list, so EVERY runtime-built
  list carries that meta and sci reads it as error location. Local fix in
  ~/dev/ClojureDart: bind the loop seed to -EMPTY-LIST instead of the literal.
  PR sent to tensegritics; until merged the :cljd alias pins the
  borkdude/ClojureDart fork (branch fix-list-empty-literal-meta) and
  sci.core-test is in the CI selector. Swap the dep back when the PR lands.
  cljd-incompatible tests are gated with `#?(:cljd nil :default (deftest
  ...))` + a ;; TODO:cljd marker.
- sci fns use the direct fixed-arity representation like the other platforms
  (no wrapper). Runtime arity mismatch surfaces as a wrapped Dart
  NoSuchMethodError with correct location (str fallback in
  rethrow-with-location-of-node). Macro application arity errors keep the
  Clojure-style "Wrong number of args (n) passed to: ns/name" message via a
  NoSuchMethodError catch in analyze-call. 1M sci fn calls: 116ms Dart AOT vs
  33ms bb (was 352ms with the apply wrapper).
- rethrow-with-location-of-node falls back to (str e) when (ex-message e) is
  nil (all plain Dart errors).
- alter-meta!/reset-meta! route through IResetMeta-aware helpers in utils on
  cljd (cljd.core versions cast to Atom). Writability check precedes applying
  f, see built-in-vars-are-read-only-test.
- instance-impl on cljd: registry maps use their :instance? closure, non-Type
  values throw "is not a class".
- Map/set literals get explicit duplicate-key checks on cljd (map-fn and the
  set constructor in analyzer), hash-map based, insertion order NOT preserved
  (no array-map on Dart). array-map in sci cljd core is hash-map.
- cljd test regex literals cannot use inline (?i) flags (Dart RegExp).
- sci.test-utils ported: tu/eval* goes through sci/eval-string on cljd,
  thrown-with-data? is a boolean macro there (cljd assert-expr not
  extensible), submap? handles RegExp.
- Shadow edit patterns: #?(:clj Exception :cljs js/Error) etc get a
  :cljd cljd.core/ExceptionInfo arm (sci errors are ex-info, ExceptionInfo is
  NOT an Exception subtype on Dart). :classes maps get :cljd {}. Host-interop
  tests are #_-gated with a ;; TODO:cljd marker, but reader-level breakage
  (vanishing conditionals in maps) must be fixed even inside #_.
- Exception/constructor registry in opts default-classes: :constructor and
  :instance? closures per class, used by analyze-new and eval-try on cljd.

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
- Analyzer shallow deps ported: callstack, faster, fns, destructure, resolve,
  records. sci.cljd-smoke-test requires them as a compilation canary.
  cljd SciRecord arm: no -contains-key? (cljd IAssociative only has -assoc),
  no munge/hash-unordered-coll/ICloneable/IIterable on cljd.
- Test scaffolding (cljd_smoke_test, impl/destructure_test) removed after the
  real test suite sweep, coverage lives in the regular test namespaces.
- .cljd test shadow trick (for big test files like core_test): same ns in a
  .cljd file shadows the .cljc on cljd only. JVM/CLJS never read .cljd. For
  small files prefer gating in the .cljc directly. Gotcha either way: the
  reader resolves ::alias/kw at read time even in skipped branches and in #_
  forms, so use fully qualified keywords when the alias is cljd-gated.

- Analyzer, evaluator, load, deftype, ctx-store compile on cljd, zero dynamic
  warnings. Analyzer notes: host pass features are exactly
  #{:cljd :cljd/clj-host :clj} (compiler.cljc host-load-input), emission is
  #{:cljd}. macros/? does not resolve on the cljd host, use
  `#?(:cljd ... :default (macros/? ...))` inside macro bodies instead (host
  read has :cljd so plain reader conditionals work there, unlike cljs).
  `^objects` breaks cljd, use `^List` (also avoids aset/aget dynamic
  warnings). No object-array/array-map, use List/filled and hash-map.
  cljd catch-all is `(catch Object e)`. Non-record sci deftype on cljd
  returns nil from analyze-deftype* (needs a cljd arm, see tasks).
  utils/reset-meta!* + types/IResetMeta replace reset-meta! for sci types.

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

## cljd gotchas (learned during namespaces layer port)

- Helper fns used by macros need ^:macro-support meta or the macro fails to
  host-compile with "Unable to resolve symbol". cljd host eval does not
  JVM-define plain defns.
- cljd host eval does not split top-level do: helpers and the macros using
  them must live in separate top-level (deftime) forms.
- :refer-clojure :exclude of `reify` breaks compilation of any variadic fn
  in that namespace (cljd emits reify for the IFn mixin). sci.impl.reify
  names its macro fn reify-macro on cljd.
- defmethod: top level only (not inside def) and single arity only.
  defmulti with :hierarchy option not supported.
- No get-method/methods/prefer-method/remove-method runtime API.
- No resolve at runtime. No format. Quoted JVM class syms in var meta (:tag)
  fail to compile, copy-var drops tags on cljd.
- `(def x (copy-core-var x))` with x excluded from clojure.core
  self-references on cljd and stack-overflows at load, pass {:init nil}.
- Return type hints on fns that can return nil (e.g. ^TBox) become Dart
  casts and throw on nil, drop them on cljd.
- (List/filled n nil) infers Dart List<Null> and (volatile! nil) infers a
  Null-typed slot: aset/vreset! of a real value then throws "x is not a
  subtype of type 'Null'". Use (#/(List/filled dynamic) n nil) and
  (volatile! (identity nil)).
- Syntax-quoted `fn etc resolve to cljd.core/... on the cljd host:
  strip-core-ns and resolve treat cljd.core like cljs.core.
- Syntax-quoted core syms MISSING from cljd.core (resolve, find-ns, ns-name,
  var?) silently qualify into the CURRENT ns, breaking emitted sci code at
  runtime. Emit ~'sym instead (doc macro). Suspect any macro fn in
  namespaces.cljc that emits less-common core fns.
- cljd drops top-level side-effect forms entirely: the (vreset! utils/...)
  wiring of the circular-dep volatiles never ran. interpreter/-install-wiring!
  is called from eval-form, eval-string* and sci.core/init on cljd.
- ->Var/->Type/->Namespace factories work on ALL platforms, prefer them over
  platform-conditional constructor calls. utils/sci-type? for Type instance
  checks.
