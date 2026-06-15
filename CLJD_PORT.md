# ClojureDart (cljd) port - status

Branch: `cljd`. Goal: compile SCI to Dart via ClojureDart so Clojure can be
evaluated on the Dart CLI and eventually a Flutter mobile app. SCI is the eval
engine; cljd just compiles SCI's .cljc source to Dart. cljd's lack of runtime
`eval` is NOT a blocker - SCI provides its own eval.

## Status: STARTED (early). Does not compile yet.

Depends on the edamame `cljd` branch (reader port). See edamame/CLJD_PORT.md.

## Bootstrap / how to build

Scratch project: `~/dev/babashka/sci-cljd/`
- deps.edn points at local edamame (cljd branch) + local ClojureDart + this
  repo's src on the `:cljd` alias `:extra-paths`.
- `src/sci_cljd/main.cljd` = test entry: `(sci/eval-string "(+ 1 2 3)")`.
- Build/run: `cd ~/dev/babashka/sci-cljd && clj -M:cljd compile && dart run`.

## Done

- Routed all 5 clojure.tools.reader.reader-types requires to
  edamame.impl.reader-types on cljd (core.cljc, impl/parser, impl/read,
  impl/interpreter, impl/load) + the IndexingReader instance? check in load.

## Current wall

cljd compiler crash compiling sci.impl.callstack:
"Cannot invoke IFn.invoke because this.source is null", traced to the
`^sci.lang.Namespace` type hint / sci.lang deftype interop layer. This is the
entry to SCI's core type system.

## Remaining (the bulk - revisit here)

1. sci.lang - Var / Namespace / Type deftypes (heavy platform code). START HERE.
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
