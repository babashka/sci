# Changelog

For a list of breaking changes, check [here](#breaking-changes-1)

[SCI](https://github.com/babashka/sci): Configurable Clojure/Script interpreter suitable for scripting and Clojure DSLs

SCI is used in [babashka](https://github.com/babashka/babashka),
[nbb](https://github.com/babashka/nbb),
[clerk](https://github.com/nextjournal/clerk),
[joyride](https://github.com/BetterThanTomorrow/joyride/) and many
[other](https://github.com/babashka/sci#projects-using-sci) projects.

## Unreleased

- Add `*suppress-read*`
- Add `load-reader`
- Fix [#872](https://github.com/babashka/sci/issues/872): `*loaded-libs*` is now the single source of truth about loaded libs
- Fix [#981](https://github.com/babashka/sci/issues/981): respect type hint on instance method callee
- Add core dynamic vars like `*warn-on-reflection*` and bind them during
  `load-string` etc. such that `set!`-ing then in a `future` works.
- Fix [#984](https://github.com/babashka/sci/issues/984): support alternative `set!` syntax in CLJS
- Fix [#986](https://github.com/babashka/sci/issues/986): preserve error location for js static method

## 0.10.47 (2025-06-27)

> [!IMPORTANT]
> This release contains a security fix

- Security issue: function recursion can be forced by returning internal keyword as return value
- Fix [#975](https://github.com/babashka/sci/issues/975): Protocol method should have :protocol var on metadata
- Fix [#971](https://github.com/babashka/sci/issues/971): fix `satisfies?` for protocol that is extended to `nil`
- Fix [#977](https://github.com/babashka/sci/issues/977): Can't analyze sci.impl.analyzer with splint

## 0.10.46 (2025-06-18)

> [!IMPORTANT]
> Important change regarding sandboxing!

When SCI is used to produce a function that contains operations on the context,
like `ns`, `def`, `intern`, `ns-unmap` etc. this function no longer holds on to
the context it was produced in. Example:

``` clojure
(require '[sci.core :as sci])
(def f (sci/eval-string "(fn [x] (intern 'user 'foo x))"))
(f 1) ;;=>
Execution error (IllegalStateException) at sci.ctx-store/get-ctx (ctx_store.cljc:30).
No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

You can make these functions work by executing them within the scope of a `sci.ctx-store/with-ctx`:

``` clojure
(def ctx (sci/init {}))
(def f (sci/eval-string* ctx "(fn [x] (intern 'user 'foo x))"))
(sci.ctx-store/with-ctx ctx
  (f)) ;; works
```

This change was necessary to remove a bunch of "magic" from SCI where functions
that operated on the context closed over the context. This is no longer the
case. The reason that this feature was removed is that these special functions
took one extra argument, the context and therefore didn't work properly with
`with-redefs` where the user would substitute a function with the expected
number of arguments. I.e. this wouldn't work in SCI (and babashka) before the
change: `(with-redefs [intern my-intern] (intern 'foo 'bar))` since intern took
an extra (unexpected) context argument.

Functions that escape SCI evaluation that do not work on the context,
e.g. `assoc`, still work as before:

``` clojure
(require '[sci.core :as sci])
(def f (sci/eval-string "(fn [x] (assoc {} x x))"))
(f 1) ;;=> {1 1}
```

Other changes and fixes in this release:

- Fix [#957](https://github.com/babashka/sci/issues/957): `sci.async/eval-string+` should return promise with `:val nil` for ns form rather than `:val <Promise>`
- Fix [#959](https://github.com/babashka/sci/issues/959): Java interop improvement: instance method invocation now leverages type hints
- Bump edamame to `1.4.30`
- Give metadata `:type` key priority in `type` implementation
- Fix [#967](https://github.com/babashka/sci/issues/967): `ns-name` should work on symbols
- Fix [#969](https://github.com/babashka/sci/issues/969): `^:clojure.core/eval-file` metadata should affect binding of `*file*` during evaluation
- Sync `sci.impl.Reflector` with changes in `clojure.lang.Reflector` in clojure 1.12.1
- Fix `:static-methods` option for class with different name in host
- Fix [#973](https://github.com/babashka/sci/issues/973): support `with-redefs` on core vars, e.g. `intern`. The fix for this
  issue entailed quite a big refactor of internals which removes "magic"
  injection of ctx in core vars that need it.
- Add `unchecked-set` and `unchecked-get` for CLJS compatibility

## 0.9.45 (2025-03-11)

- Fix [#942](https://github.com/babashka/sci/issues/942): improve error location of invalid destructuring
- Add `volatile?` to core vars
- Fix [#950](https://github.com/babashka/sci/issues/950): interop on local in CLJS
- Bump edamame to `1.4.28`

## 0.9.44 (2024-10-17)

- Fix [#917](https://github.com/babashka/sci/issues/917): support new Clojure 1.12 Java interop: `String/new`, `String/.length` and `Integer/parseInt` as fns
- Fix [#925](https://github.com/babashka/sci/issues/925): support new Clojure 1.12 array notation: `String/1`, `byte/2`
- Fix [#926](https://github.com/babashka/sci/issues/926): Support `add-watch` on vars in CLJS
- Support `aset` on primitive array using reflection
- Fix [#928](https://github.com/babashka/sci/issues/928): record constructor supports optional meta + ext map
- Fix [#934](https://github.com/babashka/sci/issues/934): `:allow` may contain namespaced symbols
- Fix [#937](https://github.com/babashka/sci/issues/937): throw when copying non-existent namespace
- Update `sci.impl.Reflector` (used for implementing JVM interop) to match Clojure 1.12

## 0.8.43 (2024-08-06)

- Fix shadow-cljs warnings

## 0.8.42 (2024-07-23)

- Fix [#626](https://github.com/babashka/sci/issues/626): add `cljs.core/exists?`
- Fix [#906](https://github.com/babashka/sci/issues/906): `merge-opts` loses `:features` or previous context
- Fix [#923](https://github.com/babashka/sci/issues/923): check for duplicate keys in dynamic set or map literals
- Fix [#919](https://github.com/babashka/sci/issues/919): `:js-libs` + `refer` + `rename` clashes with core var
- Add `hash-ordered-coll`
- `read-string` should use non-indexing reader for compatibility
- Bump edamame

## 0.8.41 (2023-11-24)

- Bump edamame to 1.3.23
- [#889](https://github.com/babashka/sci/issues/889): allow `(def foo/foo 1)` when inside namespace `foo`
- [#891](https://github.com/babashka/sci/issues/891): reset file metadata on var when it's re-evaluated from other file
- [#893](https://github.com/babashka/sci/issues/893): expose `sci.async/eval-form` and `sci.async/eval-form+`
- Improve `sci.async/eval-string`, respect top-level `do` forms
- Add experimental new `:static-methods` option to override how static methods get evaluated.
- Expose `destructure`
- Macroexpand `(.foo bar)` form
- Optimize `deref`, `swap!`, `reset!` for host values
- Add `time` macro to core namespace
- [#896](https://github.com/babashka/sci/issues/896): allow `catch` to be used as var name

## 0.8.40 (2023-06-28)

- [#888](https://github.com/babashka/sci/issues/888): add `eval-string+` with explicit initial `:ns` and explicit last active `:ns` in return value
- [#683](https://github.com/babashka/sci/issues/683): better error message when trying to recur across try
- [#884](https://github.com/babashka/sci/issues/884): preserve error location in future with `:sci/error`
- [#886](https://github.com/babashka/sci/issues/886): support `:require-macros` in CLJS

## 0.7.39 (2023-03-07)

- [#874](https://github.com/babashka/sci/issues/874): Keyword arguments as map support for CLJS
- Mutable deftype fields can be marked as such with `^:volatile-mutable` in CLJS
- Fix `.toString` implementation on `deftype`
- Resolve JS library alias via imported 'class'
- Fix babashka issue 1501: equals on deftype
- Honor `:ns-aliases` for built-in libs
- Fix issue with copy-ns + protocol
- Support `:sci/macro` for copy-ns and copy-var for copying macro functions
- Fix [#876](https://github.com/babashka/sci/issues/876): SCI analysis too eager when looking up class
- Optimizations with respect to looking up static fields

## 0.7.38 (2023-02-07)

- Add `sci/add-js-lib!` for adding js libraries including corresponding `:js-libs` init option
- Speed up Java interop around 5x by caching method lookups
- Allow destructucturing in CLJS `defmethod`
- [#862](https://github.com/babashka/sci/issues/862): fix JS constructor from class in CLJS namespace
- Improve error location in macroexpansion
- [#867](https://github.com/babashka/sci/issues/867): Support `add-watch` and `remove-watch` on `sci.lang.Var`
- [#648](https://github.com/babashka/sci/issues/648): implement `*loaded-libs*` and `(loaded-libs)`
- Expose new `parse-next+string` function
- Support qualified method names in `proxy` while ignoring the namespace
- Support `read` with non-indexing reader, fixes babashka/babashka#1465
- Improve top level macro expansion error location
- Fix pprinting vars
- Add `reader-conditional` to core vars
- Fix [#babashka/1482](https://github.com/babashka/babashka/issues/1482): make loading of libs thread safe

## 0.6.37 (2022-12-20)

- [#839](https://github.com/babashka/sci/issues/839): Performance improvement for method calls in CLJS, like `(Math/sin)` and `(.substring "foo" 0 1)`, around 5 - 7x faster
- Performance improvement when calling anonymous `fn`
- Improve `case` performance
- [#855](https://github.com/babashka/sci/issues/855): Property access in JS doesn't throw when intermediate value is `nil`
- [#842](https://github.com/babashka/sci/issues/842): Error metadata missing on some interop calls ([@bobisageek](https://github.com/bobisageek))
- [#856](https://github.com/babashka/sci/issues/856): fix `#queue [1 2 3]` literal
- Implement `lazy-seq` as macro
- Implement `ns` as macro
- [#844](https://github.com/babashka/sci/issues/844): Don't resolve `cljs.core/inc` in JVM env
- Add `case` as a macro and `case*` as special form, rather than having `case` as special form
- Implement `and` and `or` as macros
- Implement `fn`, `defn` and `defmacro` as macros, while keeping `fn*` as special form
- Implement `loop` as macro, while supporting `loop*` as special form
- Implement `let` as macro, while supporting `let*` as special form
- Fix js type hint on `await` ([@mhuebert](https://github.com/mhuebert))
- Convert libsci scripts to bb tasks, include in CI multi-os ([@ikappaki](https://github.com/ikappaki))
- Bump edamame (Clojure parser)
- Fix benchmarking code
- Fix for `SCI_ELIDE_VARS`

## 0.5.36 (2022-11-14)

- Revert commit that fixed [#832](https://github.com/babashka/sci/issues/832)
  since it caused more issues than it solved. See
  [here](https://github.com/babashka/sci#troubleshooting) for workaround.

## 0.5.35 (2022-11-14)

- [#817](https://github.com/babashka/sci/issues/817): mutation of `deftype` field should be visible in protocol method
- [#816](https://github.com/babashka/sci/issues/816): restore recur target exception in do
- [#819](https://github.com/babashka/sci/issues/819): don't use metadata in record implementation
- [#831](https://github.com/babashka/sci/issues/831): preserve location of `throw` in stack trace
- Drop name metadata from conditionally defined var
- Implement `in-ns` as function rather than built-in ([@SignSpice](https://github.com/SignSpice))
- [#832](https://github.com/babashka/sci/issues/832): reload analyzer API within CLJS to ensure `ns-publics` and `resolve` are available
- Optimize analysis and evaluation of `fn`
- Add more docstrings to built-in (core) macros and vars ([@mhuebert](https://github.com/mhuebert))
- Add `array`

## v0.5.34 (2022-10-18)

- Performance optimizations for `let` (up to 8x faster)
- [babashka/babashka#1340](https://github.com/babashka/babashka/issues/1340): Add arglists/docstring to protocol methods ([@bobisageek](https://github.com/bobisageek))
- [#801](https://github.com/babashka/sci/issues/801): preserve location metadata for vars defined in macro-expansion
- Add new `var->symbol` API function in `sci.core`
- Add new `resolve` API function in `sci.core`
- Expose `*print-namespace-maps*` from `sci.core` ([@ghoseb](https://github.com/ghoseb))
- Internal simplifications for `and`, `or` and `do`

## v0.4.33 (2022-09-15)

- [#791](https://github.com/babashka/sci/issues/791): Fix friendly arity exception messages for Clojure 1.10, 1.11 ([@lread](https://github.com/lread))
- [#794](https://github.com/babashka/sci/issues/794): Support for `IPrintWithWriter` for custom types in CLJS
- Add `ns-unalias` ([@eerohele](https://github.com/eerohele))
- Resolve record name with dot: `Foo.`
- Implement `addWatch` / `removeWatch` noops on SCI vars to prevent segfault in native images
- Improvements to error locations in exceptions
- Fix type hints on locals with same names ([@lread](https://github.com/lread))
- Add `demunge` in CLJS ([@sirwobin](https://github.com/sirwobin))
- [#785](https://github.com/babashka/sci/issues/785): `satisfies?` finds protocols that have no methods ([@lilactown](https://github.com/lilactown))
- [#781](https://github.com/babashka/sci/issues/781), [#782](https://github.com/babashka/sci/issues/782) and [#783](https://github.com/babashka/sci/issues/783): extend protocols to JS built ins using CLJS "type symbols", fix extending protocols to default and `Object` ([@lilactown](https://github.com/lilactown))
- Add `keyword-identical?` in CLJS
- Fix `deftype` mutable fields for CLJS
- `sci.ctx-store`: a place to store the context so it's available for `sci.configs` to use. See [docs](https://github.com/babashka/sci/blob/master/API.md#scictx-store).
- Make loading `cljs.analyzer.api` more reliable
- Don't partial-ize ctx-needing vars that aren't built-in
- [#774](https://github.com/babashka/sci/issues/774): make interpreter stacktrace available on exception if annotated with `^:sci/error`
- Allow unrestricted access with `set!` in CLJS if `sci.core/enable-unrestricted-access` has been called.
- Don't eval metadata on `defn` expression
- Add `hash-combine`
- [#729](https://github.com/babashka/sci/issues/729): mutable fields in deftype

## v0.3.32

- [#767](https://github.com/babashka/sci/issues/767): Reduce advanced compiled JS output with about 20% (~900kb -> ~740kb)
- [#768](https://github.com/babashka/sci/issues/768): copy `:dynamic` metadata in `copy-ns`
- [#771](https://github.com/babashka/sci/issues/771): Fix `or` with more than 20 args

## v0.3.31

- Fix `sci/copy-ns*`, the name was not copied correctly :(

## v0.3.30

- Support globally valid `:ns-aliases` for mapping e.g. `clojure.test` to `cljs.test`
- New [`copy-var*`](https://github.com/babashka/sci/blob/master/API.md#copy-var-1) API function
- [#755](https://github.com/babashka/sci/issues/755): make `sci.lang.Var` part of public API
- [#755](https://github.com/babashka/sci/issues/755): make `sci.lang.Namespace` part of public API
- Add [`add-namespace!`](https://github.com/babashka/sci/blob/master/API.md#add-namespace) API function
- Fix invalid arity problem in CLJS macros with arity >= 20
- Bump [edamame](https://github.com/borkdude/edamame) to v1.0.0
- [#762](https://github.com/babashka/sci/issues/762), [#731](https://github.com/babashka/sci/issues/731): print via global `print-method` and `print-dup` methods, don't allow by default
- [#733](https://github.com/babashka/sci/issues/733): do not allow to change meta on built-in macro
- Various `defrecord` improvements (equality, metadata preservation)
- [#738](https://github.com/babashka/sci/issues/738): dotted simple symbol should remain unresolved
- [#741](https://github.com/babashka/sci/issues/741): preserve closed over value analysis when using fixed + varargs arity
- [#739](https://github.com/babashka/sci/issues/739): `satisfies?` does not recognize marker protocol on record
- [#740](https://github.com/babashka/sci/issues/740): simple-dispatch on records
- [#743](https://github.com/babashka/sci/issues/743): `satisfies?` of marker protocol via `extend-`{`type`, `protocol`}
- [#744](https://github.com/babashka/sci/issues/744): syntax quote for records should return class representation
- [#745](https://github.com/babashka/sci/issues/745): syntax quote of imported class
- [#747](https://github.com/babashka/sci/issues/747): provide default pprint impl for SCI records
- [#748](https://github.com/babashka/sci/issues/748): fix async refer
- [#757](https://github.com/babashka/sci/issues/757): fix simultaneous async evaluations
- Record types are now an instance of `sci.lang.Type`
- [#763](https://github.com/babashka/sci/issues/763): record in syntax quote resolves to `sci.lang.Type`
- [#751](https://github.com/babashka/sci/issues/751): defprotocol returns symbol

## v0.3.5

- [#711](https://github.com/babashka/sci/issues/711): support `*print-dup*`
- [#712](https://github.com/babashka/sci/issues/712): destructuring in protocol impl not working
- [#715](https://github.com/babashka/sci/issues/715): allow property access on vars in CLJS
- Add `->Eduction`
- Expose `print-err-fn` in SCI core API
- add `:name` metadata via helper functions and use metadata for var names
- Improve error reporting for `let*` and `try`
- [#714](https://github.com/babashka/sci/issues/714): Improve instance member call parity with Clojure/JVM
-  Default `*read-eval*` / `sci/read-eval` to false
- Expose `all-ns` and `find-ns` in `sci/core`
- Fix for `copy-ns` when var has no metadata
- Add `add-class` and `add-import` API functions in `sci/core`
- `sci.async`: EXPERIMENTAL namespace for async evaluation in CLJS

## v0.3.4

- Restore compatibiliy with self-hosted CLJS (e.g. planck)
- Add CLJS `*print-err-fn*` var

## v0.3.3

### New

- [#382](https://github.com/babashka/sci/issues/382): Add `*clojure-version*` and `(clojure-version)`
- Support `:sigs` on protocol map
- Add `unchecked-dec`

### Enhancements

- [#689](https://github.com/babashka/sci/issues/689): Reified object doesn't get handled for fallback protocol impl
- [#692](https://github.com/babashka/sci/issues/692): extending `Object` with protocol doesn't work on records
- [#693](https://github.com/babashka/sci/issues/693): extend protocol to `IRecord` doesn't work on SCI records
- [#695](https://github.com/babashka/sci/issues/695): `(identical? [] [])` works
- [#700](https://github.com/babashka/sci/issues/700): `loop` doesn't expand in fully qualified `let`
- [#696](https://github.com/babashka/sci/issues/696): add `let*` special form
- [#702](https://github.com/babashka/sci/issues/702): require `cljs.analyzer.api` for `sci/copy-ns`

## v0.3.2

- [#665](https://github.com/babashka/sci/issues/665): expose `*assert*`
- Drop location metadata from symbols
- [#687](https://github.com/babashka/sci/issues/687): `declare` as macro
- [#684](https://github.com/babashka/sci/issues/684): support `print-method` on records

## v0.3.1

### Enhancements

- Fix [#680](https://github.com/babashka/sci/issues/680): allow expression in class position of `new` in JS targets
- Performance improvements

## v0.3.0

### Enhancements

- [#416](https://github.com/babashka/sci/issues/416): mutable arrays for bindings. Takes care of approx. 2-4x speedup in loops (depending on environment).
- Performance improvements for varargs function invocations.

### Changes

- Remove `:disable-arity-checks` option. Arity checks are now always taken care of
  by the host environment and SCI does not do any special handling. This means
  that in JS environments there will be no arity checking regardless of this
  option. This change was introduced for better performance.

## v0.2.9

- Feature: add `halt-when`, `pop!`, `array?`, `js-delete`, `object?` and `undefined?`
- Feature [#616](https://github.com/babashka/sci/issues/616): `:as-alias` support
- Fix [#498](https://github.com/babashka/sci/issues/498): Cannot recur from non-tail position
- Fix [#653](https://github.com/babashka/sci/issues/653): Improve error message for higher order function arity exception.
- Fix [#655](https://github.com/babashka/sci/issues/655): `merge-opts` should overwrite existing var
- Fix [#660](https://github.com/babashka/sci/issues/660): In JS you can throw and catch anything
- Fix [#664](https://github.com/babashka/sci/issues/664): cannot take value of macro `->`
- Fix [#659](https://github.com/babashka/sci/issues/659): meta evaluation order ([@erdos](https://github.com/erdos))
- Fix [#666](https://github.com/babashka/sci/issues/666): use `js/Reflect` when calling instance function, adds compatibility with GraalVM JS Polyglot
- Fix [#673](https://github.com/babashka/sci/issues/673): use `js/Reflect` when calling constructor, adds compatibility with GraalVM JS Polyglot
- Fix [#677](https://github.com/babashka/sci/issues/677): Fix `macroexpand` for `defrecord`
- Fix function with arity 15
- Fix macro which expands in `defrecord` + protocol which invokes record constructor

## v0.2.8

- Prefer metadata implementation override over `extend-protocol` [#378](https://github.com/babashka/sci/issues/378)
- Prefer metadata implementation in `extend` `extend-protocol` [#378](https://github.com/babashka/sci/issues/378)
- Make constructors in CLJS more dynamic [#624](https://github.com/babashka/sci/issues/624)
- Support Object toString override on defrecord [#627](https://github.com/babashka/sci/issues/627)
- Bump edamame to `0.0.18`
- Add `copy-ns` macro
- `clojure.core/read` improvements [#663](https://github.com/babashka/sci/issues/663)
- `clojure.core/read` can read with standard `PushbackReader`
- Support `*read-eval*`
- Support `*default-data-reader-fn*`
- Support `*reader-resolver*`
- Intern fails to rebind a referred var if already present in config [#637](https://github.com/babashka/sci/issues/637)
- Support adding protocol for satisfies check [#638](https://github.com/babashka/sci/issues/638)
- Add `reader-conditional?`
- Add CLJS `random-uuid?`
- Add `clojure.test/test`
- Expose `doseq` and `for` as normal macros ([@erdos](https://github.com/erdos))
- Fix `declare` with dynamic var [#630](https://github.com/babashka/sci/issues/630) ([@erdos](https://github.com/erdos))
- Support `ns` `:require` with string instead of symbol ([@djblue](https://github.com/djblue))
- Add `inst-ms`
- Fix `resolve` on class method (should return `nil`) [#647](https://github.com/babashka/sci/issues/647)
- Support getting instance fields from JVM classes
- Add `to-array-2d`, `aclone`, `reduce` and `amap` [#650](https://github.com/babashka/sci/issues/650) ([@MrEbbinghaus](https://github.com/MrEbbinghaus))

## v0.2.7

This release focuses on:

- Improvements regarding CLJS compatibility (interop, js literals, printing via `*print-fn*`)
- Adding more dynamic vars related to printing
- Exposing `sci/stacktrace` and `sci/format-stacktrace` for getting a stacktrace
  from a SCI exception. This allows you to build an error report like babashka
  and similar tools built with SCI.

The changes per issue:

- Fix function reference equality
  [#587](https://github.com/babashka/sci/issues/587)
- Don't convert Clojure values to JS values automatically in interop
  [#602](https://github.com/babashka/sci/issues/602)
- Calling comment with 49 (or more) args fails in CLJS [#603](https://github.com/babashka/sci/issues/603)
- Fix stacktrace for invalid import [#589](https://github.com/babashka/sci/issues/589)
- Add `js-keys`
- Support `create-ns`
- Varify more core vars ([@bobisageek](https://github.com/bobisageek))
- Fix constructor call of class added via imports
- Keep `:disable-arity-checks` after merge-opts
- Add `*flush-on-newline*`
- Add `*print-readably*`
- Support `:clojure.core/eval-file` metadata
- reify form from macro doesn't evaluate correctly [#609](https://github.com/babashka/sci/issues/609)
- Handle js literals in the same way as ClojureScript [#610](https://github.com/babashka/sci/issues/610)
- Control printing in CLJS via `*print-fn*` and via API `sci/print-fn` rather
  than `*out*` [#365](https://github.com/babashka/sci/issues/365)
- Make stacktrace logic public [#590](https://github.com/babashka/sci/issues/590)

## v0.2.6

- Use `IllegalArgumentException` in `case` when providing duplicate dispatch values
- Improve error message when protocol not found for class
- Add `thread-bound?` predicate [#560](https://github.com/babashka/sci/issues/560)
- Allow `set!` to mutate objects in CLJS [#563](https://github.com/babashka/sci/issues/563)
- Experimental `SCI_ELIDE_VARS` environment variable for smaller CLJS bundle size
- Support trailing metadata in `defn` [#567](https://github.com/babashka/sci/issues/567)
- GC improvement: don't hang on to all external bindings in closure
- Several performance improvements
- Expose `intern` in core API
- Several fixes regarding `try`/`catch` in CLJS [#583](https://github.com/babashka/sci/issues/583), [#584](https://github.com/babashka/sci/issues/584), [#585](https://github.com/babashka/sci/issues/585)
- `case` expression generated by macro doesn't work correctly [#586](https://github.com/babashka/sci/issues/586)

## v0.2.5

### Enhancements / fixes

- Fix metadata on non-constant map literal expression [#546](https://github.com/babashka/sci/issues/546)
- Support `:reload-all` [#552](https://github.com/babashka/sci/issues/552)
- Support new kwargs handling from 1.11.0 [#553](https://github.com/babashka/sci/issues/553).
- Allow dynamic `:doc` on `def`/`defn` [#554](https://github.com/babashka/sci/issues/554).
- Fix metadata on nested evaluated map [#555](https://github.com/babashka/sci/issues/555)
- Bug with protocol methods in record where later arg overrides "this" [#557](https://github.com/babashka/sci/issues/557)
- Support :rename in :refer-clojure [#558](https://github.com/babashka/sci/issues/558)
- Add `aset-...`, `delay?`, `bit-clear`
- Add `bound-fn` and `bound-fn*`
- Add arg count check for `clojure.core/for`

## v0.2.4

### New

- Detect cyclic load dependencies [#531](https://github.com/babashka/sci/issues/531)
- Add `force`
- Add `dissoc!` ([@wilkerlucio](https://github.com/wilkerlucio))

### Enhancements / fixes

- BREAKING: Do not merge ex-data into sci error [#534](https://github.com/babashka/sci/issues/534) ([@GreshamDanielStephens](https://github.com/GreshamDanielStephens))
- Pick fn arity independent of written order [#532](https://github.com/babashka/sci/issues/532) ([@GreshamDanielStephens](https://github.com/GreshamDanielStephens))
- `(instance? clojure.lang.IAtom 1)` returns `true` [#537](https://github.com/babashka/sci/issues/537)
- Fix `ns-unmap` on referred var [#539](https://github.com/babashka/sci/issues/539)

## v0.2.3

### Enhancements / fixes

- if with falsy literal returns nil [#529](https://github.com/babashka/sci/issues/529)

## v0.2.2

### Enhancements / fixes

- Fix error reporting in case of arity error [#518](https://github.com/babashka/sci/issues/518)
- Shadowing record field names in protocol functions [#513](https://github.com/babashka/sci/issues/513)
- Fix destructuring in protocol method for record [#512](https://github.com/babashka/sci/issues/512)
- Faster processing of maps, sets and vectors [#482](https://github.com/babashka/sci/issues/482)
- Prioritize current namespace vars in syntax quote [#509](https://github.com/babashka/sci/issues/509)
- Fix ns-publics to not include refers [#520](https://github.com/babashka/sci/issues/520)
- Add `refer-clojure` macro [#519](https://github.com/babashka/sci/issues/519)
- Syntax quote resolves referred var incorrectly [#526](https://github.com/babashka/sci/issues/526)
- Priorize referred vars over vars in current ns [#527](https://github.com/babashka/sci/issues/527)

## v0.2.1

### Enhancements / fixes

- Improvements for using `type` on `defrecord` [#492](https://github.com/babashka/sci/issues/492)
- Deref vars at analysis time that have `:inline` metadata in Clojure [#483](https://github.com/babashka/sci/issues/483)
- Keep only location metadata for seqs and symbols [#488](https://github.com/babashka/sci/issues/488)
- Conditionally defined vars should not have metadata [#496](https://github.com/babashka/sci/issues/496)
- Fix interop on map [#506](https://github.com/babashka/sci/issues/506)
- Performance improvements [#500](https://github.com/babashka/sci/issues/500), [#502](https://github.com/babashka/sci/issues/502), [#504](https://github.com/babashka/sci/issues/504)
- Fix shadow-cljs warnings [#499](https://github.com/babashka/sci/issues/499)

## v0.2.0

Thanks for contributing to this release:

[@lread](https://github.com/lread), [@patrick-galvin](https://github.com/patrick-galvin), [@SevereOverfl0w](https://github.com/SevereOverfl0w), [@djblue](https://github.com/djblue), [@kwrooijen](https://github.com/kwrooijen), [@sogaiu](https://github.com/sogaiu), [@joinr](https://github.com/joinr), [@RickMoynihan](https://github.com/RickMoynihan), [@galdober](https://github.com/galdober)

### Breaking changes

- Removed `:realize-max` and `:preset :termination-safe`. In the light of
  [#348](https://github.com/babashka/sci/issues/348) it would be misleading to
  claim that sci can guarantee termination within reasonable time.

### New

- Add `class?`, `iterator-seq`, `remove-watch`, `realized?`, `clojure.walk/macroexpand-all`, `find-var`, `lazy-cat`, `bound?`, `*print-namespace-maps*`, `get-thread-bindings`, `var-get`, `var-set`, `with-local-vars`
- Add `fork` API function [#369](https://github.com/babashka/sci/issues/369)
- Add API functions for parsing code and evaluating forms: `sci.core/reader`,
  `sci.core/parse-string`, `sci.core/parse-next`, `sci.core/eval-form` [#404](https://github.com/babashka/sci/issues/404)
- Support implementing `IDeref`, `IAtom`, `IAtom2` (and CLJS equivalents) [#401](https://github.com/babashka/sci/issues/401)
- Add API vars `print-meta`, `print-level` which can be used with
  `sci.core/binding` to control the dynamic var equivalent in sci programs
- Support calling `symbol` on a var [#453](https://github.com/babashka/sci/issues/453)
- `:disable-arity-checks` option: when used, sci behaves similarly to CLJS/JS by
  not checking the provided number of arguments and allowing less or more for
  single and fixed arity functions
  [#460](https://github.com/babashka/sci/issues/460)

### Enhancements / fixes

- Alter-var-root uses thread-bound value to update [#359](https://github.com/babashka/sci/issues/359)
- Eval metadata on var created with defn [#371](https://github.com/babashka/sci/issues/371)
- Metadata fn on var f fails if referring to f [#363](https://github.com/babashka/sci/issues/363)
- Fix missing protocol methods [#367](https://github.com/babashka/sci/issues/367) ([@patrick-galvin](https://github.com/patrick-galvin))
- Support multiple methods of protocol on defrecord
- Allow re-binding of core vars with clojure.core/with-redefs [#375](https://github.com/babashka/sci/issues/375)
- Fix false dynamic binding [#379](https://github.com/babashka/sci/issues/379)
- Don't eval record returned from reader function [#386](https://github.com/babashka/sci/issues/386)
- Implement `->` and `as->` as normal macros [#390](https://github.com/babashka/sci/issues/390), [#462](https://github.com/babashka/sci/issues/462) ([@kwrooijen](https://github.com/kwrooijen))
- `defn` should not introduce local for name in body [#384](https://github.com/babashka/sci/issues/384)
- Fix wrong arity in error message when calling macro [#392](https://github.com/babashka/sci/issues/392)
- Throw when trying to redefine referred var [#398](https://github.com/babashka/sci/issues/398)
- Fix for `use` [120175f](https://github.com/babashka/sci/commit/120175f2efc0328e88a832e792db342a70558806)
- Fix importing protocol classes from namespaces with hyphens [#410](https://github.com/babashka/sci/issues/410)
- Performance enhancements [#415](https://github.com/babashka/sci/issues/415), [#452](https://github.com/babashka/sci/issues/452), [#468](https://github.com/babashka/sci/issues/468), [#470](https://github.com/babashka/sci/issues/470), [#472](https://github.com/babashka/sci/issues/472), [#473](https://github.com/babashka/sci/issues/473), [#475](https://github.com/babashka/sci/issues/475), [#478](https://github.com/babashka/sci/issues/478), [#480](https://github.com/babashka/sci/issues/480)
- Support top-level do emitted from macro [#421](https://github.com/babashka/sci/issues/421)
- Support map constructor for maps [#431](https://github.com/babashka/sci/issues/431)
- Partial support for multiple reified classes [323a257](https://github.com/babashka/sci/commit/323a2574ec4d59a0544a829c1fa529fcbc110140)
- Fix calling literal symbol babashka/babashka#622
- Allow user-defined vars when def is allowed [#434](https://github.com/babashka/sci/issues/434)
- Fix default destructuring with false [#436](https://github.com/babashka/sci/issues/436)
- Fix reflection warning in multimethods code [#437](https://github.com/babashka/sci/issues/437) ([@galdober](https://github.com/galdober))
- Support nested libspecs [#399](https://github.com/babashka/sci/issues/399)
- Aliases in protocol functions should work [#440](https://github.com/babashka/sci/issues/440)
- Allow users to override :line metadata [#443](https://github.com/babashka/sci/issues/443)
- Support second arg (env) in `resolve`
- Preserve and eval reader meta on coll literals and functions [#447](https://github.com/babashka/sci/issues/447), [#448](https://github.com/babashka/sci/issues/448)
- Fix #js object reading [#449](https://github.com/babashka/sci/issues/449)
- Support unmap for imported classes [#432](https://github.com/babashka/sci/issues/432)
- Fix for reader conditional parsing borkdude/edamame#65
- Dotted field access for JS interop [#450](https://github.com/babashka/sci/issues/450)
- Syntax checks for binding [#458](https://github.com/babashka/sci/issues/458)
- Add `boolean?` to constant check [#465](https://github.com/babashka/sci/issues/465) ([@kwrooijen](https://github.com/kwrooijen))
- Check macro var value at analysis time [#467](https://github.com/babashka/sci/issues/467)
- Excluded clojure var still gets resolved to in syntax quote [#466](https://github.com/babashka/sci/issues/466)

## v0.1.0 (2020-06-16)

Thank to [@jeroenvandijk](https://github.com/jeroenvandijk), [@jjttjj](https://github.com/jjttjj), [@justone](https://github.com/justone), [@sogaiu](https://github.com/sogaiu) and [@armincerf](https://github.com/armincerf) for
contributing.

### New

- Implement hierarchies (`derive` etc.) [#237](https://github.com/babashka/sci/issues/237)
- Implement multimethods [#236](https://github.com/babashka/sci/issues/236)
- Add `ns-interns`, `ns-imports`, `ns-refers`, `ns-map`, `all-ns`
- Add `do-template`
- Add `clojure.edn` namespace
- Add `promise` and `deliver` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Add `:readers` option to support data readers ([@jjttjj](https://github.com/jjttjj))
- Add `tagged-literal`
- Add `when-some` and `if-some` ([@justone](https://github.com/justone))
- Add `re-matcher`
- Add `re-groups` ([@sogaiu](https://github.com/sogaiu))
- Implement `read-string` + `eval` [#285](https://github.com/babashka/sci/issues/285)
- Add `ns-unmap` ([@sogaiu](https://github.com/sogaiu))
- Support `*print-length*` [#294](https://github.com/babashka/sci/issues/294)
- Add `while` macro [#296](https://github.com/babashka/sci/issues/296)
- Add `clojure.repl/find-doc` [#304](https://github.com/babashka/sci/issues/304)
- Add `clojure.repl/apropos` [#317](https://github.com/babashka/sci/issues/317)
- Add `memoize`
- Add `load-string` [#307](https://github.com/babashka/sci/issues/307)
- Add `clojure.repl/pst`
- Add `with-bindings` macro [#289](https://github.com/babashka/sci/issues/289)
- Add `ns-resolve`
- Add `clojure.core/read` [#317](https://github.com/babashka/sci/issues/317)
- Add `remove-ns` [#318](https://github.com/babashka/sci/issues/318)
- Add `requiring-resolve` [#316](https://github.com/babashka/sci/issues/316)
- Add `tagged-literal?` function ([@armincerf](https://github.com/armincerf))
- Support `with-redefs` [#325](https://github.com/babashka/sci/issues/325)
- New `create-ns`, `new-macro-var`, `copy-var`, `init` and `eval-string*` API functions
- Add `enumeration-seq`
- Add `bean`
- Support GraalVM java11 [#332](https://github.com/babashka/sci/issues/332)
- Support `*print-meta*` [#334](https://github.com/babashka/sci/issues/334)
- Support `clojure.core/intern` [#336](https://github.com/babashka/sci/issues/336)
- Defprotocol and defrecord support [#279](https://github.com/babashka/sci/issues/279), [#319](https://github.com/babashka/sci/issues/319)
- Add `double-array` and `short-array`
- Add support for `*print-level*`

## Enhancements / fixes

- Elide metadata from function results (this makes calling evaluated functions
  from JavaScript easier) [#259](https://github.com/babashka/sci/issues/259)
- Eval metadata on vars (e.g. `(def ^{:test (fn [] \"foo\")} x)`).
- Syntax check on amount of args for `if` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Support namespace metadata [#269](https://github.com/babashka/sci/issues/269)
- `require` can now be used as a function
- `find-ns` should return `nil` for non-existent namespace [#299](https://github.com/babashka/sci/issues/299)
- Mark `dotimes` as termination-safe [#298](https://github.com/babashka/sci/issues/298)
- Fix metadata on syntax-quoted values [#301](https://github.com/babashka/sci/issues/301)
- Add support for `:refer :all` in namespace form [#297](https://github.com/babashka/sci/issues/297)
- Support `:rename` in `:require` [#303](https://github.com/babashka/sci/issues/303)
- Add support for `use` [#302](https://github.com/babashka/sci/issues/302)
- `resolve` can now be used a function
- `loop` bindings can refer to previous ones
- JS interop improvements [#312](https://github.com/babashka/sci/issues/312)
- Fix handling `atom` with metadata [#314](https://github.com/babashka/sci/issues/314)
- Fix unqualified binding of `when` and `nth` in `for` macro
- More JS interop improvements ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Fix for variadic recur [#321](https://github.com/babashka/sci/issues/321)
- Fix for associative destructuring ([commit](https://github.com/babashka/sci/commit/438ec15798f319f232d789b74b04ac25f15d540b))
- Add syntax check for `ns` macro: first arg is required and should be symbol
- Fix dynamic binding for functions
- Fix parser line numbers when using shebang
- Remove Java API, don't include AOT-ed sources in release
- Preserve location information in error when `NullPointerException` occurs
- Support alternative field access syntax `(. Integer -SIZE)` [#339](https://github.com/babashka/sci/issues/339)
- Check syntax of `def` and report too many arguments [#340](https://github.com/babashka/sci/issues/340)
- Fix alternative field access syntax `(Integer/SIZE)`
- Fix resolving var from other namespace via refer

## Prior to v0.1.0

Details about releases prior to v0.1.0 can be found
[here](https://github.com/babashka/sci/releases).

## Breaking changes

### 0.2.4

-  Do not merge ex-data into sci error [#534](https://github.com/babashka/sci/issues/534)

### >= 0.1.0

- Removed `:realize-max` and `:preset :termination-safe`. In the light of
  [#348](https://github.com/babashka/sci/issues/348) it would be misleading to
  claim that sci can guarantee termination within reasonable time.

### v0.0.12

- `:row` and `:col` metadata have been renamed to `:line` and `:column` to be
  more compatible with Clojure.

### v0.0.11

- macros provided via options (functions marked with `:sci/macro` metadata) now
  have two additional arguments at the start: `&env` and `&form`.
