# Changelog

For a list of breaking changes, check [here](#breaking-changes)

## Unreleased

### Enhancements / fixes

- Fix error reporting in case of arity error [#518](https://github.com/babashka/babashka/issues/518)
- Shadowing record field names in protocol functions [#513](https://github.com/babashka/babashka/issues/513)
- Fix destructuring in protocol method for record [#512](https://github.com/babashka/babashka/issues/512)
- Faster processing of maps, sets and vectors [#482](https://github.com/babashka/babashka/issues/482)
- Prioritize current namespace vars in syntax quote [#509](https://github.com/babashka/babashka/issues/509)
- Fix ns-publics to not include refers [#520](https://github.com/borkdude/sci/issues/520)
- Add `refer-clojure` macro [#519](https://github.com/borkdude/sci/issues/519)
- Syntax quote resolves referred var incorrectly [#526](https://github.com/borkdude/sci/issues/526)

## v0.2.1

### Enhancements / fixes

- Improvements for using `type` on `defrecord` [#492](https://github.com/borkdude/sci/issues/492)
- Deref vars at analysis time that have `:inline` metadata in Clojure [#483](https://github.com/borkdude/sci/issues/483)
- Keep only location metadata for seqs and symbols [#488](https://github.com/babashka/babashka/issues/488)
- Conditionally defined vars should not have metadata [#496](https://github.com/babashka/babashka/issues/496)
- Fix interop on map [#506](https://github.com/babashka/babashka/issues/506)
- Performance improvements [#500](https://github.com/babashka/babashka/issues/500), [#502](https://github.com/babashka/babashka/issues/502), [#504](https://github.com/babashka/babashka/issues/504)
- Fix shadow-cljs warnings [#499](https://github.com/babashka/babashka/issues/499)

## v0.2.0

Thanks for contributing to this release:

[@lread](https://github.com/lread), [@patrick-galvin](https://github.com/patrick-galvin), [@SevereOverfl0w](https://github.com/SevereOverfl0w), [@djblue](https://github.com/djblue), [@kwrooijen](https://github.com/kwrooijen), [@sogaiu](https://github.com/sogaiu), [@joinr](https://github.com/joinr), [@RickMoynihan](https://github.com/RickMoynihan), [@galdober](https://github.com/galdober)

### Breaking changes

- Removed `:realize-max` and `:preset :termination-safe`. In the light of
  [#348](https://github.com/borkdude/sci/issues/348) it would be misleading to
  claim that sci can guarantee termination within reasonable time.

### New

- Add `class?`, `iterator-seq`, `remove-watch`, `realized?`, `clojure.walk/macroexpand-all`, `find-var`, `lazy-cat`, `bound?`, `*print-namespace-maps*`, `get-thread-bindings`, `var-get`, `var-set`, `with-local-vars`
- Add `fork` API function [#369](https://github.com/babashka/babashka/issues/369)
- Add API functions for parsing code and evaluating forms: `sci.core/reader`,
  `sci.core/parse-string`, `sci.core/parse-next`, `sci.core/eval-form` [#404](https://github.com/babashka/babashka/issues/404)
- Support implementing `IDeref`, `IAtom`, `IAtom2` (and CLJS equivalents) [#401](https://github.com/babashka/babashka/issues/401)
- Add API vars `print-meta`, `print-level` which can be used with
  `sci.core/binding` to control the dynamic var equivalent in sci programs
- Support calling `symbol` on a var [#453](https://github.com/babashka/babashka/issues/453)
- `:disable-arity-checks` option: when used, sci behaves similarly to CLJS/JS by
  not checking the provided number of arguments and allowing less or more for
  single and fixed arity functions
  [#460](https://github.com/babashka/babashka/issues/460)

### Enhancements / fixes

- Alter-var-root uses thread-bound value to update [#359](https://github.com/babashka/babashka/issues/359)
- Eval metadata on var created with defn [#371](https://github.com/babashka/babashka/issues/371)
- Metadata fn on var f fails if referring to f [#363](https://github.com/babashka/babashka/issues/363)
- Fix missing protocol methods [#367](https://github.com/babashka/babashka/issues/367) ([@patrick-galvin](https://github.com/patrick-galvin))
- Support multiple methods of protocol on defrecord
- Allow re-binding of core vars with clojure.core/with-redefs [#375](https://github.com/babashka/babashka/issues/375)
- Fix false dynamic binding [#379](https://github.com/babashka/babashka/issues/379)
- Don't eval record returned from reader function [#386](https://github.com/babashka/babashka/issues/386)
- Implement `->` and `as->` as normal macros [#390](https://github.com/babashka/babashka/issues/390), [#462](https://github.com/babashka/babashka/issues/462) ([@kwrooijen](https://github.com/kwrooijen))
- `defn` should not introduce local for name in body [#384](https://github.com/babashka/babashka/issues/384)
- Fix wrong arity in error message when calling macro [#392](https://github.com/babashka/babashka/issues/392)
- Throw when trying to redefine referred var [#398](https://github.com/babashka/babashka/issues/398)
- Fix for `use` [120175f](https://github.com/borkdude/sci/commit/120175f2efc0328e88a832e792db342a70558806)
- Fix importing protocol classes from namespaces with hyphens [#410](https://github.com/babashka/babashka/issues/410)
- Performance enhancements [#415](https://github.com/babashka/babashka/issues/415), [#452](https://github.com/babashka/babashka/issues/452), [#468](https://github.com/babashka/babashka/issues/468), [#470](https://github.com/babashka/babashka/issues/470), [#472](https://github.com/babashka/babashka/issues/472), [#473](https://github.com/babashka/babashka/issues/473), [#475](https://github.com/babashka/babashka/issues/475), [#478](https://github.com/babashka/babashka/issues/478), [#480](https://github.com/babashka/babashka/issues/480)
- Support top-level do emitted from macro [#421](https://github.com/babashka/babashka/issues/421)
- Support map constructor for maps [#431](https://github.com/babashka/babashka/issues/431)
- Partial support for multiple reified classes [323a257](https://github.com/borkdude/sci/commit/323a2574ec4d59a0544a829c1fa529fcbc110140)
- Fix calling literal symbol babashka/babashka#622
- Allow user-defined vars when def is allowed [#434](https://github.com/babashka/babashka/issues/434)
- Fix default destructuring with false [#436](https://github.com/babashka/babashka/issues/436)
- Fix reflection warning in multimethods code [#437](https://github.com/babashka/babashka/issues/437) ([@galdober](https://github.com/galdober))
- Support nested libspecs [#399](https://github.com/babashka/babashka/issues/399)
- Aliases in protocol functions should work [#440](https://github.com/babashka/babashka/issues/440)
- Allow users to override :line metadata [#443](https://github.com/babashka/babashka/issues/443)
- Support second arg (env) in `resolve`
- Preserve and eval reader meta on coll literals and functions [#447](https://github.com/babashka/babashka/issues/447), [#448](https://github.com/babashka/babashka/issues/448)
- Fix #js object reading [#449](https://github.com/babashka/babashka/issues/449)
- Support unmap for imported classes [#432](https://github.com/babashka/babashka/issues/432)
- Fix for reader conditional parsing borkdude/edamame#65
- Dotted field access for JS interop [#450](https://github.com/babashka/babashka/issues/450)
- Syntax checks for binding [#458](https://github.com/babashka/babashka/issues/458)
- Add `boolean?` to constant check [#465](https://github.com/babashka/babashka/issues/465) ([@kwrooijen](https://github.com/kwrooijen))
- Check macro var value at analysis time [#467](https://github.com/babashka/babashka/issues/467)
- Excluded clojure var still gets resolved to in syntax quote [#466](https://github.com/babashka/babashka/issues/466)

## v0.1.0 (2020-06-16)

Thank to [@jeroenvandijk](https://github.com/jeroenvandijk), [@jjttjj](https://github.com/jjttjj), [@justone](https://github.com/justone), [@sogaiu](https://github.com/sogaiu) and [@armincerf](https://github.com/armincerf) for
contributing.

### New

- Implement hierarchies (`derive` etc.) [#237](https://github.com/babashka/babashka/issues/237)
- Implement multimethods [#236](https://github.com/babashka/babashka/issues/236)
- Add `ns-interns`, `ns-imports`, `ns-refers`, `ns-map`, `all-ns`
- Add `do-template`
- Add `clojure.edn` namespace
- Add `promise` and `deliver` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Add `:readers` option to support data readers ([@jjttjj](https://github.com/jjttjj))
- Add `tagged-literal`
- Add `when-some` and `if-some` ([@justone](https://github.com/justone))
- Add `re-matcher`
- Add `re-groups` ([@sogaiu](https://github.com/sogaiu))
- Implement `read-string` + `eval` [#285](https://github.com/babashka/babashka/issues/285)
- Add `ns-unmap` ([@sogaiu](https://github.com/sogaiu))
- Support `*print-length*` [#294](https://github.com/babashka/babashka/issues/294)
- Add `while` macro [#296](https://github.com/babashka/babashka/issues/296)
- Add `clojure.repl/find-doc` [#304](https://github.com/babashka/babashka/issues/304)
- Add `clojure.repl/apropos` [#317](https://github.com/babashka/babashka/issues/317)
- Add `memoize`
- Add `load-string` [#307](https://github.com/babashka/babashka/issues/307)
- Add `clojure.repl/pst`
- Add `with-bindings` macro [#289](https://github.com/babashka/babashka/issues/289)
- Add `ns-resolve`
- Add `clojure.core/read` [#317](https://github.com/babashka/babashka/issues/317)
- Add `remove-ns` [#318](https://github.com/babashka/babashka/issues/318)
- Add `requiring-resolve` [#316](https://github.com/babashka/babashka/issues/316)
- Add `tagged-literal?` function ([@armincerf](https://github.com/armincerf))
- Support `with-redefs` [#325](https://github.com/babashka/babashka/issues/325)
- New `create-ns`, `new-macro-var`, `copy-var`, `init` and `eval-string*` API functions
- Add `enumeration-seq`
- Add `bean`
- Support GraalVM java11 [#332](https://github.com/babashka/babashka/issues/332)
- Support `*print-meta*` [#334](https://github.com/babashka/babashka/issues/334)
- Support `clojure.core/intern` [#336](https://github.com/babashka/babashka/issues/336)
- Defprotocol and defrecord support [#279](https://github.com/babashka/babashka/issues/279), [#319](https://github.com/babashka/babashka/issues/319)
- Add `double-array` and `short-array`
- Add support for `*print-level*`

## Enhancements / fixes

- Elide metadata from function results (this makes calling evaluated functions
  from JavaScript easier) [#259](https://github.com/babashka/babashka/issues/259)
- Eval metadata on vars (e.g. `(def ^{:test (fn [] \"foo\")} x)`).
- Syntax check on amount of args for `if` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Support namespace metadata [#269](https://github.com/babashka/babashka/issues/269)
- `require` can now be used as a function
- `find-ns` should return `nil` for non-existent namespace [#299](https://github.com/babashka/babashka/issues/299)
- Mark `dotimes` as termination-safe [#298](https://github.com/babashka/babashka/issues/298)
- Fix metadata on syntax-quoted values [#301](https://github.com/babashka/babashka/issues/301)
- Add support for `:refer :all` in namespace form [#297](https://github.com/babashka/babashka/issues/297)
- Support `:rename` in `:require` [#303](https://github.com/babashka/babashka/issues/303)
- Add support for `use` [#302](https://github.com/babashka/babashka/issues/302)
- `resolve` can now be used a function
- `loop` bindings can refer to previous ones
- JS interop improvements [#312](https://github.com/babashka/babashka/issues/312)
- Fix handling `atom` with metadata [#314](https://github.com/babashka/babashka/issues/314)
- Fix unqualified binding of `when` and `nth` in `for` macro
- More JS interop improvements ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Fix for variadic recur [#321](https://github.com/babashka/babashka/issues/321)
- Fix for associative destructuring ([commit](https://github.com/borkdude/sci/commit/438ec15798f319f232d789b74b04ac25f15d540b))
- Add syntax check for `ns` macro: first arg is required and should be symbol
- Fix dynamic binding for functions
- Fix parser line numbers when using shebang
- Remove Java API, don't include AOT-ed sources in release
- Preserve location information in error when `NullPointerException` occurs
- Support alternative field access syntax `(. Integer -SIZE)` [#339](https://github.com/babashka/babashka/issues/339)
- Check syntax of `def` and report too many arguments [#340](https://github.com/babashka/babashka/issues/340)
- Fix alternative field access syntax `(Integer/SIZE)`
- Fix resolving var from other namespace via refer

## Prior to v0.1.0

Details about releases prior to v0.1.0 can be found
[here](https://github.com/borkdude/sci/releases).

## Breaking changes

### >= 0.1.0

- Removed `:realize-max` and `:preset :termination-safe`. In the light of
  [#348](https://github.com/borkdude/sci/issues/348) it would be misleading to
  claim that sci can guarantee termination within reasonable time.

### v0.0.12

- `:row` and `:col` metadata have been renamed to `:line` and `:column` to be
  more compatible with Clojure.

### v0.0.11

- macros provided via options (functions marked with `:sci/macro` metadata) now
  have two additional arguments at the start: `&env` and `&form`.
