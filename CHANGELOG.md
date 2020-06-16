# Changelog

For a list of breaking changes, check [here](#breaking-changes)

## v0.1.0 (2020-06-16)

Thank to [@jeroenvandijk](https://github.com/jeroenvandijk), [@jjttjj](https://github.com/jjttjj), [@justone](https://github.com/justone), [@sogaiu](https://github.com/sogaiu) and [@armincerf](https://github.com/armincerf) for
contributing.

### New

- Implement hierarchies (`derive` etc.) [#237](https://github.com/borkdude/babashka/issues/237)
- Implement multimethods [#236](https://github.com/borkdude/babashka/issues/236)
- Add `ns-interns`, `ns-imports`, `ns-refers`, `ns-map`, `all-ns`
- Add `do-template`
- Add `clojure.edn` namespace
- Add `promise` and `deliver` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Add `:readers` option to support data readers ([@jjttjj](https://github.com/jjttjj))
- Add `tagged-literal`
- Add `when-some` and `if-some` ([@justone](https://github.com/justone))
- Add `re-matcher`
- Add `re-groups` ([@sogaiu](https://github.com/sogaiu))
- Implement `read-string` + `eval` [#285](https://github.com/borkdude/babashka/issues/285)
- Add `ns-unmap` ([@sogaiu](https://github.com/sogaiu))
- Support `*print-length*` [#294](https://github.com/borkdude/babashka/issues/294)
- Add `while` macro [#296](https://github.com/borkdude/babashka/issues/296)
- Add `clojure.repl/find-doc` [#304](https://github.com/borkdude/babashka/issues/304)
- Add `clojure.repl/apropos` [#317](https://github.com/borkdude/babashka/issues/317)
- Add `memoize`
- Add `load-string` [#307](https://github.com/borkdude/babashka/issues/307)
- Add `clojure.repl/pst`
- Add `with-bindings` macro [#289](https://github.com/borkdude/babashka/issues/289)
- Add `ns-resolve`
- Add `clojure.core/read` [#317](https://github.com/borkdude/babashka/issues/317)
- Add `remove-ns` [#318](https://github.com/borkdude/babashka/issues/318)
- Add `requiring-resolve` [#316](https://github.com/borkdude/babashka/issues/316)
- Add `tagged-literal?` function ([@armincerf](https://github.com/armincerf))
- Support `with-redefs` [#325](https://github.com/borkdude/babashka/issues/325)
- New `create-ns`, `new-macro-var`, `copy-var`, `init` and `eval-string*` API functions
- Add `enumeration-seq`
- Add `bean`
- Support GraalVM java11 [#332](https://github.com/borkdude/babashka/issues/332)
- Support `*print-meta*` [#334](https://github.com/borkdude/babashka/issues/334)
- Support `clojure.core/intern` [#336](https://github.com/borkdude/babashka/issues/336)
- Defprotocol and defrecord support [#279](https://github.com/borkdude/babashka/issues/279), [#319](https://github.com/borkdude/babashka/issues/319)
- Add `double-array` and `short-array`
- Add support for `*print-level*`

## Enhancements / fixes

- Elide metadata from function results (this makes calling evaluated functions
  from JavaScript easier) [#259](https://github.com/borkdude/babashka/issues/259)
- Eval metadata on vars (e.g. `(def ^{:test (fn [] \"foo\")} x)`).
- Syntax check on amount of args for `if` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Support namespace metadata [#269](https://github.com/borkdude/babashka/issues/269)
- `require` can now be used as a function
- `find-ns` should return `nil` for non-existent namespace [#299](https://github.com/borkdude/babashka/issues/299)
- Mark `dotimes` as termination-safe [#298](https://github.com/borkdude/babashka/issues/298)
- Fix metadata on syntax-quoted values [#301](https://github.com/borkdude/babashka/issues/301)
- Add support for `:refer :all` in namespace form [#297](https://github.com/borkdude/babashka/issues/297)
- Support `:rename` in `:require` [#303](https://github.com/borkdude/babashka/issues/303)
- Add support for `use` [#302](https://github.com/borkdude/babashka/issues/302)
- `resolve` can now be used a function
- `loop` bindings can refer to previous ones
- JS interop improvements [#312](https://github.com/borkdude/babashka/issues/312)
- Fix handling `atom` with metadata [#314](https://github.com/borkdude/babashka/issues/314)
- Fix unqualified binding of `when` and `nth` in `for` macro
- More JS interop improvements ([@jeroenvandijk](https://github.com/jeroenvandijk))
- Fix for variadic recur [#321](https://github.com/borkdude/babashka/issues/321)
- Fix for associative destructuring ([commit](https://github.com/borkdude/sci/commit/438ec15798f319f232d789b74b04ac25f15d540b))
- Add syntax check for `ns` macro: first arg is required and should be symbol
- Fix dynamic binding for functions
- Fix parser line numbers when using shebang
- Remove Java API, don't include AOT-ed sources in release
- Preserve location information in error when `NullPointerException` occurs
- Support alternative field access syntax `(. Integer -SIZE)` [#339](https://github.com/borkdude/babashka/issues/339)
- Check syntax of `def` and report too many arguments [#340](https://github.com/borkdude/babashka/issues/340)
- Fix alternative field access syntax `(Integer/SIZE)`
- Fix resolving var from other namespace via refer

## Prior to v0.1.0

Details about releases prior to v0.1.0 can be found
[here](https://github.com/borkdude/sci/releases).

## Breaking changes

### v0.0.12

- `:row` and `:col` metadata have been renamed to `:line` and `:column` to be
  more compatible with Clojure.

### v0.0.11

- macros provided via options (functions marked with `:sci/macro` metadata) now
  have two additional arguments at the start: `&env` and `&form`.
