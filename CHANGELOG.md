# Changelog

For a list of breaking changes, check [here](#breaking-changes)

## v0.1.0 (unreleased)

- implement hierarchies (`derive` etc.) [#237](https://github.com/borkdude/babashka/issues/237)
- implement multimethods [#236](https://github.com/borkdude/babashka/issues/236)
- elide metadata from function results (this makes calling evaluated functions
  from JavaScript easier) [#259](https://github.com/borkdude/babashka/issues/259)
- eval metadata on vars (e.g. `(def ^{:test (fn [] \"foo\")} x)`).
- add `ns-interns`, `ns-imports`, `ns-refers`, `ns-map`, `all-ns`
- add `do-template`
- add `clojure.edn` namespace
- add `promise` and `deliver` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- add `:readers` option to support data readers ([@jjttjj](https://github.com/jjttjj))
- syntax check on amount of args for `if` ([@jeroenvandijk](https://github.com/jeroenvandijk))
- add `tagged-literal`
- support namespace metadata [#269](https://github.com/borkdude/babashka/issues/269)
- add `when-some` and `if-some` ([@justone](https://github.com/justone))
- add `re-matcher`
- add `re-groups` ([@sogaiu](https://github.com/sogaiu))
- implement `read-string` + `eval` [#285](https://github.com/borkdude/babashka/issues/285)
- add `ns-unmap` ([@sogaiu](https://github.com/sogaiu))
- `require` can now be used as a function
- support `*print-length*` [#294](https://github.com/borkdude/babashka/issues/294)
- `find-ns` should return `nil` for non-existent namespace [#299](https://github.com/borkdude/babashka/issues/299)
- mark `dotimes` as termination-safe [#298](https://github.com/borkdude/babashka/issues/298)
- add `while` macro [#296](https://github.com/borkdude/babashka/issues/296)
- fix metadata on syntax-quoted values [#301](https://github.com/borkdude/babashka/issues/301)
- add support for `:refer :all` in namespace form [#297](https://github.com/borkdude/babashka/issues/297)
- support `:rename` in `:require` [#303](https://github.com/borkdude/babashka/issues/303)
- add support for `use` [#302](https://github.com/borkdude/babashka/issues/302)
- add `clojure.repl/find-doc` [#304](https://github.com/borkdude/babashka/issues/304)
- add `clojure.repl/apropos` [#317](https://github.com/borkdude/babashka/issues/317)
- `resolve` can now be used a function
- add `memoize`
- add `load-string` [#307](https://github.com/borkdude/babashka/issues/307)
- `loop` bindings can refer to previous ones
- add `clojure.repl/pst`
- JS interop improvements [#312](https://github.com/borkdude/babashka/issues/312)
- add `with-bindings` macro [#289](https://github.com/borkdude/babashka/issues/289)
- fix handling `atom` with metadata [#314](https://github.com/borkdude/babashka/issues/314)
- add `ns-resolve`
- fix unqualified binding of `when` and `nth` in `for` macro
- add `clojure.core/read` [#317](https://github.com/borkdude/babashka/issues/317)
- add `remove-ns` [#318](https://github.com/borkdude/babashka/issues/318)
- add `requiring-resolve` [#316](https://github.com/borkdude/babashka/issues/316)
- More JS interop improvements ([@jeroenvandijk](https://github.com/jeroenvandijk))
- fix for variadic recur [#321](https://github.com/borkdude/babashka/issues/321)
- fix for associative destructuring ([commit](https://github.com/borkdude/sci/commit/438ec15798f319f232d789b74b04ac25f15d540b))
- add `tagged-literal?` function ([@armincerf](https://github.com/armincerf))
- support `with-redefs` [#325](https://github.com/borkdude/babashka/issues/325)
- add syntax check for `ns` macro: first arg is required and should be symbol
- new `create-ns`, `new-macro-var`, `copy-var`, `init` and `eval-string*` API functions
- fix dynamic binding for functions
- add `enumeration-seq`
- fix parser line numbers when using shebang
- remove Java API, don't include AOT-ed sources in release
- support GraalVM java11 [#332](https://github.com/borkdude/babashka/issues/332)
- add `bean`
- preserve location information in error when `NullPointerException` occurs
- support `*print-meta*` [#334](https://github.com/borkdude/babashka/issues/334)
- support `clojure.core/intern` [#336](https://github.com/borkdude/babashka/issues/336)
- support alternative field access syntax `(. Integer -SIZE)` [#339](https://github.com/borkdude/babashka/issues/339)
- check syntax of `def` and report too many arguments [#340](https://github.com/borkdude/babashka/issues/340)

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
