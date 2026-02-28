# deftype/defrecord: no var — Review Checklist

## Core Change

deftype/defrecord no longer creates a `(def Foo ...)` var. Type names are stored in a separate `:types` key in the namespace map, matching Clojure where deftype/defrecord creates a class mapping, not a var.

## Files to Review

### 1. `src/sci/lang.cljc`
- Simplified `Type` to single `data` field (removed `namespace`/`name` fields)
- Removed `Named`/`INamed` — `.getName` conflicts with Class behavior
- Added `IReference` (`alterMeta`/`resetMeta`) for `alter-meta!` (print-method registration)
- Added `HasName` implementation: `getName` returns `(str this)` (fully qualified, like `Class.getName()`)
- Removed unused `class-name`/`package-name` helpers

### 2. `src/sci/impl/utils.cljc`
- Added `init-type!` — registers placeholder Type in `:types` at analysis time
- Enables forward references without creating a var

### 3. `src/sci/impl/deftype.cljc`
- `emit-deftype`: removed `(def ~record-name ...)` and `:sci.impl/var`
- `deftype-macro`: emits `(do (defn ->Foo [fields]) (deftype* ...) (import ...))` instead of bare `(deftype* ...)`
  - `defn` makes constructors visible to static analysis (Clerk)
  - `import` auto-registers type in namespace
- `analyze-deftype*`: calls `init-type!`
- Factory fn passes `~record-name` directly instead of `(var ~record-name)`
- `to-string`: uses `(str t)` instead of `(namespace t) "." (name t)`

### 4. `src/sci/impl/records.cljc`
- `defrecord`: removed `(def ~record-name ...)` and `:sci.impl/var`; calls `init-type!`; removed `(declare ~record-name)`
- Constructor passes `~record-name` instead of `(var ~record-name)`
- `resolve-record-or-protocol-class`: looks up `(:types ns)` first, then namespace map
- `to-string`: uses `(str t)` instead of `(namespace t) "." (name t)`

### 5. `src/sci/impl/namespaces.cljc`
- `-create-type`: registers type in `:types` and removes leftover var from `(declare)`
- Removed dead code (`-reg-key!`, comments)
- `class?`: extended for `sci.lang.Type`
- `alter-meta!` (CLJS): handles `sci.lang.Type`

### 6. `src/sci/impl/evaluator.cljc`
- `eval-instance-method-invocation`: fixed tag-class fallback when tag doesn't match instance (needed for `(.getName ^Class (resolve 'Foo))`)
- `eval-import`: looks up from `:types`, stores as `:types` (not `:refers`)

### 7. `src/sci/impl/analyzer.cljc`
- Constructor resolution accepts `sci.lang.Type` values (not just vars)

### 8. `src/sci/impl/resolve.cljc`
- Symbol resolution checks `:types` key (after `:refers`, only for non-var lookups)

### 9. `src/sci/impl/parser.cljc`
- `var->sym`: removed `:sci/record` special-case
- `fully-qualify`: checks `:types` for syntax-quote (`` `Foo`` → `user.Foo`)

### 10. `src/sci/impl/multimethods.cljc`
- `defmethod` for `print-method`/`pprint`: uses `v#` directly instead of `(:sci.impl/var m#)`

### 11. `src/sci/impl/protocols.cljc`
- Uses `(var ~type)` directly for `alter-meta!` on print-method

### 12. `src/sci/impl/hierarchies.cljc`
- `->tag`: parses `(str type)` with `.lastIndexOf` instead of using `Named` `namespace`/`name`

### 13. Tests (`test/sci/defrecords_and_deftype_test.cljc`)
- Updated `macroexpand-1` test for new `(do ...)` shape
- Rewritten `deftype-macroexpand-1-produces-deftype*-test` using `tree-seq`
- Updated `deftype*-uses-flat-methods-not-metadata-test` to extract from wrapper
- New: `deftype-resolve-test` (resolve, class?, .getName, .getName with ^Class hint)
- New: `deftype-macroexpand-constructor-visible-test`
- New: `symbol-on-resolved-type-test`
- New: `syntax-quote-type-produces-dotted-form-test`
- New: `use-does-not-bring-constructor-test`
- New: `re-eval-deftype-test`
- New: `constructor-self-reference-test`
- `.getName` tests use `sci/eval-string` directly (not `tu/eval*`) for native compatibility

### 14. Test (`test/sci/core_test.cljc`)
- `macroexpand-test`: checks `(= 'do (first ...))` instead of `clojure.core/defrecord`

## TODOs

- [x] Removed `Named`/`INamed` from `sci.lang.Type`
- [x] Added `HasName/getName` returning fully qualified name (class-like behavior)
- [x] Fixed `->tag` in hierarchies to parse from `(str type)` instead of `Named`
- [x] Fixed CLJS test failures (`.getName` CLJ-only, error message regex)
- [x] Fixed native test failures (use `sci/eval-string` instead of `tu/eval*`)
- [ ] Consider caching the hierarchy tag symbol on Type to avoid string parsing in `->tag` (hot path for multimethod dispatch via `isa?`)

## Review Status

- [x] 1. `sci/lang.cljc` — reviewed: removed Named, added HasName
- [ ] 2. `sci/impl/utils.cljc`
- [ ] 3. `sci/impl/deftype.cljc`
- [ ] 4. `sci/impl/records.cljc`
- [ ] 5. `sci/impl/namespaces.cljc`
- [x] 6. `sci/impl/evaluator.cljc` — reviewed: removed getName special case, tag-class fallback
- [ ] 7. `sci/impl/analyzer.cljc`
- [ ] 8. `sci/impl/resolve.cljc`
- [ ] 9. `sci/impl/parser.cljc`
- [ ] 10. `sci/impl/multimethods.cljc`
- [ ] 11. `sci/impl/protocols.cljc`
- [x] 12. `sci/impl/hierarchies.cljc` — reviewed: ->tag uses string parsing
- [ ] 13. Tests (defrecords_and_deftype_test)
- [ ] 14. Tests (core_test)
