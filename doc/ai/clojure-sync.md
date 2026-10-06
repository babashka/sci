# Clojure 1.13 sync

Last synced clojure/clojure commit: `98d735fab02f337cee654cb0629bddc09883a75a` (2026-10-06, after 1.13.0-alpha8).

Ported from `src/clj/clojure/core.clj`:

- `destructure` map and vector parts: `src/sci/impl/destructure.cljc`
- `req!`, `some-vals`, `selector`: `src/sci/impl/namespaces.cljc`

Not ported: new core functions unrelated to destructuring (`merge-deep`, `merge-deep-with`, `tap->`, `clojure.walk/transform-keys`) and JVM-only changes (transients, inlined `meta`, `:redef`).

Kept deviation: `:or` defaults are hoisted only with `:select` or `:all`. Upstream hoists all of them since 208443ae, so a default cannot refer to a sibling binding. Test: `patch-or-default-refers-to-sibling-binding-test`.

To catch up:

```bash
cd ~/dev/clojure && git fetch origin
git log --oneline 98d735fa..origin/master -- src/clj/clojure/core.clj
git -c diff.external= diff --no-ext-diff 98d735fa origin/master -- src/clj/clojure/core.clj
```

Port the `destmap*`, `destvec*`, `some-vals`, `req!` and `selector` hunks, run upstream's destructuring deftests from `test/clojure/test_clojure/data_structures.clj` against bb with this sci, then update the commit above.
