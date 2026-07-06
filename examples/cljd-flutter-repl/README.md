# SCI Flutter REPL

A reference ClojureDart + Flutter app: an in-app REPL over a SCI context that has
**Flutter access and host interop pre-wired**. Type Clojure, it evals against the
context; a widget result renders, any other value is printed. Depends on SCI via
`:local/root "../.."`.

## The point

`make-ctx` in `src/repl/app.cljd` is the part to copy into your own app. It shows
the two ways to expose the host to interpreted Clojure on Dart, where there is
**no runtime reflection**:

- **Flutter widgets** as a `flutter` namespace of builder fns (`:namespaces`):
  `(flutter/text "hi")`, `(flutter/column [...])`, `(flutter/button "tap" f)`.
- **Host classes** via override fns (`:classes`): each member is a compiled fn
  that calls the real Dart method or field, e.g. `String` `upper`/`lower`/`length`.

Both are compiled call sites, so they survive AOT tree-shaking - unlike
reflection, which Dart AOT does not have.

## Try these in the REPL

```clojure
(+ 1 2 3)
(.upper "hello")
(flutter/text "hello from SCI")
(flutter/column [(flutter/text "a") (flutter/text "b")])
(flutter/container red (flutter/text "boxed"))
```

## Run

```bash
clojure -M:cljd init          # first time: scaffolding + wire SCI
clojure -M:cljd compile repl.app
flutter run -d chrome         # or -d macos with full Xcode installed
```

Live hot-reload: `clojure -M:cljd flutter -d chrome`.

Generated scaffolding (`macos/`, `lib/cljd-out/`, ...) is gitignored.
