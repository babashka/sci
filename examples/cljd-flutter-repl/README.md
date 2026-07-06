# SCI Flutter REPL

Minimal ClojureDart + Flutter example with an in-app SCI REPL. Widget results
render in the app; other values are printed. The example depends on SCI via
`:local/root "../.."`.

## Run

```bash
clojure -M:cljd init
clojure -M:cljd compile repl.app
flutter run -d chrome
```

Hot reload:

```bash
clojure -M:cljd flutter -d chrome
```

## Examples

```clojure
(+ 1 2 3)
(.upper "hello")

(ns app (:require ["package:flutter/material.dart" :as m]
                  [cljd.flutter :as f]))
(m/Text "hello from SCI")
(m/Column .children [(m/Text "a") (m/Text "b")])
(m/Container .color m/Colors.blue .child (m/Text "boxed"))
```

Generated scaffolding (`macos/`, `lib/cljd-out/`, ...) is gitignored.
