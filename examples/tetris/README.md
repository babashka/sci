# Scripted Tetris on Flutter via SCI + ClojureDart

Tetris whose game logic is a **SCI script** (`game.cljc`, pure Clojure). Flutter
provides the loop, input and rendering; SCI runs the game on Dart AOT.

## Files

- `game.cljc` - the game: state atom + `move!` / `rotate!` / `tick!` / `drop!` /
  `board-data`. Pure Clojure, no host interop.
- `app.cljd` - the Flutter UI shell. Loads `game.cljc` from a bundled asset,
  ticks a timer, renders `(board-data)` to a grid of `Container`s.
- `verify.cljd` - a CLI harness that runs `game.cljc` under cljd's SCI and prints
  an ASCII board.

## Status

- **Proven** (native AOT binary, no Flutter needed):
  - game logic correct (also checked with SCI on the JVM),
  - the script runs under cljd's SCI runtime:
    `clojure -M:cljd compile tetris.verify` then `dart compile exe` and run.
- **Untested**: `app.cljd`, the Flutter UI - the build env here has no Flutter
  SDK. The widget code is standard `cljd.flutter` + material but may need small
  API fixes on first run. The load-bearing parts (game + SCI-on-Dart) are done.

## Install Flutter (macOS)

```bash
# option A: homebrew
brew install --cask flutter

# option B: git (pin stable)
git clone https://github.com/flutter/flutter.git -b stable ~/flutter
export PATH="$PATH:$HOME/flutter/bin"

flutter doctor        # follow its setup steps (Xcode / a simulator)
```

## Run

1. Point the project at the Flutter app. In `deps.edn`, set:

   ```clojure
   :cljd/opts {:kind :flutter :main tetris.app}
   ```

   (the repo default is the `sci_repl` CLI; change it back afterward).

2. Make the game script a bundled asset:

   ```bash
   mkdir -p assets
   cp examples/tetris/game.cljc assets/game.cljc
   ```

   and in `pubspec.yaml`, under `flutter:`, add:

   ```yaml
   flutter:
     assets:
       - assets/game.cljc
   ```

3. Launch on a simulator or device (hot reload):

   ```bash
   flutter emulators --launch <id>     # or open a simulator
   clojure -M:cljd flutter
   ```

## Release build + obfuscation check

To confirm the type-identity interop survives AOT obfuscation (the reason the
cljd interop keys on the `:class` Type object, not its name):

```bash
flutter build apk --obfuscate --split-debug-info=build/debug
# or: flutter build ios --obfuscate --split-debug-info=build/debug
```

The game uses no host interop, so obfuscation cannot affect it; this matters
once the app calls Dart/Flutter members through `:classes` overrides.

## How it fits together

Flutter holds one `sci/init` context, evals `game.cljc` into it once, then calls
`(tick!)` / `(move! ..)` / `(rotate!)` / `(drop!)` and re-reads `(board-data)` to
rebuild the grid. The game state lives inside the SCI context (the `game.cljc`
atom), so it persists across calls. Swapping the stand-in rendering for richer
widgets, or moving more logic into the script, is additive.
