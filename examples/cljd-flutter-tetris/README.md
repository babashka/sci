# Scripted Tetris on Flutter via SCI + ClojureDart

A Flutter Tetris whose game logic is a **SCI script** (`assets/game.cljc`, pure
Clojure). Flutter provides the loop, input and rendering; SCI runs the game on
Dart AOT. A self-contained ClojureDart project depending on SCI via
`:local/root "../.."`.

## Layout

- `assets/game.cljc` - the game: state atom + `move!` / `rotate!` / `tick!` /
  `drop!` / `board-data`. Pure Clojure, no host interop. Bundled as an asset.
- `src/tetris/app.cljd` - the Flutter UI. Loads `game.cljc`, ticks a timer,
  renders `(board-data)` to a grid of `Container`s, wires the buttons to the
  script.

## Run

The game script needs no interop, but Flutter itself does - so this shows SCI
driving real Flutter widgets on Dart AOT.

First run generates the Flutter scaffolding and wires SCI into the Dart project:

```bash
clojure -M:cljd init
```

Web (no Xcode needed):

```bash
clojure -M:cljd compile tetris.app
flutter run -d chrome
```

macOS desktop (needs full Xcode, not just Command Line Tools):

```bash
sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
sudo xcodebuild -runFirstLaunch
clojure -M:cljd compile tetris.app
flutter run -d macos
```

Live hot-reload: `clojure -M:cljd flutter -d macos` (or `-d chrome`).

First run generates the Flutter scaffolding (`macos/`, `web/`, ...) and Dart
output (`lib/cljd-out/`); all of it is gitignored.

## How it fits together

Flutter holds one `sci/init` context, evals `game.cljc` into it once, then calls
`(tick!)` / `(move! ..)` / `(rotate!)` / `(drop!)` and re-reads `(board-data)` to
rebuild the grid. The game state lives inside the SCI context (the `game.cljc`
atom), so it persists across calls.
