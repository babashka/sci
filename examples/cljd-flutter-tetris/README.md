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

`app.cljd` exposes Flutter to SCI as a `flutter` namespace of builder fns
(`text`, `cell`, `row`, `column`, `gap`, `button`) that take Clojure data and
return real Flutter widgets. It inits one `sci` context with that namespace,
evals `game.cljc`, then on every tick evals `(render)` and displays the widget
it returns.

So `game.cljc` builds the **entire** UI itself - `(render)` constructs the score
text, the board grid (colored `flutter/cell`s in `flutter/row`s/`column`s) and
the control buttons, whose `on-press` callbacks are SCI fns that call
`(move! ..)` / `(rotate!)` / `(drop!)`. The game state lives in the `game.cljc`
atom inside the SCI context. Flutter is driven from the script.
