# Changes

## v0.0.12

- `:row` and `:col` metadata have been renamed to `:line` and `:column` to be
  more compatible with Clojure.

## v0.0.11

- macros provided via options (functions marked with `:sci/macro` metadata) now
  have two additional arguments at the start: `&env` and `&form`.
