# TODO

## Pre-existing issues

- `src/sci/impl/protocols.cljc` lines 215 and 251: `:bindingx` is likely a typo for `:bindings`. This causes `eval-resolve-state` to receive `nil` for bindings in `extend-protocol` and `extend-type`. Currently masked because protocol resolution still works via other fallback paths, but could cause subtle bugs.
