(ns sci.core
  (:require
   [sci.impl.interpreter :as i]))

(defn eval-string
  "Evaluates string `s` as one or multiple Clojure expressions using the Small Clojure Interpreter.

  The map `opts` may contain the following:

  - `:bindings`: a map with bindings that are used to resolve symbols
  in the Clojure form, e.g. `{'x 1}`.

  - `:namespaces`: a map of symbols to namespaces, where a namespace
  is a map with symbols to values, e.g.: `{'foo.bar {'x 1}}`. These
  namespaces can be used with `require`.

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:bindings` or `:namespaces` have to be explicitly
  enumerated.`

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:realize-max`: integer, when provided, program may realize a
  maximum number of elements from sequences, e.g. `(vec (range))` will
  throw for any number. This also applies to sequences return from the
  expression to the caller.

  - `:preset`: currently only `:termination-safe` is supported, which
  will set `:realize-max` to `100` and disallows the symbols `loop`,
  `recur` and `trampoline`."
  ([s] (eval-string s nil))
  ([s opts]
   (i/eval-string s opts)))

;;;; Scratch

(comment
  (eval-string "(inc x)" {:bindings {'x 2}})
  )
