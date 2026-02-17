# TODO

## Method resolution doesn't walk class hierarchy for interface methods

`.meta` on a value type-hinted as `^clojure.lang.APersistentMap` fails with `NoSuchFieldException: meta`, even though APersistentMap implements `IMeta`. Workaround: hint with the interface directly, e.g. `(.meta ^clojure.lang.IMeta this)`.

Repro in bb:

```clojure
(let [m (proxy [clojure.lang.APersistentMap clojure.lang.IMeta clojure.lang.IObj] []
          (iterator [] (.iterator {}))
          (containsKey [k] false)
          (entryAt [k] nil)
          (valAt ([k] nil) ([k d] d))
          (count [] 0)
          (assoc [k v] nil)
          (without [k] nil)
          (seq [] nil)
          (meta [] {:works true})
          (withMeta [md] nil)
          (toString [] (str (.meta this))))]
  (.toString m))
```
