# Bounding execution with `:interrupt-fn`

> [!WARNING]
> This is an experimental feature that may change based on user feedback. Keep an eye on the CHANGELOG for changes.

SCI supports passing a zero arg `:interrupt-fn` to its options, which is called on every `fn`
(and `loop/recur`) body entry. Using this hook you can cancel evaluation after a
certain number of iterations, check `Thread/interrupted`, etc. A demo:

``` clojure
(require '[sci.interrupt :as interrupt])

(defn limit [n]
  (let [counter (atom 0)]
    (fn [] (when (> (swap! counter inc) n)
             (interrupt/interrupt!)))))

(sci/eval-string "(loop [] (recur))" {:interrupt-fn (limit 1000000)})
;;=> throws "Interrupted"
```

This above `limit` function limits the number of iterations. The logic is up to
you. You can make one that limits wall clock time:

``` clojure
(defn time-limit [ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (fn []
      (when (> (System/currentTimeMillis) deadline)
        (interrupt/interrupt!)))))

(sci/eval-string "(loop [] (recur))" {:interrupt-fn (time-limit 1000)})
;;=> throws "Interrupted" after ~1 second
```

or one that polls `Thread/.isInterrupted`:

``` clojure
(defn thread-limit []
  (fn []
    (when (.isInterrupted (Thread/currentThread))
      (interrupt/interrupt!))))

(let [fut (future (sci/eval-string "(loop [] (recur))" {:interrupt-fn (thread-limit)}))]
  (Thread/sleep 1000)
  (future-cancel fut))
;;=> the running evaluation throws "Interrupted"
```

The `interrupt-fn` is executed on every `fn` body entrance, so it's worthwile to optimize performance.

Note that `:interrupt-fn` only fires on interpreted code. Core functions in SCI are mostly called via the host and are not interpreted. E.g. `(doall (range))` would not hit `:interrupt-fn`.
The namespace `sci.interrupt` provides interruptible core function drop-in alternatives. These are currently not loaded by default. You can opt-in on those by using the `clojure-core` configuration and adding it to `:namespaces {'clojure.core ...}`:
Note that the core overrides can introduce performance regressions in your code compared to the standard SCI clojure core functions.

``` clojure
(sci/eval-string "(reduce + (range))"
                 {:interrupt-fn (limit 1000000)
                  :namespaces {'clojure.core interrupt/clojure-core}})
;;=> throws "Interrupted"
```

Host functions that you expose yourself are not automatically made aware of
`:interrupt-fn`, this must be done manually. You can get the `:interrupt-fn`
from a `ctx` using the `sci.interrupt/get-interrupt-fn` accessor.

``` clojure
(require '[sci.ctx-store :as store])

;; A host function that loops
(defn my-host-loop [n]
  (let [interrupt-fn (interrupt/get-interrupt-fn (store/get-ctx))]
    (dotimes [_ n]
      (when interrupt-fn (interrupt-fn))
      ;; ... work ...
      )))

(sci/eval-string "(my-host-loop 1000000)"
                 {:interrupt-fn (limit 500)
                  :namespaces {'user {'my-host-loop my-host-loop}}})
;;=> throws "Interrupted"
```

Note that even with these overrides, unbounded programs are still possible.
For hard guarantees it is best to run untrusted code in a separate process that can be killed.
Here is an example of how your program can run unboundedly even with `:interrupt-fn`.
Since the long-running CPU-heavy call is made in the host, `:interrupt-fn` never hits and your program will hang.

``` clojure
(sci/eval-string "(.pow (biginteger 10) 100000000)"
                 {:interrupt-fn (limit 500)
                  :classes {:allow :all}
                  :namespaces {'clojure.core interrupt/clojure-core}})
```
