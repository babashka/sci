# Async/Await

SCI supports `async`/`await` syntax in ClojureScript for working with JavaScript Promises.

Mark a function with `^:async` metadata and use `await` inside the body:

``` clojure
(defn ^:async fetch-data []
  (let [response (await (js/fetch "https://api.example.com/data"))
        json (await (.json response))]
    (js->clj json :keywordize-keys true)))
```

Async functions always return a JavaScript Promise. To get the value, use `await` from another async function:

``` clojure
(defn ^:async main []
  (let [data (await (fetch-data))]
    (println "Got:" data)))
```

## Supported forms

`await` works inside all standard Clojure forms:

``` clojure
(defn ^:async example []
  ;; let bindings
  (let [x (await (js/Promise.resolve 1))
        y (await (js/Promise.resolve 2))]

    ;; if/when/cond
    (if (await (js/Promise.resolve true))
      :yes :no)

    ;; loop/recur
    (loop [i 0 acc []]
      (if (< i 3)
        (recur (inc i) (conj acc (await (js/Promise.resolve i))))
        acc))

    ;; try/catch/finally
    (try
      (await (js/Promise.reject (ex-info "oops" {:val 1})))
      (catch :default e
        (ex-data e)))  ;; => {:val 1}

    ;; threading macros
    (-> (js/Promise.resolve 10)
        await
        inc)

    ;; collection literals
    [(await (js/Promise.resolve 1))
     (await (js/Promise.resolve 2))]

    ;; doseq
    (doseq [url urls]
      (await (fetch url)))))
```

## Anonymous async functions

Use `^:async` metadata on `fn`:

``` clojure
(defn ^:async example []
  (let [add (^:async fn [a b]
              (+ (await (js/Promise.resolve a))
                 (await (js/Promise.resolve b))))]
    (await (add 1 2))))
```

## User-defined macros

Macros are expanded before `await` detection, so user-defined macros that expand to `await` or `recur` work automatically:

``` clojure
(defmacro my-await [x]
  (list 'await x))

(defn ^:async example []
  (my-await (js/Promise.resolve 42))) ;; => #<Promise 42>
```

## Setup

Enable JS interop in the SCI context:

``` clojure
(require '[sci.core :as sci])

(def ctx (sci/init {:classes {'js js/globalThis :allow :all}}))

(sci/eval-string* ctx "
  (defn ^:async greet [name]
    (let [response (await (js/Promise.resolve (str \"Hello, \" name \"!\")))]
      response))
  (greet \"world\")")
;; => #<Promise "Hello, world!">
```
