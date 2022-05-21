# Asynchronous evaluation

The `sci.async` namespace offers async evaluation for ClojureScript. This is
work in progress. Please try it out and provide feedback.

Difference with the synchronous evaluation in `sci.core`:

- `(ns ...)` forms are evaluated asynchronously - loading can be configured
  via the `:async-load-fn` option.
- The return value from evaluation is a JavaScript promise.
- Optionally `require` can be made async using `sci/require`, see [Require](#require).

Code examples below use
[promesa](https://cljdoc.org/d/funcool/promesa/8.0.450/doc/user-guide) to make
working with promises more convenient.

## Lazy loading a namespace

``` clojure
(ns example
  (:require [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(defn async-load-fn
  [{:keys [libname ctx]}]
  (p/resolved
   (case libname
     my.lazy-ns
     (let [mlns (sci/create-ns 'my.lazy-ns)
           lazy-greet (fn [x] (str "Hello " x "!"))
           lazy-ns {'lazy-greet (sci/new-var 'lazy-greet lazy-greet {:ns mlns})}]
       (sci/add-namespace! ctx libname lazy-ns)
       ;; empty map return value, SCI will still process `:as` and `:refer`
       {}))))

(def ctx (sci/init {:async-load-fn async-load-fn}))

(def code "

(ns foo (:require [my.lazy-ns :refer [lazy-greet]]))
(lazy-greet \"Michiel\")

")

(p/let [result (scia/eval-string* ctx code)]
  (println result)) ;; prints: "Hello Michiel!"
```

In this example we lazy load a namespace into the SCI context. Note that the
functions mapped in this namespace may come in asynchronously e.g. via an http
request. The `:async-load-fn` is used to process the `(:require ..)` part of the
`ns` form. In our implementation we use `sci/add-namespace!` to mutate the SCI
context and then we return an empty map indicating that SCI will process `:as`
and `:refer` for us. This can be prevented by return a map with `:handled true`
like in the below example.

Additionally supported return values:

- `:source "..."`, CLJS source to be evaluated

## Registering a JS library as a class

``` clojure
(ns example
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(defn async-load-fn
  [{:keys [libname opts ctx ns]}]
  (case libname
    "some_js_lib"
    (p/let [js-lib (p/resolved #js {:add +})]
      (sci/add-class! ctx (:as opts) js-lib)
      (sci/add-import! ctx ns (:as opts) (:as opts))
      {:handled true})))

(def ctx (sci/init {:async-load-fn async-load-fn}))

(defn codify [exprs]
  (str/join "\n" (map pr-str exprs)))

(def code
  (codify
   '[(ns example (:require ["some_js_lib" :as my-lib]))
     (my-lib/add 1 2)]))

(p/let [result (scia/eval-string* ctx code)]
  (println "Result:" result)) ;; Promise that prints "Result: 3"
```

In this example we simulate loading a JavaScript library asynchronously. In
practise the library could come in via an asynchronous HTTP Request, etc. but
here we just simulate it by returning a promise with JavaScript object that has
one function, `libfn`. In the async load fn, we check if the library was
required and then register it as a class in the context and as an import in the
current namespace. The `:handled true` return value indicates that SCI will not
do anything with aliases, as the `async-load-fn` has handled this already.

## Require

By default `require` is synchronous in SCI. The `sci.async` namespace contains an asynchronous require which can be substituted:

``` clojure
(ns example
  (:require [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(def async-load-fn (fn [{:keys [libname]}]
                     (case libname
                       acme.foo {:source "(ns acme.foo) (defn the-fn [] :hello)"}
                       acme.bar {:source "(ns acme.bar) (defn the-fn [] :bye)"})))

(def ctx (sci/init {:namespaces {'clojure.core {'require scia/require}}
                    :async-load-fn async-load-fn}))

(p/let [res (scia/eval-string* ctx "(require '[acme.foo :as foo]) (foo/the-fn)")
        _ (assert (= :hello res))
        res (scia/eval-string* ctx "(require '[acme.bar :as bar]) (bar/the-fn)")
        _ (assert (= :bye res))])
```

Like the `ns` form, a top level `require` is handled as if it happened
synchronously: the next expression is scheduled after the require finishes.
