# Asynchronous evaluation

The `sci.async` namespace offers async evaluation for ClojureScript. This is
work in progress. Please try it out and provide feedback.

Difference with the synchronous evaluation in `sci.core`:

- `(ns ...)` forms are evaluated asynchronously - loading can be configured
  via the `:async-load-fn` option.
- The return value from evaluation is a JavaScript promise.
- Optionally `require` can be made async using `scia/require`, see [Require](#require).

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

## Require

By default `require` is synchronous in SCI. The `sci.async` namespace contains
an asynchronous `require` which can be substituted in a context:

``` clojure
(ns example
  (:require [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(def async-load-fn (fn [{:keys [libname]}]
                     (p/resolved
                      (case libname
                        acme.foo {:source "(ns acme.foo) (defn the-fn [] :hello)"}
                        acme.bar {:source "(ns acme.bar) (defn the-fn [] :bye)"}))))

;; Substitute scia/require for clojure.core/require:
(def ctx (sci/init {:namespaces {'clojure.core {'require scia/require}}
                    :async-load-fn async-load-fn}))

(p/let [res (scia/eval-string* ctx "(require '[acme.foo :as foo]) (foo/the-fn)")
        _ (assert (= :hello res))
        res (scia/eval-string* ctx "(require '[acme.bar :as bar]) (bar/the-fn)")
        _ (assert (= :bye res))])
```

Like the `ns` form, a top level `require` is handled as if it happened
synchronously: the next expression is scheduled after the require finishes.

SCI does not limit `require` to occur at the beginning of a file, it may be used
anywhere, as long as the result is visible as a top level value:

``` clojure
(ns foo)

(+ 1 2 3)

(require '[some.lazy-ns :as slns])

(slns/the-fn)

(when (odd? 3)
  (require '[some.other-lazy-ns]))
```

## Registering a JS library as a class

> NOTE: the below code can be replaced by using `sci/add-js-lib!` from SCI > 0.6.37. See [docs](../README.md#).

``` clojure
(ns example
  (:require
   [clojure.string :as str]
   [goog.object :as gobject]
   [promesa.core :as p]
   [sci.async :as scia]
   [sci.core :as sci]))

(defn async-load-fn
  [{:keys [libname opts ctx ns]}]
  (let [[libname suffix] (str/split libname "$")]
       (case libname
         "some_js_lib"
         (p/let [js-lib (p/resolved #js {:add +
                                       :subtract -
                                     :multiply *
                                     :default *})]
           (let [js-lib (if suffix
                          (gobject/getValueByKeys js-lib (.split suffix "."))
                          js-lib)
                 munged (symbol (munge libname))]
             ;; register class globally in context
             (sci/add-class! ctx munged js-lib)
             (let [{:keys [as refer]} opts]
               (when as
                 ;; import class in current namespace with reference to globally
                 ;; registed class
                 (sci/add-import! ctx ns munged as))
               (when refer
                 (doseq [sym refer]
                   (let [prop (gobject/get js-lib sym)
                         sub-libname (str munged "$" prop)]
                     ;; register sub-library globally
                     (sci/add-class! ctx sub-libname prop)
                     ;; add import to sub-library in current namespace
                     (sci/add-import! ctx ns sub-libname sym))))))
           {:handled true}))))

(def ctx (sci/init {:async-load-fn async-load-fn
                    ;; async require override
                    :namespaces {'clojure.core {'require scia/require}}
                    ;; allow JS interop globally
                    :classes {'js goog/global :allow :all}}))

;; allow printing
(sci/alter-var-root sci/print-fn (constantly *print-fn*))

(def code-1
  "
(ns example (:require [\"some_js_lib\" :as my-lib :refer [subtract]]))
[(my-lib/add 1 2) (subtract 3 2)]
")

;; Library property namespaces:
(def code-2 "
(require '[\"some_js_lib$default\" :as awesome])
(require '[\"some_js_lib$add\" :as add])
[(awesome 3 2) (add 4 5)]
")

(p/let [result (scia/eval-string* ctx code-1)
        _ (println "Result:" result) ;; [3 1]
        result (scia/eval-string* ctx code-2)
        _ (println "Result:" result) ;; [5 9]
        ]
  )
```

In this example we simulate loading a JavaScript library asynchronously. In
practise the library could come in via an asynchronous HTTP Request, etc. but
here we just simulate it by returning a promise with JavaScript object that has
a couple of functions. In the async load fn we register the JS library as a
global class in the context and as an import in the current namespace. The
`:handled true` key/value in the return value indicates that SCI will handle `:refer` and `:as`, because we already did that ourselves in the `async-load-fn`.

In the `code-2` fragment we use library property namespaces. These were
introduced in ClojureScript `1.10.844`. You can read about that
[here](https://clojurescript.org/news/2021-04-06-release#_library_property_namespaces).
We support that in `async-load-fn` by splitting the libname on `$` and getting
the properties out of the JS library, then registering that as a class and
import.

## Preserving namespace state

To preserve namespace state, use `scia/eval-string+` which is similar to
`scia/eval-string*` but returns a map with `:val` and `:ns` in a promise. To
keep the namespace state of the previous evaluation intact, you can feed the
entire return value back into the next evaluation:

``` clojure
(p/let [ctx (sci/init {})
        {:keys [_ ns] :as ret} (scia/eval-string+ ctx "(ns foo)")
        _ (is (= "foo" (str ns)))
        {:keys [val ns]} (scia/eval-string+ ctx "(defn foo [] :hello) (foo/foo)" ret)
        _ (is (= :hello val))
        _ (is (= "foo" (str ns)))
        ;; no passing of previous state: ns is back to user again:
        {:keys [val ns]} (scia/eval-string+ ctx "(defn bar []) (symbol #'bar)")
        _ (is (= 'user/bar val))
        _ (is (= "user" (str ns)))])
```
