# Asynchronous evaluation

This is work in progress.

The `sci.async` namespace offers async evaluation for ClojureScript. Difference
with the synchronous evaluation in `sci.core`:

- `(ns ...)` forms are evaluated asynchronously - loading can be configured
  via the `:async-load-fn` option.
- The return value from evaluation is a JavaScript promise.

Example:

``` clojure
(ns example
  (:require [clojure.string :as str]
            [sci.async :as scia]
            [sci.core :as sci]))

(defn async-load-fn
  [{:keys [libname opts ctx ns]}]
  (case libname
    "some_js_lib"
    (-> (js/Promise.resolve #js {:libfn (fn [] "result!")})
        (.then (fn [mod]
                 (sci/add-class! ctx (:as opts) mod)
                 (sci/add-import! ctx ns (:as opts) (:as opts))
                 {:handled true})))))

(def ctx (sci/init {:async-load-fn async-load-fn}))

(def code (str/join
           "\n"
           (map pr-str '[(ns example (:require ["some_js_lib" :as my-lib]))
                         (my-lib/libfn)])))

(scia/eval-string* ctx code) ;; Promise that resolves to "result!"
```

In this example we simulate loading a JavaScript library asynchronously. In
practise the library could come in via an asynchronous HTTP Request, etc. but
here we just simulate it by returning a promise with JavaScript object that has
one function, `libfn`. In the async load fn, we check if the library was
required and then register it as a class in the context and as an import in the
current namespace. The `:handled true` return value indicates that SCI will not
do anything with aliases, as the `async-load-fn` has handled this already.
