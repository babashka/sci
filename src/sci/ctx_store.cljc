(ns sci.ctx-store
  "Canonical place for projects to store, update and retrieve a context.
  This can be used by projects that need to expose their context to
  functions. SCI does not populate this dynamic var itself during
  evaluation. Projects like `sci.configs` assume this var to be set in
  some of their functions."
  #?(:cljs (:require-macros [sci.ctx-store :refer [with-ctx]])))

(def ^:private ^:dynamic
  *ctx*
  "Dynamic var in which context is stored. Don't use directly, but only
  via public API."
  nil)

(defn reset-ctx!
  "Store `ctx`"
  [ctx]
  #?(:clj (clojure.core/alter-var-root (var *ctx*) (constantly ctx))
     :cljs (set! *ctx* ctx)))

(defn swap-ctx!
  "Update `ctx` using `f` and `args`"
  [f & args]
  #?(:clj (apply clojure.core/alter-var-root (var *ctx*) f args)
     :cljs (set! *ctx* (apply f *ctx* args))))

(defn get-ctx
  "Retrieve stored ctx or throw an exception."
  []
  (or *ctx*
      (let [msg "No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!"]
        (throw #?(:clj (java.lang.IllegalStateException. msg)
                  :cljs (js/Error. msg))))))

(defmacro with-ctx
  "Bind `ctx` during execution of body."
  [ctx & body]
  `(binding [*ctx* ~ctx]
     ~@body))
