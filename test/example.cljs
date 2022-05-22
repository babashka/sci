(ns example
  (:require [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(def async-load-fn (fn [{:keys [libname]}]
                     (p/resolved
                      (case libname
                        acme.foo {:source "(ns acme.foo) (defn the-fn [] :hello)"}
                        acme.bar {:source "(ns acme.bar) (defn the-fn [] :bye)"}))))

(def ctx (sci/init {:namespaces {'clojure.core {'require scia/require}}
                    :async-load-fn async-load-fn}))

(p/let [res (scia/eval-string* ctx "(require '[acme.foo :as foo]) (foo/the-fn)")
        _ (assert (= :hello res))
        res (scia/eval-string* ctx "(require '[acme.bar :as bar]) (bar/the-fn)")
        _ (assert (= :bye res))])
