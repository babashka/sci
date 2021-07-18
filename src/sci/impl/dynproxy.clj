(ns sci.impl.dynproxy
  (:import [java.lang.reflect InvocationHandler Proxy]))

(set! *warn-on-reflection* true)

(defn handler []
  (reify InvocationHandler
    (invoke [this proxy method args]
      (case (.getName method)
        "entrySet"
        (set (seq {:a 1}))
        "put"
        42))))

(defn new-proxy [target-classes]
  (Proxy/newProxyInstance (.getClassLoader clojure.lang.RT)
                          (into-array Class target-classes)
                          (handler)))

(comment
  (def proxy1 (new-proxy [java.util.Map]))
  (prn (class proxy1))
  (.put ^java.util.Map proxy1 1 2))
