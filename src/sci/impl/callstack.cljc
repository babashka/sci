(ns sci.impl.callstack
  (:refer-clojure :exclude [pop!]))

#?(:clj
   (def ^ThreadLocal callstack (proxy [ThreadLocal] []
                             (initialValue [] [])))
   :cljs
   (def callstack (atom [])))

(defn get-callstack []
  #?(:clj (.get callstack)
     :cljs @callstack))

(defn push! [data]
  (.set callstack (conj (.get callstack) data)))

(defn pop! []
  (.set callstack (pop (.get callstack))))
