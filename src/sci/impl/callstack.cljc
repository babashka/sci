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
  #?(:clj (.set callstack (conj (.get callstack) data))
     :cljs (swap! callstack conj data)))

(defn pop! []
  #?(:clj (.set callstack (pop (.get callstack)))
     :cljs (swap! callstack pop)))
