(ns sci.impl.bench
  (:require [sci.impl.macros :as macros])
  #?(:cljs (:require-macros [sci.impl.bench :refer [record]])))

(def times (atom {}))

(defn reset-times []
  (reset! times {}))

(defn print-times []
  (doseq [[k v] @times]
    (println k v)))

(macros/deftime
  (defmacro record [k & body]
    `(let [start# (. System (nanoTime))
           ret# (do ~@body)
           end#  (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
       (swap! sci.impl.utils/times update ~k (fn [prev#]
                                               (if (nil? prev#) end# (+ prev# end#))))
       ret#)))
