(ns sci.profile
  (:require [sci.impl.main :as main]))

(comment)

;; clojure -J-Dclojure.compiler.direct-linking=true -M:profile "(loop [val 0 cnt 1000000] (if (pos? cnt) (recur (inc val) (dec cnt)) val))" "{}" 100

(require '[clj-async-profiler.core :as prof])

(defn -main [& options]
  (prof/profile (apply main/main options))
  (shutdown-agents))
