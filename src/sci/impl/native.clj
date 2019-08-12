(ns sci.impl.native
  {:no-doc true}
  (:require [sci.core :refer [eval-string]]
            [clojure.edn :as edn])
  (:gen-class))

;; for testing only
(defn -main [& [form bindings]]
  (prn (eval-string form {:bindings (edn/read-string bindings)})))
