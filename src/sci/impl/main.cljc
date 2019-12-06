(ns sci.impl.main
  "Only used for testing"
  {:no-doc true}
  (:require [sci.core :as sci :refer [eval-string]]
            #?(:clj [sci.addons :as addons])
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn]))
  #?(:clj (:gen-class)))

;; for testing only
(defn -main [& [form ctx]]
  (let [v (sci/with-bindings {sci/out *out*}
            (eval-string
             form
             (-> (edn/read-string ctx)
                 (merge #?(:clj addons/future)))))]
    (when (some? v) (prn v )))
  #?(:clj (shutdown-agents)))
