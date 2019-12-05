(ns sci.impl.main
  "Only used for testing"
  {:no-doc true}
  (:require [sci.core :as sci :refer [eval-string]]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn]))
  #?(:clj (:gen-class)))

(defn future*
  [_ _ & body]
  `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
     (~'future-call f#)))

;; for testing only
(defn -main [& [form ctx]]
  (let [v (sci/with-bindings {sci/out *out*}
            (eval-string
             form
             (-> (edn/read-string ctx)
                 (update :bindings merge
                         {#?@(:clj ['future (with-meta future*
                                              {:sci/macro true})
                                    'future-call future-call])}))))]
    (when (some? v) (prn v )))
  #?(:clj (shutdown-agents)))
