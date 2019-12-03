(ns sci.impl.main
  "Only used for testing"
  {:no-doc true}
  (:require [sci.core :refer [eval-string]]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn]))
  #?(:clj (:gen-class)))

(defn future*
  [_ _ & body]
  `(~'future-call (fn [] ~@body)))

;; for testing only
(defn -main [& [form ctx]]
  (let [v (eval-string
           form
           (-> (edn/read-string ctx)
               (update :bindings merge
                       {#?@(:clj ['future (with-meta future*
                                            {:sci/macro true})
                                  'future-call future-call])})
               (assoc #?@(:clj [:in *in*])
                      :out *out*)))]
    (when (some? v) (prn v )))
  #?(:clj (shutdown-agents)))
