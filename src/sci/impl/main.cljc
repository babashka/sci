(ns sci.impl.main
  "Only used for testing"
  {:no-doc true}
  (:require
   [clojure.tools.reader.reader-types :as r]
   [sci.core :as sci :refer [eval-string]]
   #?(:clj [sci.addons :as addons])
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   [sci.impl.analyzer :as ana]
   [sci.impl.interpreter :as i]
   [sci.impl.opts :as opts]
   [sci.impl.parser :as p])
  #?(:clj (:gen-class)))

(defn ^:skip-aot main [& [form ctx n]]
  (let [n (when n (Integer. n))
        ctx (edn/read-string ctx)
        ctx (-> ctx #?(:clj (addons/future)))
        v (sci/with-bindings {sci/out *out*
                              #?@(:clj [sci/err *err*])}
            (if n
              (let [ctx (opts/init ctx)
                    reader (r/indexing-push-back-reader (r/string-push-back-reader form))
                    form (p/parse-next ctx reader)
                    form (ana/analyze ctx form)]
                (loop [i 0]
                  (let [ret (i/interpret ctx form)]
                    (if (< i n)
                      (recur (inc i))
                      ret))))
              (eval-string
               form
               (-> ctx
                   #?(:clj (addons/future))))))]
    (when (some? v) (prn v))))

;; for testing only
(defn -main [& args]
  (apply main args)
  #?(:clj (shutdown-agents)))
