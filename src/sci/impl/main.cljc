(ns sci.impl.main
  "Only used for testing"
  {:no-doc true}
  (:require
   #?(:clj [sci.addons :as addons])
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   #?(:clj [clojure.java.io :as io])
   [sci.core :as sci :refer [eval-string]])
  #?(:clj (:gen-class)))

#?(:clj
   (defn time*
     "Evaluates expr and prints the time it took.  Returns the value of
  expr."
     [_ _ expr]
     `(let [start# (. System (nanoTime))
            ret# ~expr]
        (prn (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
        ret#)))

(defn opts [ctx]
  (let [ctx (-> ctx #?(:clj (addons/future)))
        #?@(:clj [ctx (assoc-in ctx [:namespaces 'clojure.core 'time] (with-meta time* {:sci/macro true}))])
        #?@(:clj [ctx (assoc-in ctx [:classes 'java.lang.System] System)])
        #?@(:clj [ctx (assoc-in ctx [:classes 'java.lang.IllegalArgumentException] IllegalArgumentException)])
        #?@(:clj [ctx (assoc-in ctx [:classes 'java.lang.Thread] Thread)])
        #?@(:clj [ctx (assoc-in ctx [:classes 'java.lang.Math] Math)])
        #?@(:clj [ctx (assoc-in ctx [:imports] {'System 'java.lang.System
                                                'Thread 'java.lang.Thread
                                                'Math 'java.lang.Math})])]
    ctx))

(defn ^:skip-aot main [& [form ctx]]
  (let [ctx (edn/read-string ctx)
        ctx (opts ctx)
        v (sci/with-bindings {sci/out *out*
                              #?@(:clj [sci/err *err*])}
            (let [#?@(:clj [f (io/file form)])
                  #?@(:clj [form (if (.exists f)
                                   (slurp f) form)])]
              (eval-string
               form
               (-> ctx
                   #?(:clj (addons/future))))))]
    (when (some? v) (prn v))))

;; for testing only
(defn -main [& args]
  (apply main args)
  #?(:clj (shutdown-agents)))
