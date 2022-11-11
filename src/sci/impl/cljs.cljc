(ns sci.impl.cljs
  {:no-doc true}
  (:require [sci.impl.macros :as macros])
  #?(:cljs (:require-macros [sci.impl.cljs :refer [require-cljs-analyzer-api]])))

;; self-hosted is satisfied here
(def cljs-ns-publics (some-> (resolve 'cljs.analyzer.api/ns-publics) deref))
(def cljs-resolve (some-> (resolve 'cljs.analyzer.api/resolve) deref))

(macros/deftime

  (defmacro require-cljs-analyzer-api []
    (macros/? :clj
              ;; noop, macro executed from JVM Clojure, not within CLJS compiler
              nil
              :cljs #?(;; macro executed from JVM Clojure, within CLJS compiler
                       :clj
                       ;; This takes care of the scenario that sci.core is loaded outside of CLJS first and then again from CLJS vis :require-macros
                       (do (require '[cljs.analyzer.api])
                           (intern 'sci.impl.cljs 'cljs-ns-publics @(resolve 'cljs.analyzer.api/ns-publics))
                           (intern 'sci.impl.cljs 'cljs-resolve @(resolve 'cljs.analyzer.api/resolve)))
                       :cljs nil))))
