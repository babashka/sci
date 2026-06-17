(ns sci.copy-var-inlined-clash-ns
  (:refer-clojure :exclude [get]))

;; Function deliberately named 'get', the same unqualified name as a clojure.core
;; inlined-var. Mirrors the babashka httpkit pattern where a third-party ns
;; defines a function whose name collides with an entry in inlined-vars.
(defn get [m k] (clojure.core/get m k))
