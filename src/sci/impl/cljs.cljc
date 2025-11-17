(ns sci.impl.cljs
  {:no-doc true}
  (:require [sci.impl.macros :as macros])
  #?(:cljs (:require-macros [sci.impl.cljs :refer [require-cljs-analyzer-api when-not-var-exists]])))

(macros/deftime
  #?(:clj (do (def cljs-ns-publics (resolve 'cljs.analyzer.api/ns-publics))
              (def cljs-find-ns (resolve 'cljs.analyzer.api/find-ns))
              (def cljs-resolve (resolve 'cljs.analyzer.api/resolve))))
  #_:clj-kondo/ignore
  (defmacro ^:private require-cljs-analyzer-api []
    (macros/? :clj
              ;; noop, macro executed from JVM Clojure, not within CLJS compiler
              nil
              :cljs #?(;; macro executed from JVM Clojure, within CLJS compiler
                       :clj
                       (do (require '[cljs.analyzer.api])
                           (def cljs-ns-publics (resolve 'cljs.analyzer.api/ns-publics))
                           (def cljs-find-ns (resolve 'cljs.analyzer.api/find-ns)))
                       ;; self-hosted CLJS, no require supported but also not necessary
                       :cljs nil)))

  (defmacro when-not-var-exists [var & body]
    (let [resolver (resolve 'cljs.analyzer.api/resolve)
          resolved (resolver {} var)]
      (when-not resolved
        `(do ~@body)))))

;; When CLJS code is compiled, we know for sure that we can require the CLJS analyzer API
#?(:cljs (require-cljs-analyzer-api))
