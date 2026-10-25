(ns sci.perf-regressions
  (:require [sci.core :as sci]
            [sci.impl.vars :as vars]))

(defn- best-of-three [f]
  (apply min
         (for [_ (range 3)]
           (let [start (System/nanoTime)]
             (f)
             (/ (double (- (System/nanoTime) start)) 1e6)))))

(defn -main [& [mode]]
  (let [opts (if (= mode "forkable") {:runtime-mode :forkable} {})
        ctx (sci/init opts)
        closure-form "(dotimes [_ 1000000] (fn [] 1))"
        binding-form "(dotimes [_ 100000] (binding [*bench-value* 1] *bench-value*))"
        conveyor (vars/binding-conveyor-fn (fn [x] x))]
    (sci/eval-string* ctx "(def ^:dynamic *bench-value* 0)")
    (sci/eval-string* ctx "(dotimes [_ 10000] (fn [] 1))")
    (dotimes [_ 10000] (conveyor 1))
    (println mode "init-1000-ms" (best-of-three #(dotimes [_ 1000] (sci/init opts))))
    (println mode "binding-100k-ms" (best-of-three #(sci/eval-string* ctx binding-form)))
    (println mode "closures-ms" (best-of-three #(sci/eval-string* ctx closure-form)))
    (println mode "closures-8-threads-ms"
             (best-of-three
              #(doall
                (map deref
                     (repeatedly 8
                                 (fn []
                                   (future
                                     (sci/eval-string*
                                      ctx "(dotimes [_ 125000] (fn [] 1))"))))))))
    (println mode "conveyor-ms" (best-of-three #(dotimes [_ 1000000] (conveyor 1)))))
  (shutdown-agents))
