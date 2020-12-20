(ns sci.bench
  (:require [criterium.core :as crit]
            [sci.core :as sci]
            [sci.impl.analyzer :as ana]
            [sci.impl.evaluator :as eval]
            #_[sci.impl.interpreter :as i]))

(defn bench-expr [expr]
  (println "BENCHMARKING EXPRESSION:" expr)
  (let [ctx (sci/init {})]
    (println "ANALYSIS:")
    (crit/quick-bench (ana/analyze ctx expr))
    (let [ana (ana/analyze ctx expr)]
      (prn '-> (eval/eval ctx ana))
      (println "EVALUATION:")
      (crit/quick-bench (eval/eval ctx ana)))))

(defn -main []
  (bench-expr '(let [x 1 y 2] (+ x y))))
