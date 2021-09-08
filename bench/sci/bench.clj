(ns sci.bench
  (:require #_[sci.impl.interpreter :as i]
            [clojure.tools.cli :as cli]
            [criterium.core :as crit]
            [sci.core :as sci]
            [sci.impl.analyzer :as ana]
            [sci.impl.evaluator :as eval]))

(defn bench-expr [{:keys [:parse :analyze :evaluate :complete :sexpr :quick]}]
  (println "BENCHMARKING EXPRESSION:" sexpr)
  (let [ctx (sci/init {})
        rdr (sci/reader sexpr)
        parsed (sci/parse-next ctx rdr)]
    (println "PARSE:")
    (prn '-> parsed)
    (when (or complete parse)
      (if quick
        (crit/quick-bench (sci/parse-next ctx rdr))
        (crit/bench (sci/parse-next ctx rdr))))
    (println "ANALYSIS:")
    (when (or complete analyze)
      (if quick
        (crit/quick-bench (ana/analyze ctx parsed))
        (crit/bench (ana/analyze ctx parsed))))
    (let [ana (ana/analyze ctx parsed)]
      (println "EVALUATION:")
      (prn '-> (eval/eval ctx {} ana))
      (when (or complete evaluate)
        (if quick
          (crit/quick-bench (eval/eval ctx {} ana))
          (crit/bench (eval/eval ctx {} ana)))))))

(def cli-options
  ;; An option with a required argument
  [["-p" "--parse"]
   ["-a" "--analyze"]
   ["-e" "--evaluate"]
   ["-c" "--complete"]
   ["-q" "--quick"]
   ["-s" "--sexpr SEXPR"
    :default "(let [x 1 y 2] (+ x y))"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [parsed (cli/parse-opts args cli-options)
        opts (:options parsed)]
    (if (or (empty? args) (:help opts))
      (println "Options: \n" cli-options)
      (bench-expr (:options parsed)))))
