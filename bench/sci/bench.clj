(ns sci.bench
  (:require #_[sci.impl.interpreter :as i]
            [clojure.tools.cli :as cli]
            [criterium.core :as crit]
            [sci.core :as sci]
            [sci.impl.analyzer :as ana]
            [sci.impl.types :as types]))

(defn bench-expr [{:keys [:parse :analyze :evaluate :complete :sexpr :quick]}]
  (println "BENCHMARKING EXPRESSION:" sexpr)
  (let [ctx (sci/init {})
        rdr (sci/reader sexpr)
        parsed (sci/parse-next ctx rdr)
        upper-sym (gensym)
        cb (volatile! {upper-sym {0 {:syms {}}}})
        ctx (assoc ctx
                   :parents [upper-sym 0]
                   :closure-bindings cb)]
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
    (let [ana (ana/analyze ctx parsed)
          binding-array-size (count (get-in @cb [upper-sym 0 :syms]))
          bindings (object-array binding-array-size)]
      (println "EVALUATION:")
      (prn '-> (types/eval ana ctx bindings))
      (when (or complete evaluate)
        (if quick
          (crit/quick-bench (types/eval ana ctx bindings))
          (crit/bench (types/eval ana ctx bindings)))))))

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
