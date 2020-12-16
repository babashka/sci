;; run with:
;; clojure -J-Dclojure.compiler.direct-linking=true -M script/optimizations/GH_470_let_closure.clj
;; master: "Elapsed time: 6990.686673 msecs"
;; let-closure: "Elapsed time: 5351.171676 msecs"

(require '[sci.core :as sci])

(def ctx (sci/init {}))

(require '[sci.impl.analyzer :as ana])

(def analyzed (ana/analyze ctx '(let [x 1] x)))

(require '[sci.impl.evaluator :as eval])

(prn (eval/eval ctx analyzed))

(time (dotimes [_ 10000000]
        (eval/eval ctx analyzed)))
