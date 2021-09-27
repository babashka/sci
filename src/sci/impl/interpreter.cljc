(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-1])
  (:require
   [clojure.tools.reader.reader-types :as r]
   [sci.impl.analyzer :as ana]
   [sci.impl.evaluator :as eval]
   [sci.impl.opts :as opts]
   [sci.impl.parser :as p]
   [sci.impl.types :as t]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(def stats (atom {:parse 0 :analysis 0 :eval 0 :total 0}))
(defn update-stats [_ctx k t]
  (swap! stats (fn [stats]
                 (-> stats
                     (update k + t)
                     (update :total + t)))))

(defn print-stats []
  (prn (zipmap (keys @stats)
               (map #(/ (double %) 1000000.0) (vals @stats)))))

(defn eval-form [ctx form]
  (if (seq? form)
    (if (= 'do (first form))
      (loop [exprs (rest form)
             ret nil]
        (if (seq exprs)
          (recur
           (rest exprs)
           (eval-form ctx (first exprs)))
          ret))
      (when (or (not (:uberscript ctx))
                (= 'ns (first form))
                (= 'require (first form)))
        (let [analyzed (ana/analyze ctx form true)
              bindings (:bindings ctx)
              ret (if (instance? #?(:clj sci.impl.types.EvalForm
                                    :cljs sci.impl.types/EvalForm) analyzed)
                    (eval-form ctx (t/getVal analyzed))
                    (eval/eval ctx bindings analyzed))]
          ret)))
    (let [analyzed (ana/analyze ctx form)
          bindings (:bindings ctx)
          ret (eval/eval ctx bindings analyzed)]
      ret)))

(vreset! utils/eval-form-state eval-form)

(defn eval-string* [ctx s]
  (vars/with-bindings {vars/current-ns @vars/current-ns}
    (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
      (loop [ret nil]
        (let [expr (p/parse-next ctx reader)]
          (if (utils/kw-identical? p/eof expr)
            ret
            (let [ret (eval-form ctx expr)]
              (recur ret))))))))

(vreset! utils/eval-string* eval-string*)

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s opts]
   (let [init-ctx (opts/init opts)
         ret (eval-string* init-ctx s)]
     ret)))

;;;; Scratch

(comment
  (eval-string "((fn f [x] (if (< x 3) (recur (inc x)) x)) 0)")
  )
