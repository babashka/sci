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

(defn eval-form-stats [ctx form]
  (if (seq? form)
    (if (= 'do (first form))
      (loop [exprs (rest form)
             ret nil]
        (if (seq exprs)
          (recur
           (rest exprs)
           (eval-form-stats ctx (first exprs)))
          ret))
      (when (or (not (:uberscript ctx))
                (= 'ns (first form))
                (= 'require (first form)))
        (let [#?@(:clj [a0 (System/nanoTime)])
              analyzed (ana/analyze ctx form true)
              #?@(:clj [a1 (System/nanoTime)
                        _ (update-stats ctx :analysis (- a1 a0))])
              ret (if (instance? sci.impl.types.EvalForm analyzed)
                    (eval-form-stats ctx (t/getVal analyzed))
                    (let [#?@(:clj [e0 (System/nanoTime)])
                          ret (eval/eval ctx analyzed)
                          #?@(:clj [e1 (System/nanoTime)
                                    _ (update-stats ctx :eval (- e1 e0))])]
                      ret))]
          ret)))
    (let [#?@(:clj [t0 (System/nanoTime)])
          analyzed (ana/analyze ctx form)
          #?@(:clj [t1 (System/nanoTime)
                    _ (update-stats ctx :analysis (- t1 t0))])
          #?@(:clj [t0 (System/nanoTime)])
          ret (eval/eval ctx analyzed)
          #?@(:clj [t1 (System/nanoTime)
                    _ (update-stats ctx :eval (- t1 t0))])]
      ret)))

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
              ret (if (instance? sci.impl.types.EvalForm analyzed)
                    (eval-form ctx (t/getVal analyzed))
                    (eval/eval ctx analyzed))]
          ret)))
    (let [analyzed (ana/analyze ctx form)
          ret (eval/eval ctx analyzed)]
      ret)))

#?(:clj
   (when (System/getenv "SCI_STATS")
     (alter-var-root #'eval-form (constantly eval-form-stats))))

(vreset! utils/eval-form-state eval-form)

;; with stats
(defn eval-string-stats [ctx s]
  (let [ctx (assoc ctx :id (or (:id ctx) (gensym)))]
    (vars/with-bindings {vars/current-ns @vars/current-ns}
      (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
        (loop [ret nil]
          (let [#?@(:clj [t0 (System/nanoTime)])
                expr (p/parse-next ctx reader)
                #?@(:clj [t1 (System/nanoTime)
                          _ (update-stats ctx :parse (- t1 t0))])]
            (if (utils/kw-identical? p/eof expr)
              (do
                (print-stats)
                ret)
              (let [ret (eval-form ctx expr)]
                (recur ret)))))))))

(defn eval-string* [ctx s]
  (let [ctx (assoc ctx :id (or (:id ctx) (gensym)))]
    (vars/with-bindings {vars/current-ns @vars/current-ns}
      (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
        (loop [ret nil]
          (let [expr (p/parse-next ctx reader)]
            (if (utils/kw-identical? p/eof expr)
              ret
              (let [ret (eval-form ctx expr)]
                (recur ret)))))))))

#?(:clj
   (when (System/getenv "SCI_STATS")
     (alter-var-root #'eval-string* (constantly eval-string-stats))))

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
