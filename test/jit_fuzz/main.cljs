(ns jit-fuzz.main
  "Differential fuzzer: same generated program through interp and jit,
  values, error messages and error locations must all agree."
  (:require [clojure.string :as str]
            [jit-fuzz.gen :as gen]
            [sci.core :as sci]
            [sci.impl.jit :as jit]))

(defn- render
  "Bounded print (generated programs can build structurally-shared values
  that explode when fully printed or walked). Fns print differently between
  modes; normalize on the string."
  [v]
  (-> (binding [*print-length* 64 *print-level* 6] (pr-str v))
      (str/replace #"#object\[[^\]]*\]" "#fn")))

(defn- eval-one [mode src]
  (if (= :jit mode) (jit/enable!) (jit/disable!))
  ;; unrestricted: exercises the direct interop emission (nbb-style)
  (let [ctx (sci/init {:unrestricted true})]
    (try
      {:val (render (sci/eval-string* ctx src))}
      (catch :default e
        (let [d (ex-data e)]
          {:err (.-message e)
           :line (:line d)
           :col (:column d)}))
      (finally (jit/disable!)))))

(defn- run-seed [seed]
  (let [src (gen/gen-program seed)
        a (eval-one :interp src)
        b (eval-one :jit src)]
    (when-not (= a b)
      {:seed seed :src src :interp a :jit b})))

(defn -main []
  (let [args (.slice js/process.argv 2)
        start (js/parseInt (or (aget args 0) "0"))
        n (js/parseInt (or (aget args 1) "1000"))
        t0 (js/performance.now)]
    (when (exists? js/process.env.FUZZ_DUMP)
      (dotimes [i 3]
        (println "---- seed" (+ start i))
        (println (gen/gen-program (+ start i)))))
    (loop [seed start
           mismatches 0]
      (if (< seed (+ start n))
        (do
          (when (zero? (mod seed 500))
            (println "..." seed))
          (if-let [m (run-seed seed)]
            (do (println "==== MISMATCH seed" (:seed m))
                (println (:src m))
                (println "interp:" (pr-str (:interp m)))
                (println "jit:   " (pr-str (:jit m)))
                (recur (inc seed) (inc mismatches)))
            (recur (inc seed) mismatches)))
        (do
          (println "seeds" start "to" (dec (+ start n))
                   "| mismatches:" mismatches
                   "|" (.toFixed (- (js/performance.now) t0) 0) "ms")
          (when (pos? mismatches)
            (set! (.-exitCode js/process) 1)))))))

(-main)
