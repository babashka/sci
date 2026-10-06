#!/usr/bin/env bb

;; Usage: bb script/sync_clojure_destructure_tests.clj <clojure-checkout> <sha>

(require '[babashka.process :as p]
         '[clojure.string :as str]
         '[rewrite-clj.zip :as z])

(def test-names
  '[singleton-map-in-destructure-context trailing-map-destructuring
    keys-bang syms-bang strs-bang missing-directive select-directive
    select-or-defaults all-directive excess selector-test])

(def out "test/sci/clojure_destructure_test.cljc")

(defn upstream-source [dir sha]
  (:out (p/sh {:dir dir} "git" "show"
              (str sha ":test/clojure/test_clojure/data_structures.clj"))))

(defn deftests [src]
  (loop [zloc (z/of-string src) acc {}]
    (let [acc (if (and (= :list (z/tag zloc))
                       (= 'deftest (some-> zloc z/down z/sexpr)))
                (assoc acc (-> zloc z/down z/right z/sexpr) (z/string zloc))
                acc)]
      (if-let [n (z/right zloc)] (recur n acc) acc))))

(defn indent [s]
  (str/join "\n" (map #(if (str/blank? %) % (str "    " %)) (str/split-lines s))))

(let [[dir sha] *command-line-args*
      _ (when-not (and dir sha)
          (binding [*out* *err*]
            (println "Usage: bb script/sync_clojure_destructure_tests.clj <clojure-checkout> <sha>")
            (System/exit 1)))
      sha (str/trim (:out (p/sh {:dir dir} "git" "rev-parse" sha)))
      tests (deftests (upstream-source dir sha))
      missing (remove tests test-names)]
  (when (seq missing)
    (binding [*out* *err*]
      (println "Not found upstream:" (str/join " " missing))
      (System/exit 1)))
  (spit out
        (str ";; Destructuring tests from clojure/clojure data_structures.clj at " sha ", run in sci.
;; Regenerate with: bb script/sync_clojure_destructure_tests.clj <clojure-checkout> <sha>

(ns sci.clojure-destructure-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]))

(def prelude
  (str \"(def failures (atom []))
(defmacro is [form & _]
  (if (and (seq? form) (= 'thrown? (first form)))
    (list 'try (cons 'do (nnext form))
          (list 'swap! 'failures 'conj (list 'quote form))
          (list 'catch '\" #?(:clj \"Exception\" :default \":default\") \" '_ nil))
    (list 'when-not form (list 'swap! 'failures 'conj (list 'quote form)))))
(defmacro testing [_ & body] (cons 'do body))
(defmacro are [argv expr & args]
  (cons 'do (map (fn [a] (list 'is (clojure.walk/postwalk-replace (zipmap argv a) expr)))
                 (partition (count argv) args))))
(defn merge-deep [& maps]
  (apply merge-with (fn [x y] (if (and (map? x) (map? y)) (merge-deep x y) y)) maps))\"))

(defn failures [body]
  (sci/eval-string (str prelude (pr-str (cons 'do body)) \" @failures\")))

(def upstream-tests
  '[
" (str/join "\n\n" (map (comp indent tests) test-names)) "
    ])

(deftest clojure-destructure-test
  (doseq [[_ test-name & body] upstream-tests]
    (testing (str test-name)
      (is (= [] (failures body))))))
"))
  (println "Wrote" out "from" sha))
