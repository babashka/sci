(ns sci.test-runner
  (:require [cljs.test]
            [sci.core-protocols-test]
            [sci.core-test]
            [sci.error-test]
            [sci.hierarchies-test]
            [sci.impl.analyzer-test]
            [sci.impl.binding-array-refactor-test]
            [sci.interop-test]
            [sci.io-test]
            [sci.js-test]
            [sci.multimethods-test]
            [sci.namespaces-test]
            [sci.parse-test]
            [sci.protocols-test]
            [sci.read-test]
            [sci.defrecords-and-defype-test]
            [sci.reify-test]
            [sci.repl-test]
            [sci.test-utils :refer [planck-env?]]
            [sci.vars-test]))

(defn exit
  "Exit with the given status."
  [status]
  (when-let
      [exit-fn
       (cond
         ;; node
         (exists? js/process)
         #(.exit js/process %)
         ;; nashorn
         (exists? js/exit)
         js/exit
         ;; planck
         (planck-env?)
         js/PLANCK_EXIT_WITH_VALUE)]
    (exit-fn status)))

(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]
  (if-not (cljs.test/successful? m)
    (exit 1)
    (exit 0)))

(defn -main []
  (cljs.test/run-tests
   'sci.core-protocols-test
   'sci.core-test
   'sci.error-test
   'sci.hierarchies-test
   'sci.interop-test
   'sci.io-test
   'sci.js-test
   'sci.multimethods-test
   'sci.namespaces-test
   'sci.parse-test
   'sci.protocols-test
   'sci.read-test
   'sci.records-test
   'sci.reify-test
   'sci.repl-test
   'sci.impl.analyzer-test
   'sci.impl.binding-array-refactor-test
   'sci.vars-test))
