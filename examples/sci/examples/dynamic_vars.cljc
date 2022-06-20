(ns sci.examples.dynamic-vars
  (:require [sci.core :as sci]))

(def ^:dynamic *items* 10)

(defn foo [] (+ *items* 12))

(def mns (sci/create-ns 'my.namespace nil))

(def items (sci/copy-var *items* mns))

(defn sci-foo []
  (binding [*items* @items]
    (foo)))

(def ctx
  (sci/init {:namespaces
             {'my.namespace {'foo (sci/copy-var sci-foo mns {:name 'foo})
                             '*items* items}}}))

(prn (sci/eval-string* ctx "
(require '[my.namespace :as m])
(binding [m/*items* 1337] (m/foo))"))
;; => 1349
