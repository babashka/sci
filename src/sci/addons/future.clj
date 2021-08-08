(ns sci.addons.future
  {:no-doc true}
  (:refer-clojure :exclude [future pmap])
  (:require [sci.impl.namespaces :refer [copy-core-var core-var]])
  (:require [sci.impl.vars :as vars]))

(def future* ^{:sci/macro true
               :ns vars/clojure-core-ns}
  (fn [_ _ & body]
    `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
       (~'future-call f#))))

(defmacro future**
  "Like clojure.core/future but also conveys sci bindings to the thread."
  [& body]
  `(let [f# (-> (fn [] ~@body)
                (vars/binding-conveyor-fn))]
     (future-call f#)))

(defn pmap
  "Like clojure.core/pmap but also conveys sci bindings to the threads."
  ([f coll]
   (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
         rets (map #(future** (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (pmap #(apply f %) (step (cons coll colls))))))

(defn install [opts]
  (update-in opts [:namespaces 'clojure.core]
             assoc
             'future future*
             'future-call (copy-core-var future-call)
             'future-cancel (copy-core-var future-cancel)
             'future-cancelled? (copy-core-var future-cancelled?)
             'future-done? (copy-core-var future-done?)
             'future? (copy-core-var future?)
             'pmap (core-var 'pmap pmap)))
