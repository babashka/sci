(ns sci.addons
  {:no-doc true}
  (:refer-clojure :exclude [future pmap])
  (:require [sci.impl.macros :as macros]
            [sci.impl.vars :as vars]))

#?(:clj
   (def future* ^:sci/macro
     (fn [_ _ & body]
       `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
          (~'future-call f#)))))

(macros/deftime
  (defmacro future**
    "Like clojure.core/future but also conveys sci bindings to the thread."
    [& body]
    `(let [f# (-> (fn [] ~@body)
                  (vars/binding-conveyor-fn))]
       (future-call f#))))


#?(:clj (defn pmap
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
             (pmap #(apply f %) (step (cons coll colls)))))))

#?(:clj
   (defn future [opts]
     (update-in opts [:namespaces 'clojure.core]
                assoc
                'future future*
                'future-call future-call
                'future-cancel future-cancel
                'future-cancelled? future-cancelled?
                'future-done? future-done?
                'future? future?
                'pmap pmap
                'promise promise
                'deliver deliver)))