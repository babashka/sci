(ns sci.impl.callstack
  (:refer-clojure :exclude [pop!])
  (:require [sci.impl.vars :as vars])
  #?(:clj (:import [java.util LinkedList])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (def ^ThreadLocal callstack (proxy [ThreadLocal] []
                                 (initialValue [] (LinkedList.))))
   :cljs
   (def callstack (atom [])))

(defn get-callstack []
  #?(:clj (.get callstack)
     :cljs @callstack))

(defn push! [data]
  #?(:clj (let [^LinkedList cs (.get callstack)]
            (.push cs data))
     :cljs (swap! callstack conj data)))

(defn pop! []
  #?(:clj (let [^LinkedList cs (.get callstack)]
            (.pop cs))
     :cljs (swap! callstack pop)))

(defn sci-ns-name [^sci.impl.vars.SciNamespace ns]
  (vars/getName ns))

(defn select [m]
  (select-keys m [:ns :name :file :line :column :sci.impl/built-in]))

(defn expr->data [expr]
  (let [m (meta expr)
        m (assoc m :file @vars/current-file)
        f (first expr)
        fm (some-> f meta)]
    [(select m) (select fm)]))

(defn stacktrace [callstack]
  (let [data (mapcat expr->data callstack)
        data (reduce (fn [[acc last-ns last-name] entry]
                       (let [new-last-name (or (:name entry)
                                               last-name)
                             new-entry (if (identical? last-ns (:ns entry))
                                         (assoc entry :name new-last-name)
                                         entry)]
                         #_(when-not (= entry new-entry)
                           (prn entry '-> new-entry))
                         [(conj acc new-entry)
                          (:ns entry)
                          new-last-name]))
                     (let [fd (first data)]
                       ['() (:ns fd) (:name fd)])
                     data)]
    (first data)))

(defn print-stacktrace [st]
  #?(:cljs (doseq [elt st]
             (prn elt))
     :clj (doseq [elt st]
            (println "  "
                     (str
                      (if-let [nom (:name elt)]
                        (format "%s/%s"
                                (:ns elt)
                                nom)
                        (:ns elt))
                      (format " - %s"
                              (or (:file elt)
                                  (if (:sci.impl/built-in elt)
                                    "<built-in>"
                                    "<expr>")))
                      (when-let [l (:line elt)]
                        (format ":%s:%s"
                                l
                                (:column elt))))))))
