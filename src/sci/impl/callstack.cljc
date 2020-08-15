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

(defn var->data [var]
  (let [m (meta var)
        fm (some-> var first meta)
        nom (or (:name fm)
                (:fn-name fm))]
    (when nom
      (assoc m
             :name nom
             :ns (or (:ns fm)
                     (:ns m))))))

(defn stacktrace [callstack]
  (keep var->data callstack))

(defn print-stacktrace [st]
  (doseq [elt st]
    (prn elt))
  ;; #?(:clj (doseq [elt st]
  ;;           (println "  "
  ;;            (str
  ;;             (format "%s/%s"
  ;;                     (:ns elt)
  ;;                     (:name elt))
  ;;             (format " - %s"
  ;;                     (or (:file elt)
  ;;                         (if (:sci.impl/built-in elt)
  ;;                           "<built-in>"
  ;;                           "<expr>")))
  ;;             (when-let [l (:line elt)]
  ;;               (format ":%s:%s"
  ;;                       l
  ;;                       (:column elt)))))))
  )
