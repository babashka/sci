(ns sci.impl.callstack
  (:refer-clojure :exclude [pop!])
  (:require [clojure.string :as str]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(defn sci-ns-name [^sci.impl.vars.SciNamespace ns]
  (vars/getName ns))

(defn select [m]
  (let [new-m (select-keys m [:ns :name :local-name :file :line :column :sci.impl/built-in :local])]
    new-m))

(defn expr->data [expr]
  (let [m (meta expr)
        f (first expr)
        fm (some-> f meta)
        fm (if (symbol? f)
             (assoc fm
                    :local-name f
                    :local true
                    :ns (:ns m))
             fm)]
    [(select m) (select fm)]))

(defn stacktrace [callstack]
  (let [callstack @callstack
        data (mapcat expr->data callstack)
        data (reduce (fn [[acc last-file last-ns last-name] entry]
                       (let [new-last-name (or (:name entry)
                                               last-name)
                             new-last-file (or (:file entry)
                                               last-file)
                             new-entry (if (identical? last-ns (:ns entry))
                                         (assoc entry
                                                :name new-last-name
                                                :file new-last-file)
                                         entry)]
                         [(conj acc new-entry)
                          new-last-file
                          (:ns entry)
                          new-last-name]))
                     (let [fd (first data)]
                       ['() (:file fd) (:ns fd) (:name fd)])
                     data)]
    (first data)))

(defn right-pad [s n]
  (let [n (- n (count s))]
    (str s (str/join (repeat n " ")))))

(defn format-stacktrace [st]
  (let [data (map (fn [elt]
                    {:name (str (if-let [nom (:name elt)]
                                  (str (:ns elt) "/" nom)
                                  (:ns elt))
                                (when (:local elt)
                                  (str "#" (:local-name elt))))
                     :loc (str (or (:file elt)
                                   (if (:sci.impl/built-in elt)
                                     "<built-in>"
                                     "<expr>"))
                               (when-let [l (:line elt)]
                                 (str ":" l ":" (:column elt))))})
                  st)
        max-name (reduce max 0 (map (comp count :name) data))
        max-loc (reduce max 0 (map (comp count :loc) data))]
    (map (fn [{:keys [:name :loc]}]
           (str (right-pad name max-name)
                " - "
                (right-pad loc max-loc)))
         data)))
