(ns sci.impl.callstack
  {:no-doc true}
  (:require [clojure.string :as str]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(defn sci-ns-name [^sci.impl.vars.SciNamespace ns]
  (vars/getName ns))

(defn select [m]
  (let [new-m (select-keys m [:ns :name :local-name :file :line :column
                              :sci.impl/built-in :local :macro])]
    new-m))

(defn expr->data [expr]
  (let [m (meta expr)
        f (when (seqable? expr) (first expr))
        fm (or (:sci.impl/f-meta m)
               (some-> f meta))
        fm (if (symbol? f)
             (assoc fm
                    :local-name f
                    :local true
                    :ns (:ns m)
                    :macro (or (:sci/macro fm)
                               (:macro fm)))
             fm)]
    (filter not-empty [(select m) (select fm)])))

(defn stacktrace [callstack]
  (let [callstack @callstack
        callstack (dedupe callstack)
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

(defn phase [ex stacktrace]
  (or (:phase (ex-data ex))
      (when (some :macro stacktrace)
        "macroexpand")))

(defn right-pad [s n]
  (let [n (- n (count s))]
    (str s (str/join (repeat n " ")))))

(defn format-stacktrace [st]
  (let [data (keep (fn [{:keys [:file :ns :line :column :sci.impl/built-in
                                :local :local-name]
                         nom :name}]
                     (when (or line built-in)
                       {:name (str (if nom
                                     (str ns "/" nom)
                                     ns)
                                   (when local
                                     (str "#" local-name)))
                        :loc (str (or file
                                      (if built-in
                                        "<built-in>"
                                        "<expr>"))
                                  (when line
                                    (str ":" line ":" column)))}))
                   st)
        max-name (reduce max 0 (map (comp count :name) data))]
    (map (fn [{:keys [:name :loc]}]
           (str (right-pad name max-name)
                " - "
                loc))
         data)))
