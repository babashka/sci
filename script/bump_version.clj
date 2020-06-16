#!/usr/bin/env bb

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(def version-file (io/file "resources" "SCI_VERSION"))
(def version (str/trim (slurp version-file)))

(defn next-version [version]
  (if (str/ends-with? version "-SNAPSHOT")
    (str/replace version "-SNAPSHOT" "")
    (let [[major minor patch] (str/split version #"\.")
          patch (Integer. patch)
          patch (inc patch)]
      (str (str/join "." [major minor patch])
           "-SNAPSHOT"))))

(spit version-file (next-version version))
