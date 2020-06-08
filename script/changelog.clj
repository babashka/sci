#!/usr/bin/env bb

(ns changelog
  (:require [clojure.string :as str]))

(let [changelog (slurp "CHANGELOG.md")
      replaced (str/replace changelog
                            #" #(\d+)"
                            (fn [[_ issue]]
                              (format " [#%s](https://github.com/borkdude/babashka/issues/%s)"
                                      issue issue)))
      replaced (str/replace replaced
                            #"\(@(\w+)\)"
                            (fn [[_ name]]
                              (format "([@%s](https://github.com/%s))"
                                      name name)))]
  (spit "CHANGELOG.md" replaced))
