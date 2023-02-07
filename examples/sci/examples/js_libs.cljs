(ns sci.examples.js-libs
  (:require ["fs" :as fs]
            [sci.core :as sci]))

(sci/eval-string "
(require '[\"fs\" :as fs])
(fs/existsSync \"README.md\")"
                 {:js-libs {"fs" fs}})
;;=> true
