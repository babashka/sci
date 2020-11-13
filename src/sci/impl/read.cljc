(ns sci.impl.read
  (:refer-clojure :exclude [eval load-string read read-string])
  (:require [clojure.tools.reader.reader-types :as r]
            [sci.impl.io :as io]
            [sci.impl.parser :as parser]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn read
  "Added for compatibility. Does not support the options from the original yet."
  ([sci-ctx]
   (read sci-ctx @io/in))
  ([sci-ctx stream]
   (read sci-ctx stream true nil))
  ([sci-ctx stream eof-error? eof-value]
   (read sci-ctx stream eof-error? eof-value false))
  ([sci-ctx stream _eof-error? eof-value _recursive?]
   (let [v (parser/parse-next sci-ctx stream {:eof eof-value})]
     (if (utils/kw-identical? :edamame.impl.parser/eof v)
       eof-value
       v)))
  ([sci-ctx _opts stream]
   (parser/parse-next sci-ctx stream)))

(defn read-string
  ([sci-ctx s]
   (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
     (parser/parse-next sci-ctx reader))))

(defn eval [sci-ctx form]
  (@utils/eval-form-state sci-ctx form))

(defn load-string [sci-ctx s]
  (vars/with-bindings {vars/current-ns @vars/current-ns}
    (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
      (loop [ret nil]
        (let [x (parser/parse-next sci-ctx reader)]
          (if (utils/kw-identical? :edamame.impl.parser/eof x)
            ret
            (recur (eval sci-ctx x))))))))
