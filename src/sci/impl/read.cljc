(ns sci.impl.read
  {:no-doc true}
  (:refer-clojure :exclude [eval load-string read read-string read+string])
  (:require [clojure.tools.reader.reader-types :as r]
            [sci.impl.io :as io]
            [sci.impl.parser :as parser]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn read
  "Added for compatibility. Does not support all of the options from the original yet."
  ([sci-ctx]
   (read sci-ctx @io/in))
  ([sci-ctx stream]
   (read sci-ctx stream true nil))
  ([sci-ctx stream eof-error? eof-value]
   (read sci-ctx stream eof-error? eof-value false))
  ([sci-ctx stream _eof-error? eof-value _recursive?]
   (let [v (parser/parse-next sci-ctx stream {:eof eof-value})]
     (if (utils/kw-identical? parser/eof v)
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
          (if (utils/kw-identical? parser/eof x)
            ret
            (recur (eval sci-ctx x))))))))

;; used by source-fn
(defn source-logging-reader
  [x]
  #?(:clj (r/source-logging-push-back-reader (r/push-back-reader x))
     :cljs (let [string-reader (r/string-reader x)
                 buf-len 1
                 pushback-reader (r/PushbackReader. string-reader
                                                    (object-array buf-len)
                                                    buf-len buf-len)]
             (r/source-logging-push-back-reader pushback-reader))))
