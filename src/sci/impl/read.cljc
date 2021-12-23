(ns sci.impl.read
  {:no-doc true}
  (:refer-clojure :exclude [eval load-string read read-string read+string])
  (:require [clojure.tools.reader.reader-types :as r]
            [sci.impl.io :as io]
            [sci.impl.parser :as parser]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn- eof-or-throw [opts v]
  (if (utils/kw-identical? parser/eof v)
    (if-let [eof (:eof opts)]
      (if (not (utils/kw-identical? :eofthrow eof))
        eof
        (throw (ex-info "EOF while reading"
                        {:type :sci.error/parse
                         :opts opts})))
      (throw (ex-info "EOF while reading"
                      {:type :sci.error/parse
                       :opts opts})))
    v))


(defn with-resolver [opts]
  #?(:clj (if-let [^clojure.lang.LispReader$Resolver resolver
                   @parser/reader-resolver]
            (assoc opts :auto-resolve
                   (fn [alias]
                     (if (= :current alias)
                       (let [c (.currentNS ^clojure.lang.LispReader$Resolver resolver)]
                         c)
                       (.resolveAlias ^clojure.lang.LispReader$Resolver resolver alias))))
            opts)
     :cljs opts))

(defn read
  ([sci-ctx]
   (read sci-ctx @io/in))
  ([sci-ctx stream]
   (read sci-ctx stream true nil))
  ([sci-ctx stream eof-error? eof-value]
   (read sci-ctx stream eof-error? eof-value false))
  ([sci-ctx stream _eof-error? eof-value _recursive?]
   (let [v (parser/parse-next sci-ctx stream
                              (-> {:eof eof-value}
                                  (with-resolver)))]
     (eof-or-throw {:eof eof-value} v)))
  ([sci-ctx opts stream]
   (let [opts (with-resolver opts)
         opts (if (:read-cond opts)
                ;; always prioritize platform feature
                (assoc opts :features (into #?(:clj #{:clj}
                                               :cljs #{:cljs})
                                            (:features opts)))
                opts)
         v (parser/parse-next sci-ctx stream opts)]
     (eof-or-throw opts v))))

(defn read-string
  ([sci-ctx s]
   (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
     (read sci-ctx reader)))
  ([sci-ctx opts s]
   (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
     (read sci-ctx opts reader))))

(defn load-string [sci-ctx s]
  (vars/with-bindings {vars/current-ns @vars/current-ns}
    (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
      (loop [ret nil]
        (let [x (parser/parse-next sci-ctx reader)]
          (if (utils/kw-identical? parser/eof x)
            ret
            (recur (utils/eval sci-ctx x))))))))

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
