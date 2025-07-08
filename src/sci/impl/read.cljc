(ns sci.impl.read
  {:no-doc true}
  (:refer-clojure :exclude [eval load-string load-reader read read-string read+string])
  (:require [clojure.tools.reader.reader-types :as r]
            [sci.ctx-store :as store]
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

(defn with-suppressed [opts]
  (if @parser/suppress-read (assoc opts :suppress-read true)
      opts))

(defn read
  ([]
   (read @io/in))
  ([stream]
   (read stream true nil))
  ([stream eof-error? eof-value]
   (read stream eof-error? eof-value false))
  ([stream _eof-error? eof-value _recursive?]
   (let [v (parser/parse-next (store/get-ctx) stream
                              (-> {:eof eof-value}
                                  (with-resolver)
                                  (with-suppressed)))]
     (eof-or-throw {:eof eof-value} v)))
  ([opts stream]
   (let [opts (-> opts with-resolver with-suppressed)
         opts (if (:read-cond opts)
                ;; always prioritize platform feature
                (assoc opts :features (into #?(:clj #{:clj}
                                               :cljs #{:cljs})
                                            (:features opts)))
                opts)
         v (parser/parse-next (store/get-ctx) stream opts)]
     (eof-or-throw opts v))))

(defn read-string
  ([s]
   (let [reader (r/string-push-back-reader s)]
     (read reader)))
  ([opts s]
   (let [reader (r/string-push-back-reader s)]
     (read opts reader))))

(defn load-reader [reader]
  (vars/with-bindings {utils/current-ns @utils/current-ns}
    (let [ctx (store/get-ctx)]
      (loop [ret nil]
        (let [x (parser/parse-next ctx reader)]
          (if (utils/kw-identical? parser/eof x)
            ret
            (recur (utils/eval ctx x))))))))

(defn load-string [s]
  (let [rdr (r/indexing-push-back-reader (r/string-push-back-reader s))]
    (load-reader rdr)))

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
