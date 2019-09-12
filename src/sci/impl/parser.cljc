(ns sci.impl.parser
  "This code is largely inspired by rewrite-clj(sc), so thanks to all
  who contribured to those projects."
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   #?(:clj  [clojure.tools.reader.edn :as edn]
      :cljs [cljs.tools.reader.edn :as edn])
   #?(:clj  [clojure.tools.reader.reader-types :as r]
      :cljs [cljs.tools.reader.reader-types :as r])
   [sci.impl.readers :as readers])
  #?(:clj (:import [java.io Closeable]
                   [java.util.regex Pattern]))
  #?(:cljs (:import [goog.string StringBuffer])))

#?(:clj (set! *warn-on-reflection* true))

(declare parse-next)

(defn whitespace?
  [#?(:clj ^java.lang.Character c :default c)]
  #?(:clj (and c (or (= c \,) (Character/isWhitespace c)))
     :cljs (and c (< -1 (.indexOf #js [\return \newline \tab \space ","] c)))))

(defn location [#?(:cljs ^not-native reader :default reader)]
  {:row (r/get-line-number reader)
   :col (r/get-column-number reader)})

(defn parse-whitespace
  [_ctx #?(:cljs ^not-native reader :default reader)]
  (loop []
    (let [c (r/read-char reader)]
      (if (whitespace? c)
        (recur)
        (do (r/unread reader c)
            reader)))))

(defn parse-quoted
  [#?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore quote
  (list 'quote (edn/read reader)))

(defn parse-comment
  [#?(:cljs ^not-native reader :default reader)]
  (r/read-line reader)
  reader)

(defn parse-deref
  [ctx #?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore leading @
  (list 'deref (parse-next ctx reader)))

(defn parse-to-delimiter
  ([ctx #?(:cljs ^not-native reader :default reader) delimiter]
   (parse-to-delimiter ctx reader delimiter []))
  ([ctx #?(:cljs ^not-native reader :default reader) delimiter into]
   (r/read-char reader) ;; ignore delimiter
   (let [ctx (assoc ctx :expected-delimiter delimiter)]
     (loop [vals (transient into)]
       (let [next-val (parse-next ctx reader)]
         (if (#?(:clj identical? :cljs keyword-identical?) ::expected-delimiter next-val)
           (persistent! vals)
           (recur (conj! vals next-val))))))))

(defn parse-list [ctx #?(:cljs ^not-native reader :default reader)]
  (apply list (parse-to-delimiter ctx reader \))))

(defn throw-reader
  "Throw reader exception, including line line/column."
  ([#?(:cljs ^:not-native reader :default reader) msg]
   (throw-reader reader msg nil))
  ([#?(:cljs ^:not-native reader :default reader) msg data]
   (let [c (r/get-column-number reader)
         l (r/get-line-number reader)]
     (throw
      (ex-info
       (str msg
            " [at line " l ", column " c "]")
       (merge {:row l, :col c} data))))))

(defn read-regex
  "Modeled after tools.reader/read-regex."
  [_ctx #?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore leading double-quote
  (let [sb #?(:clj (StringBuilder.)
              :cljs (goog.string.StringBuffer.))]
    (loop [ch (r/read-char reader)]
      (if (identical? \" ch)
        #?(:clj (Pattern/compile (str sb))
           :cljs (re-pattern (str sb)))
        (if (nil? ch)
          (throw-reader reader "Error while parsing regex")
          (do
            (.append sb ch )
            (when (identical? \\ ch)
              (let [ch (r/read-char reader)]
                (when (nil? ch)
                  (throw-reader reader "Error while parsing regex"))
                (.append sb ch)))
            (recur (r/read-char reader))))))))

(defn parse-sharp
  [ctx #?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore sharp
  (case (r/peek-char reader)
    nil (throw-reader reader "Unexpected EOF.")
    \{ (parse-to-delimiter ctx reader \} #{})
    \( (let [expr (parse-list ctx reader)]
         (readers/read-fn expr))
    \" (read-regex ctx reader)
    \^ (parse-next ctx reader)
    \_ (do (r/read-char reader) ;; ignore underscore
           (parse-next ctx reader) ;; ignore form
           (parse-next ctx reader))
    ;; \: ;; TODO, namespaced map
    ;; \? ;; TODO, reader conditionals
    ))

(defn dispatch
  [ctx #?(:cljs ^not-native reader :default reader) c]
  (case c
    nil ::eof
    \( (parse-list ctx reader)
    \[ (parse-to-delimiter ctx reader \])
    \{ (apply hash-map (parse-to-delimiter ctx reader \}))
    (\} \] \)) (let [expected (:expected-delimiter ctx)]
                 (if (not= expected c)
                   (throw-reader reader
                                 (str "Unmatched delimiter: " c)
                                 ctx)
                   (do
                     (r/read-char reader) ;; read delimiter
                     ::expected-delimiter)))
    \' (parse-quoted reader)
    \; (parse-comment reader)
    \@ (parse-deref ctx reader)
    \# (parse-sharp ctx reader)
    (edn/read reader)))

(defn parse-next [ctx reader]
  (parse-whitespace ctx reader) ;; skip leading whitespace
  (let [c (r/peek-char reader)
        loc (location reader)
        obj (dispatch ctx reader c)]
    (if (identical? reader obj)
      (parse-next ctx reader)
      (if #?(:clj
             (instance? clojure.lang.IObj obj)
             :cljs (satisfies? IWithMeta obj))
        (vary-meta obj merge loc)
        obj))))

(defn string-reader
  "Create reader for strings."
  [s]
  (r/indexing-push-back-reader
   (r/string-push-back-reader s)))

(defn parse-string [s]
  (let [^Closeable r (string-reader s)
        ctx {:expected-delimiter nil}]
    (parse-next ctx r)))

(defn parse-string-all [s]
  (let [^Closeable r (string-reader s)
        ctx {:expected-delimiter nil}
        ret (loop [ret (transient [])]
              (let [next-val (parse-next ctx r)]
                (if (#?(:clj identical? :cljs keyword-identical?) ::eof next-val)
                  (persistent! ret)
                  (recur (conj! ret next-val)))))]
    ret))

;;;; Scratch

(comment
  (parse-string "{:a 1} {:a 2}")
  )
