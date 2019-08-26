(ns sci.impl.parser
  "This code is largely inspired by rewrite-clj(sc), so thanks to all
  who contribured to those projects."
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   #?(:clj  [sci.impl.toolsreader.v1v3v2.clojure.tools.reader.edn :as edn]
      :cljs [sci.impl.toolsreader.v1v3v2.cljs.tools.reader.edn :as edn])
   #?(:clj  [sci.impl.toolsreader.v1v3v2.clojure.tools.reader.reader-types :as r]
      :cljs [sci.impl.toolsreader.v1v3v2.cljs.tools.reader.reader-types :as r])
   [sci.impl.readers :as readers])
  #?(:clj (:import [java.io Closeable])))

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

(defn parse-sharp
  [ctx #?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore sharp
  (case (r/peek-char reader)
    nil (throw-reader reader "Unexpected EOF.")
    \{ (parse-to-delimiter ctx reader \} #{})
    \( (let [expr (parse-list ctx reader)]
         (readers/read-fn expr))
    \" (let [expr (edn/read reader)]
         (re-pattern expr))
    \^ (parse-next ctx reader)
    \_ (do (parse-next ctx reader) ;; ignore form
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
    \' (parse-quoted reader)
    (\} \] \)) (let [expected (:expected-delimiter ctx)]
                 (if (not= expected c)
                   (throw-reader reader
                                 (str "Unmatched delimiter: " c)
                                 ctx)
                   (do
                     (r/read-char reader) ;; read delimiter
                     ::expected-delimiter)))
    \; (parse-comment reader)
    \@ (parse-deref ctx reader)
    \# (parse-sharp ctx reader)
    (if (whitespace? c) (parse-whitespace ctx reader)
        (edn/read reader))))

(defn parse-next [ctx reader]
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
        ctx {:expected-delimiter nil}]
    (loop [ret (transient [])]
      (let [next-val (parse-next ctx r)]
        (if (#?(:clj identical? :cljs keyword-identical?) ::eof next-val)
          (persistent! ret)
          (recur (conj! ret next-val)))))))

;;;; Scratch

(comment
  (parse-string "{:a 1} {:a 2}")
  )
