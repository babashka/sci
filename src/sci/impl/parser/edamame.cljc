(ns sci.impl.parser.edamame
  "This code is largely inspired by rewrite-clj(sc), so thanks to all
  who contribured to those projects."
  {:no-doc true}
  (:require
   #?(:clj  [clojure.tools.reader.edn :as edn]
      :cljs [cljs.tools.reader.edn :as edn])
   #?(:clj  [clojure.tools.reader.reader-types :as r]
      :cljs [cljs.tools.reader.reader-types :as r])
   #?(:clj  [clojure.tools.reader.impl.inspect :as i]
      :cljs [cljs.tools.reader.impl.inspect :as i])
   #?(:clj [clojure.tools.reader.impl.utils :refer [namespace-keys]]
      :cljs [cljs.tools.reader.impl.utils :refer [reader-conditional namespace-keys]])
   #?(:clj [clojure.tools.reader.impl.commons :as commons]
      :cljs [cljs.tools.reader.impl.commons :as commons])
   #?(:cljs [cljs.reader :refer [*tag-table*]])
   [clojure.string :as str]
   [edamame.impl.read-fn :refer [read-fn]]
   [sci.impl.parser.edamame.syntax-quote :refer [syntax-quote]])
  #?(:clj (:import [java.io Closeable]
                   [clojure.tools.reader.reader_types SourceLoggingPushbackReader]))
  #?(:cljs (:import [goog.string StringBuffer])))

#?(:clj (set! *warn-on-reflection* true))

;;;; tools.reader

;; This is used for reading tokens (numbers, strings, symbols, chars,
;; ##Inf). We're falling back on the EDN reader to handle this. Tried inlining
;; it, but saw no significant performance gain.
(defn edn-read [ctx #?(:cljs ^not-native reader :default reader)]
  (let [tools-reader-opts (:tools.reader/opts ctx)]
    (edn/read tools-reader-opts reader)))

(defn dispatch-macro? [ch]
  (contains? #{\^  ;; deprecated
               \'
               \(
               \{
               \"
               \!
               \_
               \?
               \:
               \#} ch))

(def read-token #'edn/read-token)
(def parse-symbol #'commons/parse-symbol)

;;;; end tools.reader

(defrecord Loc [line column])

(defn location [#?(:cljs ^not-native reader :default reader)]
  (->Loc
   (r/get-line-number reader)
   (r/get-column-number reader)))

(defn kw-identical? [kw v]
  (#?(:clj identical? :cljs keyword-identical?) kw v))

(declare parse-next)

(defn parse-comment
  [#?(:cljs ^not-native reader :default reader)]
  (r/read-line reader)
  reader)

#?(:cljs
   (defn whitespace?
     [c]
     (and c (< -1 (.indexOf #js [\return \newline \tab \space ","] c)))))

#?(:clj
   (defmacro whitespace? [c]
     `(and ~c (or (identical? ~c \,)
                  (Character/isWhitespace ~(with-meta c
                                             {:tag 'java.lang.Character}))))))


(defn skip-whitespace
  "Skips whitespace. Returns reader. If end of stream is reached, returns nil."
  [_ctx #?(:cljs ^not-native reader :default reader)]
  (loop []
    (when-let [c (r/read-char reader)]
      (if (whitespace? c)
        (recur)
        (do (r/unread reader c)
            reader)))))

(defn throw-reader
  "Throw reader exception, including line line/column. line/column is
  read from the reader but it can be overriden by passing loc
  optional parameter."
  ([ctx #?(:cljs ^:not-native reader :default reader) msg]
   (throw-reader ctx reader msg nil))
  ([ctx #?(:cljs ^:not-native reader :default reader) msg data]
   (throw-reader ctx reader msg data nil))
  ([_ctx #?(:cljs ^:not-native reader :default reader) msg data loc]
   (let [c (:column loc (r/get-column-number reader))
         l (:line loc (r/get-line-number reader))]
     (throw
      (ex-info msg
               (merge {:type :edamame/error
                       :line l
                       :column c} data))))))

(def non-match (symbol "non-match"))

(defn non-match? [v]
  (identical? v non-match))

(defn throw-eof-while-reading [ctx reader]
  (throw-reader ctx reader "EOF while reading"))

(defn parse-to-delimiter
  ([ctx #?(:cljs ^not-native reader :default reader) delimiter]
   (parse-to-delimiter ctx reader delimiter []))
  ([ctx #?(:cljs ^not-native reader :default reader) delimiter into]
   (let [line (r/get-line-number reader)
         column (r/get-column-number reader)
         opened (r/read-char reader)
         ctx (-> ctx
                 (assoc ::expected-delimiter delimiter)
                 (assoc ::opened-delimiter {:char opened :line line :column column}))]
     (loop [vals (transient into)]
       (let [;; if next-val is uneval, we get back the expected delimiter...
             next-val (parse-next ctx reader)
             cond-splice? (some-> next-val meta ::cond-splice)]
         (cond
           (kw-identical? ::eof next-val)
           (throw-reader ctx
                         reader
                         (str "EOF while reading, expected " delimiter " to match " opened " at [" line "," column "]")
                         {:edamame/expected-delimiter (str delimiter)
                          :edamame/opened-delimiter (str opened)})
           (kw-identical? ::expected-delimiter next-val)
           (persistent! vals)
           cond-splice? (do (doseq [v next-val]
                              (conj! vals v))
                            (recur vals))
           (non-match? next-val) (recur vals)
           :else
           (recur (conj! vals next-val))))))))

(defn parse-list [ctx #?(:cljs ^not-native reader :default reader)]
  (apply list (parse-to-delimiter ctx reader \))))

(defn read-regex-pattern
  "Modeled after tools.reader/read-regex."
  [ctx #?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore leading double-quote
  (let [sb #?(:clj (StringBuilder.)
              :cljs (StringBuffer.))]
    (loop [ch (r/read-char reader)]
      (if (identical? \" ch)
        #?(:clj (str sb)
           :cljs (str sb))
        (if (nil? ch)
          (throw-reader ctx reader "Error while parsing regex")
          (do
            (.append sb ch )
            (when (identical? \\ ch)
              (let [ch (r/read-char reader)]
                (when (nil? ch)
                  (throw-reader ctx reader "Error while parsing regex"))
                (.append sb ch)))
            (recur (r/read-char reader))))))))

(defn- duplicate-keys-error [msg coll]
  ;; https://github.com/clojure/tools.reader/blob/97d5dac9f5e7c04d8fe6c4a52cd77d6ced560d76/src/main/cljs/cljs/tools/reader/impl/errors.cljs#L233
  (letfn [(duplicates [seq]
            (for [[id freq] (frequencies seq)
                  :when (> freq 1)]
              id))]
    (let [dups (duplicates coll)]
      (apply str msg
             (when (> (count dups) 1) "s")
             ": " (interpose ", " dups)))))

(defn throw-dup-keys
  [ctx #?(:cljs ^not-native reader :default reader) loc kind ks]
  (throw-reader
   ctx reader
   (duplicate-keys-error
    (str (str/capitalize (name kind)) " literal contains duplicate key")
    ks)
   nil
   loc))

(defn parse-set
  [ctx #?(:cljs ^not-native reader :default reader)]
  (let [start-loc (location reader)
        coll (parse-to-delimiter ctx reader \})
        the-set (set coll)]
    (when-not (= (count coll) (count the-set))
      (throw-dup-keys ctx reader start-loc :set coll))
    the-set))

(defn parse-first-matching-condition [ctx #?(:cljs ^not-native reader :default reader)]
  (let [features (:features ctx)]
    (loop [match non-match]
      (let [k (parse-next ctx reader)]
        (if (kw-identical? k ::expected-delimiter)
          match
          (let [next-is-match? (and (non-match? match)
                                    (or (contains? features k)
                                        (kw-identical? k :default)))]
            (if next-is-match?
              (let [match (parse-next ctx reader)
                    ctx (assoc ctx ::suppress true)]
                (loop []
                  (let [next-val (parse-next ctx reader)]
                    (when-not (kw-identical? ::expected-delimiter
                                             next-val)
                      (if (kw-identical? ::eof next-val)
                        (let [delimiter (::expected-delimiter ctx)
                              {:keys [:line :column :char]} (::opened-delimiter ctx)]
                          (throw-reader ctx
                                        reader
                                        (str "EOF while reading, expected " delimiter " to match " char " at [" line "," column "]")
                                        {:edamame/expected-delimiter (str delimiter)
                                         :edamame/opened-delimiter (str char)}))
                        (recur)))))
                match)
              (do
                ;; skip over next val and try next key
                (parse-next (assoc ctx ::suppress true)
                            reader)
                (recur match)))))))))

(defn parse-reader-conditional [ctx #?(:cljs ^not-native reader :default reader)]
  (skip-whitespace ctx reader)
  (let [opt (:read-cond ctx)
        splice? (= \@ (r/peek-char reader))]
    (when splice? (r/read-char reader))
    (skip-whitespace ctx reader)
    (cond (kw-identical? :preserve opt)
          (reader-conditional (parse-next ctx reader) splice?)
          (fn? opt)
          (opt (vary-meta
                (parse-next ctx reader)
                assoc :edamame/read-cond-splicing splice?))
          :else
          (let [line (r/get-line-number reader)
                column (r/get-column-number reader)
                opened (r/read-char reader)
                ctx (-> ctx
                        (assoc ::expected-delimiter \))
                        (assoc ::opened-delimiter {:char opened :line line :column column}))
                match (parse-first-matching-condition ctx reader)]
            (cond (non-match? match) reader
                  splice? (vary-meta match
                                     #(assoc % ::cond-splice true))
                  :else match)))))

(defn get-auto-resolve
  ([ctx reader next-val]
   (get-auto-resolve ctx reader next-val nil))
  ([ctx reader next-val msg]
   (if-let [v (:auto-resolve ctx)]
     v
     (throw-reader ctx reader
                   (or msg "Use `:auto-resolve` to resolve aliases.")
                   {:expr (str ":" next-val)}))))

(defn auto-resolve
  "Returns namespace for given alias."
  ([ctx m kns reader next-val] (auto-resolve ctx m kns reader next-val nil))
  ([ctx m kns reader next-val msg]
   (if-let [kns (m kns)]
     kns
     (throw-reader ctx reader
                   (or msg (str "Alias `" (symbol kns) "` not found in `:auto-resolve`"))
                   {:expr (str ":" next-val)}))))

(defn parse-namespaced-map [ctx #?(:cljs ^not-native reader :default reader)]
  (let [auto-resolved? (when (identical? \: (r/peek-char reader))
                         (r/read-char reader)
                         true)
        current-ns? (when auto-resolved?
                      (identical? \{ (r/peek-char reader)))
        prefix (if auto-resolved?
                 (when-not current-ns?
                   (edn-read ctx reader))
                 (edn-read ctx reader))
        the-map (parse-next ctx reader)]
    (if auto-resolved?
      (let [ns (if current-ns? :current (symbol (name prefix)))
            f (get-auto-resolve ctx reader ns)
            resolved-ns (auto-resolve ctx f ns reader prefix)]
        (zipmap (namespace-keys (str resolved-ns) (keys the-map))
                (vals the-map)))
      (let [resolved-ns (name prefix)]
        (zipmap (namespace-keys resolved-ns (keys the-map))
                (vals the-map))))))

(defn parse-sharp
  [ctx #?(:cljs ^not-native reader :default reader)]
  (let [c (r/peek-char reader)]
    (case c
      nil (throw-reader ctx reader (str "Unexpected EOF."))
      \" (let [pat (read-regex-pattern ctx reader)]
           (re-pattern pat))
      \( (let [fn-expr (parse-next ctx reader)]
           (read-fn fn-expr))
      \' (do
           (r/read-char reader) ;; ignore quote
           (let [next-val (parse-next ctx reader)]
             (when (kw-identical? ::eof next-val)
               (throw-eof-while-reading ctx reader))
             (list 'var next-val)))
      \= (do
           (r/read-char reader) ;; ignore =
           (let [next-val (parse-next ctx reader)]
             (list 'read-eval next-val)))
      \{ (parse-set ctx reader)
      \_ (do
           (r/read-char reader) ;; read _
           (parse-next ctx reader) ;; ignore next form
           reader)
      \? (do
           (when-not (:read-cond ctx)
             (throw-reader
              ctx reader
              (str "Conditional read not allowed.")))
           (r/read-char reader) ;; ignore ?
           (parse-reader-conditional ctx reader))
      \: (do
           (r/read-char reader) ;; ignore :
           (parse-namespaced-map ctx reader))
      \! (do
           (parse-comment reader)
           reader)
      ;; catch-all
      (if (dispatch-macro? c)
        (do (r/unread reader \#)
            (edn-read ctx reader))
        ;; reader tag
        (let [suppress? (::suppress ctx)]
          (if suppress?
            (do
              ;; read symbol
              (parse-next ctx reader)
              ;; read form
              (parse-next ctx reader))
            (let [sym (parse-next ctx reader)
                  data (parse-next ctx reader)
                  f (or (when-let [readers (:readers ctx)]
                          (readers sym))
                        #?(:clj (default-data-readers sym)
                           :cljs (@*tag-table* sym)))]
              (if f (f data)
                  (throw (new #?(:clj Exception :cljs js/Error)
                              (str "No reader function for tag " sym)))))))))))

(defn throw-odd-map
  [ctx #?(:cljs ^not-native reader :default reader) loc elements]
  (throw-reader ctx reader
                (str
                 "The map literal starting with "
                 (i/inspect (first elements))
                 " contains "
                 (count elements)
                 " form(s). Map literals must contain an even number of forms.")
                nil
                loc))

(defn parse-map
  [ctx #?(:cljs ^not-native reader :default reader)]
  (let [start-loc (location reader)
        elements (parse-to-delimiter ctx reader \})
        c (count elements)]
    (when (pos? c)
      (when (odd? c)
        (throw-odd-map ctx reader start-loc elements))
      (let [ks (take-nth 2 elements)]
        (when-not (apply distinct? ks)
          (throw-dup-keys ctx reader start-loc :map ks))))
    (apply hash-map elements)))

(defn parse-keyword [ctx #?(:cljs ^not-native reader :default reader)]
  (r/read-char reader) ;; ignore :
  (let [init-c (r/read-char reader)]
    (when (whitespace? init-c)
      (throw-reader ctx reader (str "Invalid token: :")))
    (let [^String token (read-token reader :keyword init-c)
          auto-resolve? (identical? \: (.charAt token 0))]
      (if auto-resolve?
        (let [token (if auto-resolve? (subs token 1) token)
              [token-ns token-name] (parse-symbol token)]
          (if token-ns
            (let [f (get-auto-resolve ctx reader token)
                  kns (auto-resolve ctx f (symbol token-ns) reader token-ns)]
              (keyword (str kns) token-name))
            ;; resolve current ns
            (let [f (get-auto-resolve ctx reader token "Use `:auto-resolve` + `:current` to resolve current namespace.")
                  kns (auto-resolve ctx f :current reader token "Use `:auto-resolve` + `:current` to resolve current namespace.")]
              (keyword (str kns) token-name))))
        (keyword token)))))

(defn dispatch
  [ctx #?(:cljs ^not-native reader :default reader) c]
  (let [sharp? (= \# c)]
    (if sharp? (do
                 (r/read-char reader) ;; ignore sharp
                 (parse-sharp ctx reader))
        (case c
          nil ::eof
          \@ (do (r/read-char reader) ;; skip @
                 (let [next-val (parse-next ctx reader)]
                   (list 'clojure.core/deref next-val)))
          \' (do
               (r/read-char reader) ;; skip '
               (let [next-val (parse-next ctx reader)]
                 (when (kw-identical? ::eof next-val)
                   (throw-eof-while-reading ctx reader))
                 (list 'quote next-val)))
          \` (do
               (r/read-char reader) ;; skip `
               (let [next-val (parse-next ctx reader)
                     gensyms (atom {})
                     ctx (assoc ctx :gensyms gensyms)
                     ret (syntax-quote ctx reader next-val)]
                 ret))
          \~ (do
               (r/read-char reader) ;; skip `
               (let [nc (r/peek-char reader)]
                 (if (identical? nc \@)
                   (do
                     (r/read-char reader) ;; ignore @
                     (let [next-val (parse-next ctx reader)]
                       (list 'clojure.core/unquote-splicing next-val)))
                   (let [next-val (parse-next ctx reader)]
                     (list 'clojure.core/unquote next-val)))))
          \( (parse-list ctx reader)
          \[ (parse-to-delimiter ctx reader \])
          \{ (parse-map ctx reader)
          (\} \] \)) (let [expected (::expected-delimiter ctx)]
                       (if (not= expected c)
                         (let [loc (location reader)]
                           (r/read-char reader) ;; ignore unexpected
                           ;; delimiter to be able to
                           ;; continue reading, fix for
                           ;; babashka socket REPL
                           (throw-reader ctx reader
                                         (str "Unmatched delimiter: " c
                                              (when expected
                                                (str ", expected: " expected
                                                     (when-let [{:keys [:line :column :char]} (::opened-delimiter ctx)]
                                                       (str " to match " char " at " [line column])))))
                                         ctx
                                         loc))
                         (do
                           ;; read delimiter
                           (r/read-char reader)
                           ::expected-delimiter)))
          \; (parse-comment reader)
          \^ (do
               (r/read-char reader) ;; ignore ^
               (let [meta-val (parse-next ctx reader true)
                     val-val (vary-meta (parse-next ctx reader)
                                        merge meta-val)]
                 val-val))
          \: (parse-keyword ctx reader)
          (edn-read ctx reader)))))

(defn desugar-meta
  "Resolves syntactical sugar in metadata" ;; could be combined with some other desugar?
  ([f]
   (cond
     (keyword? f) {f true}
     (symbol? f)  {:tag f}
     (string? f)  {:tag f}
     :else        f)))

(defn iobj? [obj]
  #?(:clj
     (instance? clojure.lang.IObj obj)
     :cljs (satisfies? IWithMeta obj)))

(defn parse-next
  ([ctx reader] (parse-next ctx reader nil))
  ([ctx reader desugar]
   (if-let [c (and (skip-whitespace ctx reader)
                   (r/peek-char reader))]
     (let [loc (location reader)
           log? (:source ctx)
           buf (fn [] (str (:buffer @#?(:clj (.source-log-frames ^SourceLoggingPushbackReader reader)
                                        :cljs (.-frames reader)))))
           offset (when log? (count (buf)))
           obj (if log?
                 #?(:clj (r/log-source reader (dispatch ctx reader c))
                    :cljs (r/log-source* reader #(dispatch ctx reader c)))
                 (dispatch ctx reader c))]
       (if (identical? reader obj)
         (recur ctx reader desugar)
         (if (kw-identical? ::expected-delimiter obj)
           obj
           (let [iobj?? (iobj? obj)
                 src (when log?
                       (.trim (subs (buf) offset)))
                 loc? (or (and iobj??
                               (or (symbol? obj)
                                   (seq? obj))))
                 line (when loc? (:line loc))
                 column (when loc? (:column loc))
                 obj (if desugar (desugar-meta obj) obj)
                 obj (cond loc? (vary-meta obj
                                           #(cond-> %
                                              ;; Note: using 3-arity of assoc, because faster
                                              loc? (-> (assoc :line line)
                                                       (assoc :column column))
                                              src (assoc :source src)))
                           :else obj)]
             obj))))
     ::eof)))

(defn string-reader
  "Create reader for strings."
  [s]
  (r/indexing-push-back-reader
   (r/string-push-back-reader s)))

(defn parse-string [s opts]
  (let [src? (:source opts)
        r (if src? (r/source-logging-push-back-reader s)
              (string-reader s))
        ctx (assoc opts ::expected-delimiter nil)
        v (parse-next ctx r)]
    (if (kw-identical? ::eof v) nil v)))

(defn parse-string-all [s opts]
  (let [^Closeable r (string-reader s)
        ctx (assoc opts ::expected-delimiter nil)]
    (loop [ret (transient [])]
      (let [next-val (parse-next ctx r)]
        (if (kw-identical? ::eof next-val)
          (persistent! ret)
          (recur (conj! ret next-val)))))))

(defn reader
  [x]
  #?(:clj (r/indexing-push-back-reader (r/push-back-reader x))
     :cljs (let [string-reader (r/string-reader x)
                 buf-len 1
                 pushback-reader (r/PushbackReader. string-reader
                                                    (object-array buf-len)
                                                    buf-len buf-len)]
             (r/indexing-push-back-reader pushback-reader))))

(defn get-line-number [reader]
  (r/get-line-number reader))

(defn get-column-number [reader]
  (r/get-column-number reader))

(defn source-logging-reader
  [x]
  #?(:clj (r/source-logging-push-back-reader (r/push-back-reader x))
     :cljs (let [string-reader (r/string-reader x)
                 buf-len 1
                 pushback-reader (r/PushbackReader. string-reader
                                                    (object-array buf-len)
                                                    buf-len buf-len)]
             (r/source-logging-push-back-reader pushback-reader))))

;;;; Scratch

(comment
  )
