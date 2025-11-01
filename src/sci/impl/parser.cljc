(ns sci.impl.parser
  {:no-doc true}
  (:refer-clojure :exclude [read-string eval])
  (:require
   [clojure.string :as str]
   [clojure.tools.reader.reader-types :as rt]
   [edamame.core :as edamame]
   [sci.impl.interop :as interop]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]))

#?(:cljs nil :default (set! *warn-on-reflection* true))

(def ^:const eof :sci.impl.parser.edamame/eof)

(def read-eval
  (utils/new-var '*read-eval* false {:ns utils/clojure-core-ns
                                     :dynamic true
                                     :doc "Defaults to true (or value specified by system property, see below)\n  ***This setting implies that the full power of the reader is in play,\n  including syntax that can cause code to execute. It should never be\n  used with untrusted sources. See also: clojure.edn/read.***\n\n  When set to logical false in the thread-local binding,\n  the eval reader (#=) and record/type literal syntax are disabled in read/load.\n  Example (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))\n\n  The default binding can be controlled by the system property\n  'clojure.read.eval' System properties can be set on the command line\n  like this:\n\n  java -Dclojure.read.eval=false ...\n\n  The system property can also be set to 'unknown' via\n  -Dclojure.read.eval=unknown, in which case the default binding\n  is :unknown and all reads will fail in contexts where *read-eval*\n  has not been explicitly bound to either true or false. This setting\n  can be a useful diagnostic tool to ensure that all of your reads\n  occur in considered contexts. You can also accomplish this in a\n  particular scope by binding *read-eval* to :unknown\n  "}))

(def data-readers
  (utils/new-var '*data-readers* {}
                 {:ns utils/clojure-core-ns
                  :dynamic true
                  :doc "Map from reader tag symbols to data reader Vars.\n\n  When Clojure starts, it searches for files named 'data_readers.clj'\n  and 'data_readers.cljc' at the root of the classpath. Each such file\n  must contain a literal map of symbols, like this:\n\n      {foo/bar my.project.foo/bar\n       foo/baz my.project/baz}\n\n  The first symbol in each pair is a tag that will be recognized by\n  the Clojure reader. The second symbol in the pair is the\n  fully-qualified name of a Var which will be invoked by the reader to\n  parse the form following the tag. For example, given the\n  data_readers.clj file above, the Clojure reader would parse this\n  form:\n\n      #foo/bar [1 2 3]\n\n  by invoking the Var #'my.project.foo/bar on the vector [1 2 3]. The\n  data reader function is invoked on the form AFTER it has been read\n  as a normal Clojure data structure by the reader.\n\n  Reader tags without namespace qualifiers are reserved for\n  Clojure. Default reader tags are defined in\n  clojure.core/default-data-readers but may be overridden in\n  data_readers.clj, data_readers.cljc, or by rebinding this Var."}))

(def default-data-reader-fn
  (utils/new-var '*default-data-reader-fn* nil
                 {:ns utils/clojure-core-ns
                  :dynamic true
                  :doc "When no data reader is found for a tag and *default-data-reader-fn*\n  is non-nil, it will be called with two arguments,\n  the tag and the value.  If *default-data-reader-fn* is nil (the\n  default), an exception will be thrown for the unknown tag."}))

(def reader-resolver
  (utils/new-var '*reader-resolver* nil
                 {:ns utils/clojure-core-ns
                  :dynamic true}))

(def suppress-read
  (utils/new-var '*suppress-read* nil
                 {:ns utils/clojure-core-ns
                  :dynamic true}))

(def default-opts
  (edamame/normalize-opts
   {:all true
    :row-key :line
    :col-key :column
    :read-cond :allow
    :location? seq?
    :end-location false}))

(defn var->sym [v]
  (when-let [m (meta v)]
    (if (:sci/record m)
      (-> (deref v)
          str
          symbol)
      (when-let [var-name (:name m)]
        (when-let [ns (:ns m)]
          (symbol (str (types/getName ns))
                  (str var-name)))))))

(defn fully-qualify [ctx sym]
  (let [env @(:env ctx)
        sym-ns (when-let [n (namespace sym)]
                 (symbol n))
        current-ns (utils/current-ns-name)
        current-ns-str (str current-ns)
        namespaces (get env :namespaces)
        the-current-ns (get namespaces current-ns)
        aliases (:aliases the-current-ns)
        res-without-sym
        (fn res-without-sym [sym]
          (let [sym-name-str (name sym)]
            (or (when-let [refers (:refers the-current-ns)]
                  (when-let [v (get refers sym)]
                    (var->sym v)))
                (when-let [v (get the-current-ns sym)]
                  (var->sym v))
                (when (or (and (contains? (get namespaces 'clojure.core) sym)
                               ;; only valid when the symbol isn't excluded
                               (not (some-> the-current-ns
                                            :refer
                                            (get 'clojure.core)
                                            :exclude
                                            (contains? sym))))
                          (contains? utils/ana-macros sym))
                  (symbol "clojure.core" sym-name-str))
                (interop/fully-qualify-class ctx sym)
                ;; all unresolvable symbols all resolved in the current namespace
                (if (str/includes? sym-name-str ".")
                  ;; NOTE: move to to edamame?
                  (if (and (not (str/starts-with? sym-name-str "."))
                           (str/ends-with? sym-name-str "."))
                    (symbol (str (res-without-sym (symbol (subs sym-name-str 0 (dec (count sym-name-str))))) "."))
                    ;; unresolved class
                    sym)
                  (symbol current-ns-str sym-name-str)))))
        ret (if-not sym-ns
              (res-without-sym sym)
              (let [sym-name (name sym)]
                (or
                 #?@(:cljs []
                     :default [(when (and (= 1 (#?(:clj .length :cljr .Length) sym-name))
                                          (-> sym-name
                                              (#?(:clj .charAt :cljr .get_Chars) 0)
                                              #?(:clj Character/isDigit :cljr Char/IsDigit)))
                                 (when-let [clazz (interop/resolve-array-class ctx sym-ns sym-name)]
                                   (symbol (pr-str clazz))))])
                 (let [nss (get env :namespaces)]
                   (if (get nss sym-ns)
                     sym
                     (if-let [ns (get aliases sym-ns)]
                       (symbol (str ns) sym-name)
                       #?(:cljs
                          ;; This enables using `(fs/readFileSync) mode in macros, e.g. in nbb
                          (if-let [import (-> nss (get current-ns) :imports (get sym-ns))]
                            (symbol (str import) (name sym))
                            sym)
                          :default sym)))))))]
    ret))

(defn throw-eval-read [_]
  (throw (ex-info "EvalReader not allowed when *read-eval* is false."
                  {:type :sci.error/parse})))

(defn auto-resolve [ctx opts]
  (or (:auto-resolve opts)
      (let [env (:env ctx)
            env-val @env
            current-ns (utils/current-ns-name)
            the-current-ns (get-in env-val [:namespaces current-ns])
            aliases (:aliases the-current-ns)
            auto-resolve (assoc aliases :current current-ns)]
        auto-resolve)))

(defn get-line-number [reader]
  (rt/get-line-number reader))

(defn get-column-number [reader]
  (rt/get-column-number reader))

(defn parse-next
  ([ctx r]
   (parse-next ctx r nil))
  ([ctx r opts]
   (let [features (:features ctx)
         readers (:readers ctx)
         readers (if (utils/var? readers) @readers readers)
         auto-resolve (auto-resolve ctx opts)
         parse-opts (cond-> (assoc default-opts
                                   :features features
                                   :auto-resolve auto-resolve
                                   :syntax-quote {:resolve-symbol #(fully-qualify ctx %)}
                                   :readers (fn [t]
                                              (or (and readers (readers t))
                                                  (@data-readers t)
                                                  (some-> (@utils/eval-resolve-state ctx {} t)
                                                          meta
                                                          :sci.impl.record/map-constructor)
                                                  (when-let [f @default-data-reader-fn]
                                                    (fn [form]
                                                      (f t form)))))
                                   :read-eval (if @read-eval
                                                (fn [x]
                                                  (utils/eval ctx x))
                                                throw-eval-read))
                      opts (merge opts))
         ret (try (let [v (edamame/parse-next r parse-opts)]
                    (if (utils/kw-identical? v :edamame.core/eof)
                      eof
                      (if (and (symbol? v)
                               (rt/indexing-reader? r))
                        (vary-meta v assoc
                                   :line (get-line-number r)
                                   :column (- (get-column-number r)
                                              (#?(:clj .length
                                                  :cljs .-length
                                                  :cljr .Length)
                                               (str v))))
                        v)))
                  (catch #?(:cljs cljs.core/ExceptionInfo
                            :default clojure.lang.ExceptionInfo) e
                    (throw (ex-info (#?(:clj .getMessage
                                        :cljs .-message
                                        :cljr .Message)
                                     e)
                                    (assoc (ex-data e)
                                           :type :sci.error/parse
                                           :phase "parse"
                                           :file @utils/current-file)
                                    e))))]
     ret)))

(defn reader [x]
  (edamame/reader x))

(defn parse-string
  ([ctx s]
   (let [r (reader s)
         v (parse-next ctx r)]
     (if (utils/kw-identical? eof v) nil v))))

;;;; Scratch

(comment
  )
