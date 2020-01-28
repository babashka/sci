(ns sci.impl.namespaces
  {:no-doc true}
  (:refer-clojure :exclude [ex-message ex-cause])
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [sci.impl.vars :as vars]
   [sci.impl.io :as io]
   [sci.impl.hierarchies :as hierarchies]
   [sci.impl.multimethods :as mm]))

(defn macrofy [f]
  (vary-meta f #(assoc % :sci/macro true)))

(defn ->>*
  [_ _ x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defn dotimes*
  [_ _ bindings & body]
  (assert (vector? bindings))
  (assert (= 2 (count bindings)))
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(defn if-not*
  "if-not from clojure.core"
  ([&form &env test then] (if-not* &form &env test then nil))
  ([_&form _&env test then else]
   `(if (not ~test) ~then ~else)))

(defn when*
  [_ _ test & body]
  (list 'if test (cons 'do body)))

(defn when-not*
  "when-not from clojure.core"
  [_&form _&env test & body]
  (list 'if test nil (cons 'do body)))

(defn doto*
  "doto from clojure.core"
  [_&form _&env x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (with-meta
                  (if (seq? f)
                    `(~(first f) ~gx ~@(next f))
                    `(~f ~gx))
                  (meta f)))
              forms)
       ~gx)))

(defn cond*
  [_ _ & clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (new #?(:clj IllegalArgumentException
                           :cljs js/Error)
                        "cond requires an even number of forms")))
          (cons 'clojure.core/cond (next (next clauses))))))

(defn cond->*
  [_&form _&env expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (-> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn cond->>*
  [_&form _&env expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (->> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn if-let*
  ([&form &env bindings then]
   (if-let* &form &env bindings then nil))
  ([_&form _&env bindings then else & _oldform]
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))

(defn when-let*
  [_&form _&env bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))

(defn when-first* [_ _ bindings & body]
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
         ~@body))))

(defn some->*
  [_&form _&env expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn some->>*
  [_ _ expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(def ex-message
  (if-let [v (resolve 'clojure.core/ex-message)]
    @v
    (fn ex-message [ex]
      (when (instance? #?(:clj Throwable :cljs js/Error) ex)
        #?(:clj (.getMessage ^Throwable ex)
           :cljs (.-message ex))))))

(def ex-cause
  (if-let [v (resolve 'clojure.core/ex-cause)]
    @v
    (fn ex-message [ex]
      (when (instance? #?(:clj Throwable :cljs ExceptionInfo) ex)
        #?(:clj (.getCause ^Throwable ex)
           :cljs (.-cause ex))))))

(defn assert*
  ([_&form _ x]
   `(when-not ~x
      (throw (#?(:clj AssertionError. :cljs js/Error.) (str "Assert failed: " (pr-str '~x))))))
  ([_&form _ x message]
   `(when-not ~x
      (throw (#?(:clj AssertionError. :cljs js/Error.) (str "Assert failed: " ~message "\n" (pr-str '~x)))))))

(defn with-open*
  [_ _ bindings & body]
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (.close ~(bindings 0)))))
    :else #?(:clj (throw (IllegalArgumentException.
                          "with-open only allows Symbols in bindings"))
             :cljs ::TODO)))

(defn letfn* [_ _ fnspecs & body]
  (let [syms (map first fnspecs)
        state-sym (gensym "state")
        fns (map (fn [sym]
                   `(fn [& args#]
                      (apply (get (deref ~state-sym) '~sym) args#))) syms)]
    `(let [~state-sym (volatile! {})
           ~@(interleave syms fns)
           ~@(mapcat (fn [sym fnspec]
                       [sym `(fn ~sym ~@(rest fnspec))]) syms fnspecs)]
       (do ~@(map (fn [sym]
                    `(vswap! ~state-sym assoc '~sym ~sym)) syms)
           nil ;; if body is empty, we return nil and not the result of vswaps.
           ~@body))))


(defn vswap!*
  [vol f & args]
  (vreset! vol (apply f @vol args)))

(defn delay*
  [_ _ & body]
  #?(:clj `(new clojure.lang.Delay (fn [] ~@body))
     :cljs `(new cljs.core.Delay (fn [] ~@body))))

(defn defn-*
  [_ _ name & decls]
  (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(defn condp*
  [_ _ pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [[[a b c :as clause] more]
                     (split-at (if (= :>> (second args)) 3 2) args)
                     n (count clause)]
                 (cond
                   (= 0 n) `(throw (new #?(:clj IllegalArgumentException
                                           :cljs js/Error)
                                        (str "No matching clause: " ~expr)))
                   (= 1 n) a
                   (= 2 n) `(if (~pred ~a ~expr)
                              ~b
                              ~(emit pred expr more))
                   :else `(if-let [p# (~pred ~a ~expr)]
                            (~c p#)
                            ~(emit pred expr more)))))]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

(defn defonce*
  [_ _ name expr]
  `(let [v# (def ~name)]
     (when-not (~'has-root-impl v#)
       (def ~name ~expr))))

(defn double-dot
  ([_ _ x form] `(. ~x ~form))
  ([_ _ x form & more] `(.. (. ~x ~form) ~@more)))

(defn has-root-impl [sci-var]
  (vars/hasRoot sci-var))

;;;; Namespaces

(defn sci-ns-name [^sci.impl.vars.SciNamespace ns]
  (vars/getName ns))

(defn sci-alias [ctx alias-sym ns-sym]
  (swap! (:env ctx)
         (fn [env]
           (let [current-ns (vars/current-ns-name)]
             (assoc-in env [:namespaces current-ns :aliases alias-sym] ns-sym))))
  nil)

(defn sci-find-ns [ctx ns-sym]
  (assert (symbol? ns-sym))
  (when (get-in @(:env ctx) [:namespaces ns-sym])
    (vars/->SciNamespace ns-sym)))

(defn sci-the-ns [ctx x]
  (if (instance? sci.impl.vars.SciNamespace x) x
      (sci-find-ns ctx x)))

(defn sci-ns-aliases [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        aliases (get-in @(:env ctx) [:namespaces name :aliases])]
    (zipmap (keys aliases)
            (map (fn [sym]
                   (vars/->SciNamespace sym))
                 (vals aliases)))))

(defn sci-ns-publics [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (dissoc m :aliases)]
    (into {} (keep (fn [[k v]]
                     (when-not (:private (meta v))
                       [k v]))
                   m))))

;;;; End namespaces

(def clojure-core
  {'*ns* vars/current-ns
   ;; io
   '*in* io/in
   '*out* io/out
   '*err* io/err
   '*file* vars/current-file
   'newline io/newline
   'flush io/flush
   'pr io/pr
   'prn io/prn
   'print io/print
   'println io/println
   #?@(:clj ['printf io/printf])
   'with-out-str (with-meta io/with-out-str
                   {:sci/macro true})
   #?@(:clj ['with-in-str (with-meta io/with-in-str
                            {:sci/macro true})
             'read-line io/read-line])
   ;; end io
   '.. (macrofy double-dot)
   '= =
   '< <
   '<= <=
   '> >
   '>= >=
   '+ +
   '- -
   '* *
   '/ /
   '== ==
   '->> (macrofy ->>*)
   'add-watch add-watch
   'aget aget
   'alias (with-meta sci-alias {:sci.impl/op :needs-ctx})
   'alter-meta! alter-meta!
   'alter-var-root vars/alter-var-root
   'ancestors (with-meta hierarchies/ancestors* {:sci.impl/op :needs-ctx})
   'aset aset
   'alength alength
   'any? any?
   'apply apply
   'array-map array-map
   'assert (with-meta assert* {:sci/macro true})
   'assoc assoc
   'assoc! assoc!
   'assoc-in assoc-in
   'associative? associative?
   'atom atom
   'binding (with-meta vars/binding {:sci/macro true})
   'binding-conveyor-fn vars/binding-conveyor-fn
   'bit-and-not bit-and-not
   'bit-set bit-set
   'bit-shift-left bit-shift-left
   'bit-shift-right bit-shift-right
   'bit-xor bit-xor
   'boolean boolean
   'boolean? boolean?
   'booleans booleans
   'butlast butlast
   'bytes bytes
   'bit-test bit-test
   'bit-and bit-and
   'bounded-count bounded-count
   'bit-or bit-or
   'bit-flip bit-flip
   'bit-not bit-not
   'byte byte
   'cat cat
   'char char
   'char? char?
   #?@(:cljs ['clj->js clj->js])
   'cond (macrofy cond*)
   'cond-> (macrofy cond->*)
   'cond->> (macrofy cond->>*)
   'condp (macrofy condp*)
   'conj conj
   'conj! conj!
   'cons cons
   'contains? contains?
   'count count
   'cycle cycle
   'comp comp
   'concat concat
   'comparator comparator
   'coll? coll?
   'compare compare
   'complement complement
   'constantly constantly
   'chars chars
   'completing completing
   'counted? counted?
   'chunk chunk
   'chunk-append chunk-append
   'chunk-buffer chunk-buffer
   'chunk-cons chunk-cons
   'chunk-first chunk-first
   'chunk-rest chunk-rest
   'chunk-next chunk-next
   'chunked-seq? chunked-seq?
   'dec dec
   'dedupe dedupe
   'defn- (macrofy defn-*)
   'defmulti (with-meta mm/defmulti
               {:sci/macro true
                :sci.impl/op :needs-ctx})
   'defmethod (macrofy mm/defmethod)
   'defonce (macrofy defonce*)
   'delay (macrofy delay*)
   'deref deref
   'derive (with-meta hierarchies/derive* {:sci.impl/op :needs-ctx})
   'descendants (with-meta hierarchies/descendants* {:sci.impl/op :needs-ctx})
   'dissoc dissoc
   'distinct distinct
   'distinct? distinct?
   'disj disj
   'doall doall
   'dorun dorun
   'dotimes (macrofy dotimes*)
   'doto (macrofy doto*)
   'double double
   'double? double?
   'drop drop
   'drop-last drop-last
   'drop-while drop-while
   'doubles doubles
   'eduction eduction
   'empty empty
   'empty? empty?
   'even? even?
   'every? every?
   'every-pred every-pred
   'ensure-reduced ensure-reduced
   'ex-data ex-data
   'ex-info ex-info
   'ex-message ex-message
   'ex-cause ex-cause
   'find-ns (with-meta sci-find-ns {:sci.impl/op :needs-ctx})
   'first first
   'float? float?
   'floats floats
   'fnil fnil
   'fnext fnext
   'ffirst ffirst
   'flatten flatten
   'false? false?
   'filter filter
   'filterv filterv
   'find find
   'frequencies frequencies
   'float float
   'fn? fn?
   'get get
   'get-in get-in
   'group-by group-by
   'gensym gensym
   'has-root-impl has-root-impl
   'hash hash
   'hash-map hash-map
   'hash-set hash-set
   'hash-unordered-coll hash-unordered-coll
   'ident? ident?
   'identical? identical?
   'identity identity
   'if-let (macrofy if-let*)
   'if-not (macrofy if-not*)
   'ifn? ifn?
   'inc inc
   'inst? inst?
   'instance? instance?
   'int-array int-array
   'interleave interleave
   'into into
   'iterate iterate
   'int int
   'int? int?
   'interpose interpose
   'indexed? indexed?
   'integer? integer?
   'ints ints
   'into-array into-array
   'isa? (with-meta hierarchies/isa?* {:sci.impl/op :needs-ctx})
   #?@(:cljs ['js->clj js->clj])
   #?@(:cljs ['js-obj js-obj])
   'juxt juxt
   'keep keep
   'keep-indexed keep-indexed
   'key key
   'keys keys
   'keyword keyword
   'keyword? keyword?
   'last last
   'letfn (macrofy letfn*)
   'long long
   'list list
   'list? list?
   'longs longs
   'list* list*
   'long-array long-array
   'make-array make-array
   'make-hierarchy make-hierarchy
   'map map
   'map? map?
   'map-indexed map-indexed
   'map-entry? map-entry?
   'mapv mapv
   'mapcat mapcat
   'max max
   'max-key max-key
   'meta meta
   'merge merge
   'merge-with merge-with
   'min min
   'min-key min-key
   'multi-fn-add-method-impl mm/multi-fn-add-method-impl
   'multi-fn?-impl mm/multi-fn?-impl
   'multi-fn-impl mm/multi-fn-impl
   'munge munge
   'mod mod
   'name name
   'namespace namespace
   ;; 'newline newline
   'nfirst nfirst
   'not not
   'not= not=
   'not-every? not-every?
   'neg? neg?
   'neg-int? neg-int?
   'nth nth
   'nthnext nthnext
   'nthrest nthrest
   'nil? nil?
   'nat-int? nat-int?
   'number? number?
   'not-empty not-empty
   'not-any? not-any?
   'next next
   'nnext nnext
   'ns-aliases (with-meta sci-ns-aliases {:sci.impl/op :needs-ctx})
   'ns-publics (with-meta sci-ns-publics {:sci.impl/op :needs-ctx})
   'ns-name sci-ns-name
   'odd? odd?
   'object-array object-array
   'parents (with-meta hierarchies/parents* {:sci.impl/op :needs-ctx})
   'peek peek
   'pop pop
   'pop-thread-bindings vars/pop-thread-bindings
   'pos? pos?
   'pos-int? pos-int?
   'partial partial
   'partition partition
   'partition-all partition-all
   'partition-by partition-by
   'persistent! persistent!
   'pr-str pr-str
   'prn-str prn-str
   'print-str print-str
   'push-thread-bindings vars/push-thread-bindings
   'qualified-ident? qualified-ident?
   'qualified-symbol? qualified-symbol?
   'qualified-keyword? qualified-keyword?
   'quot quot
   're-seq re-seq
   're-find re-find
   're-pattern re-pattern
   're-matches re-matches
   'rem rem
   'remove remove
   'reset-meta! reset-meta!
   'rest rest
   'repeatedly repeatedly
   'reverse reverse
   'rand-int rand-int
   'rand-nth rand-nth
   'range range
   'record? record?
   'reduce reduce
   'reduce-kv reduce-kv
   'reduced reduced
   'reduced? reduced?
   'reset! reset!
   'reset-vals! reset-vals!
   'reversible? reversible?
   'rsubseq rsubseq
   'reductions reductions
   'rand rand
   'replace replace
   'rseq rseq
   'random-sample random-sample
   'repeat repeat
   'run! run!
   'set? set?
   'sequential? sequential?
   'select-keys select-keys
   'simple-keyword? simple-keyword?
   'simple-symbol? simple-symbol?
   'some? some?
   'some-> (macrofy some->*)
   'some->> (macrofy some->>*)
   'string? string?
   'str str
   'second second
   'set set
   'seq seq
   'seq? seq?
   'short short
   'shuffle shuffle
   'sort sort
   'sort-by sort-by
   'subs subs
   #?@(:clj ['supers supers])
   'symbol symbol
   'symbol? symbol?
   'special-symbol? special-symbol?
   'subvec subvec
   'some-fn some-fn
   'some some
   'split-at split-at
   'split-with split-with
   'sorted-set sorted-set
   'subseq subseq
   'sorted-set-by sorted-set-by
   'sorted-map-by sorted-map-by
   'sorted-map sorted-map
   'sorted? sorted?
   'simple-ident? simple-ident?
   'sequence sequence
   'seqable? seqable?
   'shorts shorts
   'swap! swap!
   'swap-vals! swap-vals!
   'take take
   'take-last take-last
   'take-nth take-nth
   'take-while take-while
   'the-ns (with-meta sci-the-ns {:sci.impl/op :needs-ctx})
   'trampoline trampoline
   'transduce transduce
   'transient transient
   'tree-seq tree-seq
   'type type
   'true? true?
   'to-array to-array
   'update update
   'update-in update-in
   'uri? uri?
   'uuid? uuid?
   'unchecked-inc-int unchecked-inc-int
   'unchecked-long unchecked-long
   'unchecked-negate unchecked-negate
   'unchecked-remainder-int unchecked-remainder-int
   'unchecked-subtract-int unchecked-subtract-int
   'unsigned-bit-shift-right unsigned-bit-shift-right
   'unchecked-float unchecked-float
   'unchecked-add-int unchecked-add-int
   'unchecked-double unchecked-double
   'unchecked-multiply-int unchecked-multiply-int
   'unchecked-int unchecked-int
   'unchecked-multiply unchecked-multiply
   'unchecked-dec-int unchecked-dec-int
   'unchecked-add unchecked-add
   'unreduced unreduced
   'unchecked-divide-int unchecked-divide-int
   'unchecked-subtract unchecked-subtract
   'unchecked-negate-int unchecked-negate-int
   'unchecked-inc unchecked-inc
   'unchecked-char unchecked-char
   'unchecked-byte unchecked-byte
   'unchecked-short unchecked-short
   'underive (with-meta hierarchies/underive* {:sci.impl/op :needs-ctx})
   'val val
   'vals vals
   'var? sci.impl.vars/var?
   'vary-meta vary-meta
   'vec vec
   'vector vector
   'vector? vector?
   'volatile! volatile!
   'vreset! vreset!
   'vswap! vswap!*
   'when-first (macrofy when-first*)
   'when-let (macrofy when-let*)
   'when (macrofy when*)
   'when-not (macrofy when-not*)
   'with-meta with-meta
   'with-open (macrofy with-open*)
   ;; 'with-redefs (macrofy vars/with-redefs)
   ;; 'with-redefs-fn vars/with-redefs-fn
   'zipmap zipmap
   'zero? zero?
   #?@(:clj ['+' +'
             '-' -'
             '*' *'
             'boolean-array boolean-array
             'bound? bound?
             'byte-array byte-array
             'bigint bigint
             'bytes? bytes?
             'biginteger biginteger
             'bigdec bigdec
             'char-array char-array
             'char-escape-string char-escape-string
             'char-name-string char-name-string
             'class class
             'dec' dec'
             'decimal? decimal?
             'denominator denominator
             'format format
             'float-array float-array
             'inc' inc'
             'line-seq line-seq
             'num num
             'namespace-munge namespace-munge
             'numerator numerator
             'replicate replicate
             'rational? rational?
             'ratio? ratio?
             'rationalize rationalize
             'seque seque
             'xml-seq xml-seq])})

(defn dir-fn
  [ctx ns]
  (let [current-ns (vars/current-ns-name)
        the-ns (sci-the-ns ctx
                           (get (sci-ns-aliases ctx current-ns) ns ns))]
    (sort (map first (sci-ns-publics ctx the-ns)))))

(defn dir
  [_ _ nsname]
  `(doseq [v# (clojure.repl/dir-fn '~nsname)]
     (println v#)))

(def clojure-repl
  {'dir-fn (with-meta dir-fn {:sci.impl/op :needs-ctx})
   'dir (macrofy dir)})

(def namespaces
  {'clojure.core clojure-core
   'clojure.string {'blank? str/blank?
                    'capitalize str/capitalize
                    'ends-with? str/ends-with?
                    'escape str/escape
                    'includes? str/includes?
                    'index-of str/index-of
                    'join str/join
                    'last-index-of str/last-index-of
                    'lower-case str/lower-case
                    'replace str/replace
                    'replace-first str/replace-first
                    'reverse str/reverse
                    'split str/split
                    'split-lines str/split-lines
                    'starts-with? str/starts-with?
                    'trim str/trim
                    'trim-newline str/trim-newline
                    'triml str/triml
                    'trimr str/trimr
                    'upper-case str/upper-case
                    #?@(:clj ['re-quote-replacement str/re-quote-replacement])}
   'clojure.set {'difference set/difference
                 'index set/index
                 'intersection set/intersection
                 'join set/join
                 'map-invert set/map-invert
                 'project set/project
                 'rename set/rename
                 'rename-keys set/rename-keys
                 'select set/select
                 'subset? set/subset?
                 'superset? set/superset?
                 'union set/union}
   'clojure.walk {'walk clojure.walk/walk
                  'postwalk clojure.walk/postwalk
                  'prewalk clojure.walk/prewalk
                  #?@(:clj ['postwalk-demo clojure.walk/postwalk-demo
                            'prewalk-demo clojure.walk/prewalk-demo])
                  'keywordize-keys clojure.walk/keywordize-keys
                  'stringify-keys clojure.walk/stringify-keys
                  'prewalk-replace clojure.walk/prewalk-replace
                  'postwalk-replace clojure.walk/postwalk-replace}
   'clojure.repl clojure-repl})

(def aliases
  '{str clojure.string
    set clojure.set})
