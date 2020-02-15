(ns sci.impl.namespaces
  {:no-doc true}
  (:refer-clojure :exclude [ex-message ex-cause])
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [sci.impl.hierarchies :as hierarchies]
   [sci.impl.io :as io]
   [sci.impl.multimethods :as mm]
   [sci.impl.vars :as vars]
   [sci.impl.macros :as macros])
  #?(:cljs (:require-macros [sci.impl.namespaces :refer [copy-var]])))

(def clojure-core-ns (vars/->SciNamespace 'clojure.core))

(macros/deftime
  (defmacro copy-var
    ([sym]
     `(vars/->SciVar ~sym '~sym {:doc (-> (var ~sym) meta :doc)
                                 :ns clojure-core-ns
                                 :sci.impl/built-in true}))
    ([sym ns]
     `(vars/->SciVar ~sym '~sym {:doc (-> (var ~sym) meta :doc)
                                 :ns ~ns
                                 :sci.impl/built-in true}))))

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

(defn sci-ns-interns [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (dissoc m :aliases)]
    m))

(defn sci-ns-publics [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (dissoc m :aliases)]
    (into {} (keep (fn [[k v]]
                     (when-not (:private (meta v))
                       [k v]))
                   m))))

(defn sci-all-ns [ctx]
  (map #(vars/->SciNamespace %) (keys (get @(:env ctx) :namespaces))))

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
   ;; multimethods
   'defmulti (with-meta mm/defmulti
               {:sci/macro true
                :sci.impl/op :needs-ctx})
   'defmethod (macrofy mm/defmethod)
   'get-method (copy-var get-method)
   'methods (copy-var methods)
   'multi-fn-add-method-impl mm/multi-fn-add-method-impl
   'multi-fn?-impl mm/multi-fn?-impl
   'multi-fn-impl mm/multi-fn-impl
   'prefer-method (copy-var prefer-method)
   'prefers (copy-var prefers)
   'remove-method (copy-var remove-method)
   'remove-all-methods (copy-var remove-all-methods)
   ;; end multimethods
   '.. (macrofy double-dot)
   '= (copy-var =)
   '< (copy-var <)
   '<= (copy-var <=)
   '> (copy-var >)
   '>= (copy-var >=)
   '+ (copy-var +)
   '- (copy-var -)
   '* (copy-var *)
   '/ (copy-var /)
   '== (copy-var ==)
   '->> (macrofy ->>*)
   'add-watch (copy-var add-watch)
   'aget (copy-var aget)
   'alias (with-meta sci-alias {:sci.impl/op :needs-ctx})
   'all-ns (with-meta sci-all-ns {:sci.impl/op :needs-ctx})
   'alter-meta! (copy-var alter-meta!)
   'alter-var-root vars/alter-var-root
   'ancestors (with-meta hierarchies/ancestors* {:sci.impl/op :needs-ctx})
   'aset (copy-var aset)
   'alength (copy-var alength)
   'any? (copy-var any?)
   'apply (copy-var apply)
   'array-map (copy-var array-map)
   'assert (with-meta assert* {:sci/macro true})
   'assoc (copy-var assoc)
   'assoc! (copy-var assoc!)
   'assoc-in (copy-var assoc-in)
   'associative? (copy-var associative?)
   'atom (copy-var atom)
   'binding (with-meta vars/binding {:sci/macro true})
   'binding-conveyor-fn vars/binding-conveyor-fn
   'bit-and-not (copy-var bit-and-not)
   'bit-set (copy-var bit-set)
   'bit-shift-left (copy-var bit-shift-left)
   'bit-shift-right (copy-var bit-shift-right)
   'bit-xor (copy-var bit-xor)
   'boolean (copy-var boolean)
   'boolean? (copy-var boolean?)
   'booleans (copy-var booleans)
   'butlast (copy-var butlast)
   'bytes (copy-var bytes)
   'bit-test (copy-var bit-test)
   'bit-and (copy-var bit-and)
   'bounded-count (copy-var bounded-count)
   'bit-or (copy-var bit-or)
   'bit-flip (copy-var bit-flip)
   'bit-not (copy-var bit-not)
   'byte (copy-var byte)
   'cat (copy-var cat)
   'char (copy-var char)
   'char? (copy-var char?)
   #?@(:cljs ['clj->js (copy-var clj->js)])
   'cond (macrofy cond*)
   'cond-> (macrofy cond->*)
   'cond->> (macrofy cond->>*)
   'condp (macrofy condp*)
   'conj (copy-var conj)
   'conj! (copy-var conj!)
   'cons (copy-var cons)
   'contains? (copy-var contains?)
   'count (copy-var count)
   'cycle (copy-var cycle)
   'comp (copy-var comp)
   'concat (copy-var concat)
   'comparator (copy-var comparator)
   'coll? (copy-var coll?)
   'compare (copy-var compare)
   'complement (copy-var complement)
   'constantly (copy-var constantly)
   'chars (copy-var chars)
   'completing (copy-var completing)
   'counted? (copy-var counted?)
   'chunk (copy-var chunk)
   'chunk-append (copy-var chunk-append)
   'chunk-buffer (copy-var chunk-buffer)
   'chunk-cons (copy-var chunk-cons)
   'chunk-first (copy-var chunk-first)
   'chunk-rest (copy-var chunk-rest)
   'chunk-next (copy-var chunk-next)
   'chunked-seq? (copy-var chunked-seq?)
   'dec (copy-var dec)
   'dedupe (copy-var dedupe)
   'defn- (macrofy defn-*)
   'defonce (macrofy defonce*)
   'delay (macrofy delay*)
   #?@(:clj ['deliver (copy-var deliver)])
   'deref (copy-var deref)
   'derive (with-meta hierarchies/derive* {:sci.impl/op :needs-ctx})
   'descendants (with-meta hierarchies/descendants* {:sci.impl/op :needs-ctx})
   'dissoc (copy-var dissoc)
   'distinct (copy-var distinct)
   'distinct? (copy-var distinct?)
   'disj (copy-var disj)
   'doall (copy-var doall)
   'dorun (copy-var dorun)
   'dotimes (macrofy dotimes*)
   'doto (macrofy doto*)
   'double (copy-var double)
   'double? (copy-var double?)
   'drop (copy-var drop)
   'drop-last (copy-var drop-last)
   'drop-while (copy-var drop-while)
   'doubles (copy-var doubles)
   'eduction (copy-var eduction)
   'empty (copy-var empty)
   'empty? (copy-var empty?)
   'even? (copy-var even?)
   'every? (copy-var every?)
   'every-pred (copy-var every-pred)
   'ensure-reduced (copy-var ensure-reduced)
   'ex-data (copy-var ex-data)
   'ex-info (copy-var ex-info)
   'ex-message (copy-var ex-message)
   'ex-cause (copy-var ex-cause)
   'find-ns (with-meta sci-find-ns {:sci.impl/op :needs-ctx})
   'first (copy-var first)
   'float? (copy-var float?)
   'floats (copy-var floats)
   'fnil (copy-var fnil)
   'fnext (copy-var fnext)
   'ffirst (copy-var ffirst)
   'flatten (copy-var flatten)
   'false? (copy-var false?)
   'filter (copy-var filter)
   'filterv (copy-var filterv)
   'find (copy-var find)
   'frequencies (copy-var frequencies)
   'float (copy-var float)
   'fn? (copy-var fn?)
   'get (copy-var get)
   'get-thread-binding-frame-impl vars/get-thread-binding-frame
   'get-in (copy-var get-in)
   'group-by (copy-var group-by)
   'gensym (copy-var gensym)
   'has-root-impl (copy-var has-root-impl)
   'hash (copy-var hash)
   'hash-map (copy-var hash-map)
   'hash-set (copy-var hash-set)
   'hash-unordered-coll (copy-var hash-unordered-coll)
   'ident? (copy-var ident?)
   'identical? (copy-var identical?)
   'identity (copy-var identity)
   'if-let (macrofy if-let*)
   'if-not (macrofy if-not*)
   'ifn? (copy-var ifn?)
   'inc (copy-var inc)
   'inst? (copy-var inst?)
   'instance? (copy-var instance?)
   'int-array (copy-var int-array)
   'interleave (copy-var interleave)
   'into (copy-var into)
   'iterate (copy-var iterate)
   'int (copy-var int)
   'int? (copy-var int?)
   'interpose (copy-var interpose)
   'indexed? (copy-var indexed?)
   'integer? (copy-var integer?)
   'ints (copy-var ints)
   'into-array (copy-var into-array)
   'isa? (with-meta hierarchies/isa?* {:sci.impl/op :needs-ctx})
   #?@(:cljs ['js->clj (copy-var js->clj)])
   #?@(:cljs ['js-obj (copy-var js-obj)])
   'juxt (copy-var juxt)
   'keep (copy-var keep)
   'keep-indexed (copy-var keep-indexed)
   'key (copy-var key)
   'keys (copy-var keys)
   'keyword (copy-var keyword)
   'keyword? (copy-var keyword?)
   'last (copy-var last)
   'letfn (macrofy letfn*)
   'long (copy-var long)
   'list (copy-var list)
   'list? (copy-var list?)
   'longs (copy-var longs)
   'list* (copy-var list*)
   'long-array (copy-var long-array)
   'make-array (copy-var make-array)
   'make-hierarchy (copy-var make-hierarchy)
   'map (copy-var map)
   'map? (copy-var map?)
   'map-indexed (copy-var map-indexed)
   'map-entry? (copy-var map-entry?)
   'mapv (copy-var mapv)
   'mapcat (copy-var mapcat)
   'max (copy-var max)
   'max-key (copy-var max-key)
   'meta (copy-var meta)
   'merge (copy-var merge)
   'merge-with (copy-var merge-with)
   'min (copy-var min)
   'min-key (copy-var min-key)
   'munge (copy-var munge)
   'mod (copy-var mod)
   'name (copy-var name)
   'namespace (copy-var namespace)
   ;; 'newline newline
   'nfirst (copy-var nfirst)
   'not (copy-var not)
   'not= (copy-var not=)
   'not-every? (copy-var not-every?)
   'neg? (copy-var neg?)
   'neg-int? (copy-var neg-int?)
   'nth (copy-var nth)
   'nthnext (copy-var nthnext)
   'nthrest (copy-var nthrest)
   'nil? (copy-var nil?)
   'nat-int? (copy-var nat-int?)
   'number? (copy-var number?)
   'not-empty (copy-var not-empty)
   'not-any? (copy-var not-any?)
   'next (copy-var next)
   'nnext (copy-var nnext)
   'ns-aliases (with-meta sci-ns-aliases {:sci.impl/op :needs-ctx})
   'ns-interns (with-meta sci-ns-interns {:sci.impl/op :needs-ctx})
   'ns-publics (with-meta sci-ns-publics {:sci.impl/op :needs-ctx})
   'ns-name sci-ns-name
   'odd? (copy-var odd?)
   'object-array (copy-var object-array)
   'parents (with-meta hierarchies/parents* {:sci.impl/op :needs-ctx})
   'peek (copy-var peek)
   'pop (copy-var pop)
   'pop-thread-bindings vars/pop-thread-bindings
   'pos? (copy-var pos?)
   'pos-int? (copy-var pos-int?)
   'partial (copy-var partial)
   'partition (copy-var partition)
   'partition-all (copy-var partition-all)
   'partition-by (copy-var partition-by)
   'persistent! (copy-var persistent!)
   'pr-str (copy-var pr-str)
   #?@(:clj ['promise (copy-var promise)])
   'prn-str (copy-var prn-str)
   'print-str (copy-var print-str)
   'push-thread-bindings vars/push-thread-bindings
   'qualified-ident? (copy-var qualified-ident?)
   'qualified-symbol? (copy-var qualified-symbol?)
   'qualified-keyword? (copy-var qualified-keyword?)
   'quot (copy-var quot)
   're-seq (copy-var re-seq)
   're-find (copy-var re-find)
   're-pattern (copy-var re-pattern)
   're-matches (copy-var re-matches)
   'rem (copy-var rem)
   'remove (copy-var remove)
   'reset-meta! (copy-var reset-meta!)
   'rest (copy-var rest)
   'repeatedly (copy-var repeatedly)
   'reverse (copy-var reverse)
   'rand-int (copy-var rand-int)
   'rand-nth (copy-var rand-nth)
   'range (copy-var range)
   'record? (copy-var record?)
   'reduce (copy-var reduce)
   'reduce-kv (copy-var reduce-kv)
   'reduced (copy-var reduced)
   'reduced? (copy-var reduced?)
   'reset! (copy-var reset!)
   'reset-vals! (copy-var reset-vals!)
   'reset-thread-binding-frame-impl vars/reset-thread-binding-frame
   'reversible? (copy-var reversible?)
   'rsubseq (copy-var rsubseq)
   'reductions (copy-var reductions)
   'rand (copy-var rand)
   'replace (copy-var replace)
   'rseq (copy-var rseq)
   'random-sample (copy-var random-sample)
   'repeat (copy-var repeat)
   'run! (copy-var run!)
   #?@(:clj ['satisfies? (copy-var satisfies?)])
   'set? (copy-var set?)
   'sequential? (copy-var sequential?)
   'select-keys (copy-var select-keys)
   'simple-keyword? (copy-var simple-keyword?)
   'simple-symbol? (copy-var simple-symbol?)
   'some? (copy-var some?)
   'some-> (macrofy some->*)
   'some->> (macrofy some->>*)
   'string? (copy-var string?)
   'str (copy-var str)
   'second (copy-var second)
   'set (copy-var set)
   'seq (copy-var seq)
   'seq? (copy-var seq?)
   'short (copy-var short)
   'shuffle (copy-var shuffle)
   'sort (copy-var sort)
   'sort-by (copy-var sort-by)
   'subs (copy-var subs)
   #?@(:clj ['supers (copy-var supers)])
   'symbol (copy-var symbol)
   'symbol? (copy-var symbol?)
   'special-symbol? (copy-var special-symbol?)
   'subvec (copy-var subvec)
   'some-fn (copy-var some-fn)
   'some (copy-var some)
   'split-at (copy-var split-at)
   'split-with (copy-var split-with)
   'sorted-set (copy-var sorted-set)
   'subseq (copy-var subseq)
   'sorted-set-by (copy-var sorted-set-by)
   'sorted-map-by (copy-var sorted-map-by)
   'sorted-map (copy-var sorted-map)
   'sorted? (copy-var sorted?)
   'simple-ident? (copy-var simple-ident?)
   'sequence (copy-var sequence)
   'seqable? (copy-var seqable?)
   'shorts (copy-var shorts)
   'swap! (copy-var swap!)
   'swap-vals! (copy-var swap-vals!)
   'take (copy-var take)
   'take-last (copy-var take-last)
   'take-nth (copy-var take-nth)
   'take-while (copy-var take-while)
   'the-ns (with-meta sci-the-ns {:sci.impl/op :needs-ctx})
   'trampoline (copy-var trampoline)
   'transduce (copy-var transduce)
   'transient (copy-var transient)
   'tree-seq (copy-var tree-seq)
   'type (copy-var type)
   'true? (copy-var true?)
   'to-array (copy-var to-array)
   'update (copy-var update)
   'update-in (copy-var update-in)
   'uri? (copy-var uri?)
   'uuid? (copy-var uuid?)
   'unchecked-inc-int (copy-var unchecked-inc-int)
   'unchecked-long (copy-var unchecked-long)
   'unchecked-negate (copy-var unchecked-negate)
   'unchecked-remainder-int (copy-var unchecked-remainder-int)
   'unchecked-subtract-int (copy-var unchecked-subtract-int)
   'unsigned-bit-shift-right (copy-var unsigned-bit-shift-right)
   'unchecked-float (copy-var unchecked-float)
   'unchecked-add-int (copy-var unchecked-add-int)
   'unchecked-double (copy-var unchecked-double)
   'unchecked-multiply-int (copy-var unchecked-multiply-int)
   'unchecked-int (copy-var unchecked-int)
   'unchecked-multiply (copy-var unchecked-multiply)
   'unchecked-dec-int (copy-var unchecked-dec-int)
   'unchecked-add (copy-var unchecked-add)
   'unreduced (copy-var unreduced)
   'unchecked-divide-int (copy-var unchecked-divide-int)
   'unchecked-subtract (copy-var unchecked-subtract)
   'unchecked-negate-int (copy-var unchecked-negate-int)
   'unchecked-inc (copy-var unchecked-inc)
   'unchecked-char (copy-var unchecked-char)
   'unchecked-byte (copy-var unchecked-byte)
   'unchecked-short (copy-var unchecked-short)
   'underive (with-meta hierarchies/underive* {:sci.impl/op :needs-ctx})
   'val (copy-var val)
   'vals (copy-var vals)
   'var? sci.impl.vars/var?
   'vary-meta (copy-var vary-meta)
   'vec (copy-var vec)
   'vector (copy-var vector)
   'vector? (copy-var vector?)
   'volatile! (copy-var volatile!)
   'vreset! (copy-var vreset!)
   'vswap! (copy-var vswap!*)
   'when-first (macrofy when-first*)
   'when-let (macrofy when-let*)
   'when (macrofy when*)
   'when-not (macrofy when-not*)
   'with-meta (copy-var with-meta)
   'with-open (macrofy with-open*)
   ;; 'with-redefs (macrofy vars/with-redefs)
   ;; 'with-redefs-fn vars/with-redefs-fn
   'zipmap (copy-var zipmap)
   'zero? (copy-var zero?)
   #?@(:clj ['+' (copy-var +')
             '-' (copy-var -')
             '*' (copy-var *')
             'boolean-array (copy-var boolean-array)
             'bound? (copy-var bound?)
             'byte-array (copy-var byte-array)
             'bigint (copy-var bigint)
             'bytes? (copy-var bytes?)
             'biginteger (copy-var biginteger)
             'bigdec (copy-var bigdec)
             'char-array (copy-var char-array)
             'char-escape-string (copy-var char-escape-string)
             'char-name-string (copy-var char-name-string)
             'class (copy-var class)
             'dec' (copy-var dec')
             'decimal? (copy-var decimal?)
             'denominator (copy-var denominator)
             'format (copy-var format)
             'float-array (copy-var float-array)
             'inc' (copy-var inc')
             'line-seq (copy-var line-seq)
             'num (copy-var num)
             'namespace-munge (copy-var namespace-munge)
             'numerator (copy-var numerator)
             'replicate (copy-var replicate)
             'rational? (copy-var rational?)
             'ratio? (copy-var ratio?)
             'rationalize (copy-var rationalize)
             'seque (copy-var seque)
             'xml-seq (copy-var xml-seq)])})

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

(defn doc
  [_ _ sym]
  `(println (-> (resolve '~sym) meta :doc)))

(def clojure-repl
  {'dir-fn (with-meta dir-fn {:sci.impl/op :needs-ctx})
   'dir (macrofy dir)
   'doc (macrofy doc)})

(defn apply-template
  [argv expr values]
  (assert (vector? argv))
  (assert (every? symbol? argv))
  (walk/postwalk-replace (zipmap argv values) expr))

(defn do-template
  [_ _ argv expr & values]
  (let [c (count argv)]
    `(do ~@(map (fn [a] (apply-template argv expr a))
                (partition c values)))))

(def clojure-template
  {'apply-template apply-template
   'do-template (macrofy do-template)})

(def clojure-string-namespace (vars/->SciNamespace 'clojure.string))
(def clojure-set-namespace (vars/->SciNamespace 'clojure.set))
(def clojure-walk-namespace (vars/->SciNamespace 'clojure.walk))
(def clojure-edn-namespace (vars/->SciNamespace 'clojure.edn))

(def namespaces
  {'clojure.core clojure-core
   'clojure.string {'blank? (copy-var str/blank? clojure-string-namespace)
                    'capitalize (copy-var str/capitalize clojure-string-namespace)
                    'ends-with? (copy-var str/ends-with? clojure-string-namespace)
                    'escape (copy-var str/escape clojure-string-namespace)
                    'includes? (copy-var str/includes? clojure-string-namespace)
                    'index-of (copy-var str/index-of clojure-string-namespace)
                    'join (copy-var str/join clojure-string-namespace)
                    'last-index-of (copy-var str/last-index-of clojure-string-namespace)
                    'lower-case (copy-var str/lower-case clojure-string-namespace)
                    'replace (copy-var str/replace clojure-string-namespace)
                    'replace-first (copy-var str/replace-first clojure-string-namespace)
                    'reverse (copy-var str/reverse clojure-string-namespace)
                    'split (copy-var str/split clojure-string-namespace)
                    'split-lines (copy-var str/split-lines clojure-string-namespace)
                    'starts-with? (copy-var str/starts-with? clojure-string-namespace)
                    'trim (copy-var str/trim clojure-string-namespace)
                    'trim-newline (copy-var str/trim-newline clojure-string-namespace)
                    'triml (copy-var str/triml clojure-string-namespace)
                    'trimr (copy-var str/trimr clojure-string-namespace)
                    'upper-case (copy-var str/upper-case clojure-string-namespace)
                    #?@(:clj ['re-quote-replacement (copy-var str/re-quote-replacement clojure-string-namespace)])}
   'clojure.set {'difference (copy-var set/difference clojure-set-namespace)
                 'index (copy-var set/index clojure-set-namespace)
                 'intersection (copy-var set/intersection clojure-set-namespace)
                 'join (copy-var set/join clojure-set-namespace)
                 'map-invert (copy-var set/map-invert clojure-set-namespace)
                 'project (copy-var set/project clojure-set-namespace)
                 'rename (copy-var set/rename clojure-set-namespace)
                 'rename-keys (copy-var set/rename-keys clojure-set-namespace)
                 'select (copy-var set/select clojure-set-namespace)
                 'subset? (copy-var set/subset? clojure-set-namespace)
                 'superset? (copy-var set/superset? clojure-set-namespace)
                 'union (copy-var set/union clojure-set-namespace)}
   'clojure.walk {'walk (copy-var clojure.walk/walk clojure-walk-namespace)
                  'postwalk (copy-var clojure.walk/postwalk clojure-walk-namespace)
                  'prewalk (copy-var clojure.walk/prewalk clojure-walk-namespace)
                  #?@(:clj ['postwalk-demo (copy-var clojure.walk/postwalk-demo clojure-walk-namespace)
                            'prewalk-demo (copy-var clojure.walk/prewalk-demo clojure-walk-namespace)])
                  'keywordize-keys (copy-var clojure.walk/keywordize-keys clojure-walk-namespace)
                  'stringify-keys (copy-var clojure.walk/stringify-keys clojure-walk-namespace)
                  'prewalk-replace (copy-var clojure.walk/prewalk-replace clojure-walk-namespace)
                  'postwalk-replace (copy-var clojure.walk/postwalk-replace clojure-walk-namespace)}
   'clojure.template clojure-template
   'clojure.repl clojure-repl
   'clojure.edn {'read (copy-var edn/read clojure-edn-namespace)
                 'read-string (copy-var edn/read-string clojure-edn-namespace)}})

(def aliases
  '{str clojure.string
    set clojure.set})
