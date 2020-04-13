(ns sci.impl.namespaces
  {:no-doc true}
  (:refer-clojure :exclude [ex-message ex-cause eval read-string require use])
  (:require
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.tools.reader.reader-types :as r]
   [clojure.walk :as walk]
   [sci.impl.hierarchies :as hierarchies]
   [sci.impl.io :as io]
   [sci.impl.macros :as macros]
   [sci.impl.multimethods :as mm]
   [sci.impl.parser :as parser]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars])
  #?(:cljs (:require-macros [sci.impl.namespaces :refer [copy-var copy-core-var]])))

(def clojure-core-ns (vars/->SciNamespace 'clojure.core nil))

(macros/deftime
  ;; Note: self hosted CLJS can't deal with multi-arity macros so this macro is split in 2
  (defmacro copy-var
    ([sym ns]
     `(let [m# (-> (var ~sym) meta)]
        (vars/->SciVar ~sym '~sym {:doc (:doc m#)
                                   :name (:name m#)
                                   :arglists (:arglists m#)
                                   :ns ~ns
                                   :sci.impl/built-in true}))))
  (defmacro copy-core-var
    ([sym]
     `(copy-var ~sym clojure-core-ns)
     #_`(let [m# (-> (var ~sym) meta)]
        (vars/->SciVar ~sym '~sym {:doc (:doc m#)
                                   :name (:name m#)
                                   :arglists (:arglists m#)
                                   :ns clojure-core-ns
                                   :sci.impl/built-in true})))))

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
       (~utils/allowed-loop [~i 0]
         (when (< ~i n#)
           ~@body
           (~utils/allowed-recur (unchecked-inc ~i)))))))

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

(defn if-some*
  ([&form &env bindings then]
   (if-some* &form &env bindings then nil))
  ([_&form _&env bindings then else & _oldform]
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (nil? temp#)
          ~else
          (let [~form temp#]
            ~then))))))

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

(defn when-some* [_ _ bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (nil? temp#)
         nil
         (let [~form temp#]
           ~@body)))))

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

(defn while*
  [_ _ test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

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
  (utils/namespace-object (:env ctx) ns-sym false nil))

(defn sci-the-ns [ctx x]
  (if (instance? sci.impl.vars.SciNamespace x) x
      (sci-find-ns ctx x)))

(defn sci-ns-aliases [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        aliases (get-in @(:env ctx) [:namespaces name :aliases])]
    (zipmap (keys aliases)
            (map (fn [sym]
                   (vars/->SciNamespace sym nil))
                 (vals aliases)))))

(defn sci-ns-interns [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (dissoc m :aliases :imports :obj)]
    m))

(defn sci-ns-publics [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (dissoc m :aliases :imports :obj)]
    (into {} (keep (fn [[k v]]
                     (when-not (:private (meta v))
                       [k v]))
                   m))))

(defn sci-ns-imports [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        env @(:env ctx)
        global-imports (:imports env)
        namespace-imports (get-in env [:namespaces name :imports])
        class-opts (:class->opts ctx)
        all-aliased (concat (keys global-imports) (keys namespace-imports))
        all-imports (concat (vals global-imports) (vals namespace-imports))]
    (zipmap all-aliased (map (comp :class #(get class-opts %)) all-imports))))

(defn sci-ns-refers [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        env @(:env ctx)
        the-ns (get-in env [:namespaces name])
        the-ns (dissoc the-ns :aliases :imports :obj)
        clojure-core (get-in env [:namespaces 'clojure.core])
        clojure-core (dissoc clojure-core :aliases :imports :obj)]
    (merge the-ns clojure-core)))

(defn sci-ns-map [ctx sci-ns]
  (merge (sci-ns-interns ctx sci-ns)
         (sci-ns-refers ctx sci-ns)
         (sci-ns-imports ctx sci-ns)))

(defn sci-ns-unmap [ctx sci-ns sym]
  (assert (symbol? sym)) ; protects :aliases, :imports, :obj, etc.
  (swap! (:env ctx)
         (fn [env]
           (let [sci-ns (sci-the-ns ctx sci-ns)
                 name (sci-ns-name sci-ns)
                 m (get-in env [:namespaces name])]
             (assoc-in env [:namespaces name] (dissoc m sym)))))
  nil)

(defn sci-all-ns [ctx]
  (let [env (:env ctx)]
    (map #(utils/namespace-object env % true nil) (keys (get @env :namespaces)))))

;;;; End namespaces

;;;; Eval and read-string

(defn read-string
  ([sci-ctx s]
   (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
     (parser/parse-next sci-ctx reader)))
  #_([opts s] (clojure.lang.RT/readString s opts)))

(defn eval [sci-ctx form]
  (@utils/eval-form-state sci-ctx form))

;;;; End eval and read-string

(defn require [sci-ctx & args]
  (apply @utils/eval-require-state sci-ctx args))

(defn use [sci-ctx & args]
  (apply @utils/eval-use-state sci-ctx args))

(def clojure-core
  {:obj clojure-core-ns
   '*ns* vars/current-ns
   ;; io
   '*in* io/in
   '*out* io/out
   '*err* io/err
   '*file* vars/current-file
   '*print-length* io/print-length
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
   'get-method (copy-core-var get-method)
   'methods (copy-core-var methods)
   'multi-fn-add-method-impl mm/multi-fn-add-method-impl
   'multi-fn?-impl mm/multi-fn?-impl
   'multi-fn-impl mm/multi-fn-impl
   'prefer-method (copy-core-var prefer-method)
   'prefers (copy-core-var prefers)
   'remove-method (copy-core-var remove-method)
   'remove-all-methods (copy-core-var remove-all-methods)
   ;; end multimethods
   '.. (macrofy double-dot)
   '= (copy-core-var =)
   '< (copy-core-var <)
   '<= (copy-core-var <=)
   '> (copy-core-var >)
   '>= (copy-core-var >=)
   '+ (copy-core-var +)
   '- (copy-core-var -)
   '* (copy-core-var *)
   '/ (copy-core-var /)
   '== (copy-core-var ==)
   '->> (macrofy ->>*)
   'add-watch (copy-core-var add-watch)
   'aget (copy-core-var aget)
   'alias (with-meta sci-alias {:sci.impl/op :needs-ctx})
   'all-ns (with-meta sci-all-ns {:sci.impl/op :needs-ctx})
   'alter-meta! (copy-core-var alter-meta!)
   'alter-var-root (copy-core-var vars/alter-var-root)
   'ancestors (with-meta hierarchies/ancestors* {:sci.impl/op :needs-ctx})
   'aset (copy-core-var aset)
   'alength (copy-core-var alength)
   'any? (copy-core-var any?)
   'apply (copy-core-var apply)
   'array-map (copy-core-var array-map)
   'assert (with-meta assert* {:sci/macro true})
   'assoc (copy-core-var assoc)
   'assoc! (copy-core-var assoc!)
   'assoc-in (copy-core-var assoc-in)
   'associative? (copy-core-var associative?)
   'atom (copy-core-var atom)
   'binding (with-meta vars/binding {:sci/macro true})
   'binding-conveyor-fn vars/binding-conveyor-fn
   'bit-and-not (copy-core-var bit-and-not)
   'bit-set (copy-core-var bit-set)
   'bit-shift-left (copy-core-var bit-shift-left)
   'bit-shift-right (copy-core-var bit-shift-right)
   'bit-xor (copy-core-var bit-xor)
   'boolean (copy-core-var boolean)
   'boolean? (copy-core-var boolean?)
   'booleans (copy-core-var booleans)
   'butlast (copy-core-var butlast)
   'bytes (copy-core-var bytes)
   'bit-test (copy-core-var bit-test)
   'bit-and (copy-core-var bit-and)
   'bounded-count (copy-core-var bounded-count)
   'bit-or (copy-core-var bit-or)
   'bit-flip (copy-core-var bit-flip)
   'bit-not (copy-core-var bit-not)
   'byte (copy-core-var byte)
   'cat (copy-core-var cat)
   'char (copy-core-var char)
   'char? (copy-core-var char?)
   #?@(:cljs ['clj->js (copy-core-var clj->js)])
   'cond (macrofy cond*)
   'cond-> (macrofy cond->*)
   'cond->> (macrofy cond->>*)
   'condp (macrofy condp*)
   'conj (copy-core-var conj)
   'conj! (copy-core-var conj!)
   'cons (copy-core-var cons)
   'contains? (copy-core-var contains?)
   'count (copy-core-var count)
   'cycle (copy-core-var cycle)
   'comp (copy-core-var comp)
   'concat (copy-core-var concat)
   'comparator (copy-core-var comparator)
   'coll? (copy-core-var coll?)
   'compare (copy-core-var compare)
   'complement (copy-core-var complement)
   'constantly (copy-core-var constantly)
   'chars (copy-core-var chars)
   'completing (copy-core-var completing)
   'counted? (copy-core-var counted?)
   'chunk (copy-core-var chunk)
   'chunk-append (copy-core-var chunk-append)
   'chunk-buffer (copy-core-var chunk-buffer)
   'chunk-cons (copy-core-var chunk-cons)
   'chunk-first (copy-core-var chunk-first)
   'chunk-rest (copy-core-var chunk-rest)
   'chunk-next (copy-core-var chunk-next)
   'chunked-seq? (copy-core-var chunked-seq?)
   'dec (copy-core-var dec)
   'dedupe (copy-core-var dedupe)
   'defn- (macrofy defn-*)
   'defonce (macrofy defonce*)
   'delay (macrofy delay*)
   #?@(:clj ['deliver (copy-core-var deliver)])
   'deref (copy-core-var deref)
   'derive (with-meta hierarchies/derive* {:sci.impl/op :needs-ctx})
   'descendants (with-meta hierarchies/descendants* {:sci.impl/op :needs-ctx})
   'dissoc (copy-core-var dissoc)
   'distinct (copy-core-var distinct)
   'distinct? (copy-core-var distinct?)
   'disj (copy-core-var disj)
   'doall (copy-core-var doall)
   'dorun (copy-core-var dorun)
   'dotimes (macrofy dotimes*)
   'doto (macrofy doto*)
   'double (copy-core-var double)
   'double? (copy-core-var double?)
   'drop (copy-core-var drop)
   'drop-last (copy-core-var drop-last)
   'drop-while (copy-core-var drop-while)
   'doubles (copy-core-var doubles)
   'eduction (copy-core-var eduction)
   'empty (copy-core-var empty)
   'empty? (copy-core-var empty?)
   'eval (with-meta eval {:sci.impl/op :needs-ctx})
   'even? (copy-core-var even?)
   'every? (copy-core-var every?)
   'every-pred (copy-core-var every-pred)
   'ensure-reduced (copy-core-var ensure-reduced)
   'ex-data (copy-core-var ex-data)
   'ex-info (copy-core-var ex-info)
   'ex-message (copy-core-var ex-message)
   'ex-cause (copy-core-var ex-cause)
   'find-ns (with-meta sci-find-ns {:sci.impl/op :needs-ctx})
   'first (copy-core-var first)
   'float? (copy-core-var float?)
   'floats (copy-core-var floats)
   'fnil (copy-core-var fnil)
   'fnext (copy-core-var fnext)
   'ffirst (copy-core-var ffirst)
   'flatten (copy-core-var flatten)
   'false? (copy-core-var false?)
   'filter (copy-core-var filter)
   'filterv (copy-core-var filterv)
   'find (copy-core-var find)
   'frequencies (copy-core-var frequencies)
   'float (copy-core-var float)
   'fn? (copy-core-var fn?)
   'get (copy-core-var get)
   'get-thread-binding-frame-impl vars/get-thread-binding-frame
   'get-in (copy-core-var get-in)
   'group-by (copy-core-var group-by)
   'gensym (copy-core-var gensym)
   'has-root-impl (copy-core-var has-root-impl)
   'hash (copy-core-var hash)
   'hash-map (copy-core-var hash-map)
   'hash-set (copy-core-var hash-set)
   'hash-unordered-coll (copy-core-var hash-unordered-coll)
   'ident? (copy-core-var ident?)
   'identical? (copy-core-var identical?)
   'identity (copy-core-var identity)
   'if-let (macrofy if-let*)
   'if-some (macrofy if-some*)
   'if-not (macrofy if-not*)
   'ifn? (copy-core-var ifn?)
   'inc (copy-core-var inc)
   'inst? (copy-core-var inst?)
   'instance? (copy-core-var instance?)
   'int-array (copy-core-var int-array)
   'interleave (copy-core-var interleave)
   'into (copy-core-var into)
   'iterate (copy-core-var iterate)
   'int (copy-core-var int)
   'int? (copy-core-var int?)
   'interpose (copy-core-var interpose)
   'indexed? (copy-core-var indexed?)
   'integer? (copy-core-var integer?)
   'ints (copy-core-var ints)
   'into-array (copy-core-var into-array)
   'isa? (with-meta hierarchies/isa?* {:sci.impl/op :needs-ctx})
   #?@(:cljs ['js->clj (copy-core-var js->clj)])
   #?@(:cljs ['js-obj (copy-core-var js-obj)])
   'juxt (copy-core-var juxt)
   'keep (copy-core-var keep)
   'keep-indexed (copy-core-var keep-indexed)
   'key (copy-core-var key)
   'keys (copy-core-var keys)
   'keyword (copy-core-var keyword)
   'keyword? (copy-core-var keyword?)
   'last (copy-core-var last)
   'letfn (macrofy letfn*)
   'long (copy-core-var long)
   'list (copy-core-var list)
   'list? (copy-core-var list?)
   'longs (copy-core-var longs)
   'list* (copy-core-var list*)
   'long-array (copy-core-var long-array)
   'make-array (copy-core-var make-array)
   'make-hierarchy (copy-core-var make-hierarchy)
   'map (copy-core-var map)
   'map? (copy-core-var map?)
   'map-indexed (copy-core-var map-indexed)
   'map-entry? (copy-core-var map-entry?)
   'mapv (copy-core-var mapv)
   'mapcat (copy-core-var mapcat)
   'max (copy-core-var max)
   'max-key (copy-core-var max-key)
   'meta (copy-core-var meta)
   'merge (copy-core-var merge)
   'merge-with (copy-core-var merge-with)
   'min (copy-core-var min)
   'min-key (copy-core-var min-key)
   'munge (copy-core-var munge)
   'mod (copy-core-var mod)
   'name (copy-core-var name)
   'namespace (copy-core-var namespace)
   ;; 'newline newline
   'nfirst (copy-core-var nfirst)
   'not (copy-core-var not)
   'not= (copy-core-var not=)
   'not-every? (copy-core-var not-every?)
   'neg? (copy-core-var neg?)
   'neg-int? (copy-core-var neg-int?)
   'nth (copy-core-var nth)
   'nthnext (copy-core-var nthnext)
   'nthrest (copy-core-var nthrest)
   'nil? (copy-core-var nil?)
   'nat-int? (copy-core-var nat-int?)
   'number? (copy-core-var number?)
   'not-empty (copy-core-var not-empty)
   'not-any? (copy-core-var not-any?)
   'next (copy-core-var next)
   'nnext (copy-core-var nnext)
   'ns-aliases (with-meta sci-ns-aliases {:sci.impl/op :needs-ctx})
   'ns-imports (with-meta sci-ns-imports {:sci.impl/op :needs-ctx})
   'ns-interns (with-meta sci-ns-interns {:sci.impl/op :needs-ctx})
   'ns-publics (with-meta sci-ns-publics {:sci.impl/op :needs-ctx})
   'ns-refers (with-meta sci-ns-refers {:sci.impl/op :needs-ctx})
   'ns-map (with-meta sci-ns-map {:sci.impl/op :needs-ctx})
   'ns-unmap (with-meta sci-ns-unmap {:sci.impl/op :needs-ctx})
   'ns-name sci-ns-name
   'odd? (copy-core-var odd?)
   'object-array (copy-core-var object-array)
   'parents (with-meta hierarchies/parents* {:sci.impl/op :needs-ctx})
   'peek (copy-core-var peek)
   'pop (copy-core-var pop)
   'pop-thread-bindings vars/pop-thread-bindings
   'pos? (copy-core-var pos?)
   'pos-int? (copy-core-var pos-int?)
   'partial (copy-core-var partial)
   'partition (copy-core-var partition)
   'partition-all (copy-core-var partition-all)
   'partition-by (copy-core-var partition-by)
   'persistent! (copy-core-var persistent!)
   'pr-str (copy-core-var pr-str)
   #?@(:clj ['promise (copy-core-var promise)])
   'prn-str (copy-core-var prn-str)
   'print-str (copy-core-var print-str)
   'push-thread-bindings vars/push-thread-bindings
   'qualified-ident? (copy-core-var qualified-ident?)
   'qualified-symbol? (copy-core-var qualified-symbol?)
   'qualified-keyword? (copy-core-var qualified-keyword?)
   'quot (copy-core-var quot)
   're-seq (copy-core-var re-seq)
   're-find (copy-core-var re-find)
   #?@(:clj ['re-groups (copy-core-var re-groups)])
   're-pattern (copy-core-var re-pattern)
   #?@(:clj ['re-matcher (copy-core-var re-matcher)])
   're-matches (copy-core-var re-matches)
   'rem (copy-core-var rem)
   'remove (copy-core-var remove)
   'require (with-meta require {:sci.impl/op :needs-ctx})
   'reset-meta! (copy-core-var reset-meta!)
   'rest (copy-core-var rest)
   'repeatedly (copy-core-var repeatedly)
   'reverse (copy-core-var reverse)
   'rand-int (copy-core-var rand-int)
   'rand-nth (copy-core-var rand-nth)
   'range (copy-core-var range)
   'record? (copy-core-var record?)
   'reduce (copy-core-var reduce)
   'reduce-kv (copy-core-var reduce-kv)
   'reduced (copy-core-var reduced)
   'reduced? (copy-core-var reduced?)
   'reset! (copy-core-var reset!)
   'reset-vals! (copy-core-var reset-vals!)
   'reset-thread-binding-frame-impl vars/reset-thread-binding-frame
   'reversible? (copy-core-var reversible?)
   'rsubseq (copy-core-var rsubseq)
   'reductions (copy-core-var reductions)
   'rand (copy-core-var rand)
   'read-string (with-meta read-string {:sci.impl/op :needs-ctx})
   'replace (copy-core-var replace)
   'rseq (copy-core-var rseq)
   'random-sample (copy-core-var random-sample)
   'repeat (copy-core-var repeat)
   'run! (copy-core-var run!)
   #?@(:clj ['satisfies? (copy-core-var satisfies?)])
   'set? (copy-core-var set?)
   'sequential? (copy-core-var sequential?)
   'select-keys (copy-core-var select-keys)
   'simple-keyword? (copy-core-var simple-keyword?)
   'simple-symbol? (copy-core-var simple-symbol?)
   'some? (copy-core-var some?)
   'some-> (macrofy some->*)
   'some->> (macrofy some->>*)
   'string? (copy-core-var string?)
   'str (copy-core-var str)
   'second (copy-core-var second)
   'set (copy-core-var set)
   'seq (copy-core-var seq)
   'seq? (copy-core-var seq?)
   'short (copy-core-var short)
   'shuffle (copy-core-var shuffle)
   'sort (copy-core-var sort)
   'sort-by (copy-core-var sort-by)
   'subs (copy-core-var subs)
   #?@(:clj ['supers (copy-core-var supers)])
   'symbol (copy-core-var symbol)
   'symbol? (copy-core-var symbol?)
   'special-symbol? (copy-core-var special-symbol?)
   'subvec (copy-core-var subvec)
   'some-fn (copy-core-var some-fn)
   'some (copy-core-var some)
   'split-at (copy-core-var split-at)
   'split-with (copy-core-var split-with)
   'sorted-set (copy-core-var sorted-set)
   'subseq (copy-core-var subseq)
   'sorted-set-by (copy-core-var sorted-set-by)
   'sorted-map-by (copy-core-var sorted-map-by)
   'sorted-map (copy-core-var sorted-map)
   'sorted? (copy-core-var sorted?)
   'simple-ident? (copy-core-var simple-ident?)
   'sequence (copy-core-var sequence)
   'seqable? (copy-core-var seqable?)
   'shorts (copy-core-var shorts)
   'swap! (copy-core-var swap!)
   'swap-vals! (copy-core-var swap-vals!)
   'tagged-literal (copy-core-var tagged-literal)
   'take (copy-core-var take)
   'take-last (copy-core-var take-last)
   'take-nth (copy-core-var take-nth)
   'take-while (copy-core-var take-while)
   'the-ns (with-meta sci-the-ns {:sci.impl/op :needs-ctx})
   'trampoline (copy-core-var trampoline)
   'transduce (copy-core-var transduce)
   'transient (copy-core-var transient)
   'tree-seq (copy-core-var tree-seq)
   'type (copy-core-var type)
   'true? (copy-core-var true?)
   'to-array (copy-core-var to-array)
   'update (copy-core-var update)
   'update-in (copy-core-var update-in)
   'uri? (copy-core-var uri?)
   'uuid? (copy-core-var uuid?)
   'unchecked-inc-int (copy-core-var unchecked-inc-int)
   'unchecked-long (copy-core-var unchecked-long)
   'unchecked-negate (copy-core-var unchecked-negate)
   'unchecked-remainder-int (copy-core-var unchecked-remainder-int)
   'unchecked-subtract-int (copy-core-var unchecked-subtract-int)
   'unsigned-bit-shift-right (copy-core-var unsigned-bit-shift-right)
   'unchecked-float (copy-core-var unchecked-float)
   'unchecked-add-int (copy-core-var unchecked-add-int)
   'unchecked-double (copy-core-var unchecked-double)
   'unchecked-multiply-int (copy-core-var unchecked-multiply-int)
   'unchecked-int (copy-core-var unchecked-int)
   'unchecked-multiply (copy-core-var unchecked-multiply)
   'unchecked-dec-int (copy-core-var unchecked-dec-int)
   'unchecked-add (copy-core-var unchecked-add)
   'unreduced (copy-core-var unreduced)
   'unchecked-divide-int (copy-core-var unchecked-divide-int)
   'unchecked-subtract (copy-core-var unchecked-subtract)
   'unchecked-negate-int (copy-core-var unchecked-negate-int)
   'unchecked-inc (copy-core-var unchecked-inc)
   'unchecked-char (copy-core-var unchecked-char)
   'unchecked-byte (copy-core-var unchecked-byte)
   'unchecked-short (copy-core-var unchecked-short)
   'underive (with-meta hierarchies/underive* {:sci.impl/op :needs-ctx})
   'use (with-meta use {:sci.impl/op :needs-ctx})
   'val (copy-core-var val)
   'vals (copy-core-var vals)
   'var? sci.impl.vars/var?
   'vary-meta (copy-core-var vary-meta)
   'vec (copy-core-var vec)
   'vector (copy-core-var vector)
   'vector? (copy-core-var vector?)
   'volatile! (copy-core-var volatile!)
   'vreset! (copy-core-var vreset!)
   'vswap! (copy-core-var vswap!*)
   'when-first (macrofy when-first*)
   'when-let (macrofy when-let*)
   'when-some (macrofy when-some*)
   'when (macrofy when*)
   'when-not (macrofy when-not*)
   'while (macrofy while*)
   'with-meta (copy-core-var with-meta)
   'with-open (macrofy with-open*)
   ;; 'with-redefs (macrofy vars/with-redefs)
   ;; 'with-redefs-fn vars/with-redefs-fn
   'zipmap (copy-core-var zipmap)
   'zero? (copy-core-var zero?)
   #?@(:clj ['+' (copy-core-var +')
             '-' (copy-core-var -')
             '*' (copy-core-var *')
             'boolean-array (copy-core-var boolean-array)
             'bound? (copy-core-var bound?)
             'byte-array (copy-core-var byte-array)
             'bigint (copy-core-var bigint)
             'bytes? (copy-core-var bytes?)
             'biginteger (copy-core-var biginteger)
             'bigdec (copy-core-var bigdec)
             'char-array (copy-core-var char-array)
             'char-escape-string (copy-core-var char-escape-string)
             'char-name-string (copy-core-var char-name-string)
             'class (copy-core-var class)
             'dec' (copy-core-var dec')
             'decimal? (copy-core-var decimal?)
             'denominator (copy-core-var denominator)
             'format (copy-core-var format)
             'float-array (copy-core-var float-array)
             'inc' (copy-core-var inc')
             'line-seq (copy-core-var line-seq)
             'num (copy-core-var num)
             'namespace-munge (copy-core-var namespace-munge)
             'numerator (copy-core-var numerator)
             'replicate (copy-core-var replicate)
             'rational? (copy-core-var rational?)
             'ratio? (copy-core-var ratio?)
             'rationalize (copy-core-var rationalize)
             'seque (copy-core-var seque)
             'xml-seq (copy-core-var xml-seq)])})

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

#_(defn- print-doc [{n :ns
                   nm :name
                   :keys [forms arglists special-form doc url macro spec]
                   :as m}]
  (println "-------------------------")
  (println (or spec (str (when n (str (ns-name n) "/")) nm)))
  (when forms
    (doseq [f forms]
      (print "  ")
      (prn f)))
  (when arglists
    (prn arglists))
  (cond
    special-form
    (do
      (println "Special Form")
      (println " " doc)
      (if (contains? m :url)
        (when url
          (println (str "\n  Please see http://clojure.org/" url)))
        (println (str "\n  Please see http://clojure.org/special_forms#" nm))))
    macro
    (println "Macro")
    spec
    (println "Spec"))
  (when doc (println " " doc))
  (when n
    (when-let [fnspec (spec/get-spec (symbol (str (ns-name n)) (name nm)))]
      (println "Spec")
      (doseq [role [:args :ret :fn]]
        (when-let [spec (get fnspec role)]
          (println " " (str (name role) ":") (spec/describe spec)))))))

(defn doc
  [_ _ sym]
  `(if-let [var# (resolve '~sym)]
     (let [m# (meta var#)
           arglists# (:arglists m#)
           doc# (:doc m#)
           macro?# (:macro m#)]
       (println "-------------------------")
       (println (str (when-let [ns* (:ns m#)]
                       (str (ns-name ns*) "/"))
                     (:name m#)))
       (when arglists# (println arglists#))
       (when macro?# (println "Macro"))
       (when doc# (println " " doc#)))
     (if-let [ns# (find-ns '~sym)]
       (let [m# (meta ns#)
             doc# (:doc m#)]
         (println "-------------------------")
         (println (str (ns-name ns#)))
         (when doc# (println " " doc#))))))

(def clojure-repl
  {:obj (vars/->SciNamespace 'clojure.repl nil)
   'dir-fn (with-meta dir-fn {:sci.impl/op :needs-ctx})
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
  {:obj (vars/->SciNamespace 'clojure.template nil)
   'apply-template apply-template
   'do-template (macrofy do-template)})

(def clojure-string-namespace (vars/->SciNamespace 'clojure.string nil))
(def clojure-set-namespace (vars/->SciNamespace 'clojure.set nil))
(def clojure-walk-namespace (vars/->SciNamespace 'clojure.walk nil))
(def clojure-edn-namespace (vars/->SciNamespace 'clojure.edn nil))

(def namespaces
  {'clojure.core clojure-core
   'clojure.string {:obj clojure-string-namespace
                    'blank? (copy-var str/blank? clojure-string-namespace)
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
   'clojure.set {:obj clojure-set-namespace
                 'difference (copy-var set/difference clojure-set-namespace)
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
   'clojure.walk {:obj clojure-walk-namespace
                  'walk (copy-var clojure.walk/walk clojure-walk-namespace)
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
   'clojure.edn {:obj clojure-edn-namespace
                 'read (copy-var edn/read clojure-edn-namespace)
                 'read-string (copy-var edn/read-string clojure-edn-namespace)}})

(def aliases
  '{str clojure.string
    set clojure.set})
