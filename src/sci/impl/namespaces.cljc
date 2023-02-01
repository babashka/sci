(ns sci.impl.namespaces
  {:no-doc true}
  (:refer-clojure :exclude [ex-message ex-cause eval read
                            read-string require
                            use load-string
                            *1 *2 *3 *e #?(:cljs type)
                            bound-fn* with-bindings*
                            vswap!
                            #?(:cljs this-as)
                            clojure-version
                            print-method
                            print-dup
                            #?(:cljs alter-meta!)
                            memfn])
  (:require
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   #?(:clj [clojure.java.io :as jio])
   #?(:clj [sci.impl.proxy :as proxy])
   #?(:clj [sci.impl.copy-vars :refer [copy-core-var copy-var macrofy new-var]]
      :cljs [sci.impl.copy-vars :refer [new-var]])
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [sci.impl.cljs]
   [sci.impl.core-protocols :as core-protocols]
   [sci.impl.deftype :as deftype]
   [sci.impl.destructure :as destructure]
   [sci.impl.doseq-macro :as doseq-macro]
   [sci.impl.fns :as fns]
   [sci.impl.for-macro :as for-macro]
   [sci.impl.hierarchies :as hierarchies]
   [sci.impl.io :as io]
   [sci.impl.macros :as macros]
   [sci.impl.multimethods :as mm]
   [sci.impl.parser :as parser]
   [sci.impl.protocols :as protocols]
   [sci.impl.read :as read :refer [load-string read read-string]]
   [sci.impl.records :as records]
   [sci.impl.reify :as reify]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer [eval]]
   [sci.impl.vars :as vars]
   [sci.lang]
   [sci.impl.load :as load])
  #?(:cljs (:require-macros
            [sci.impl.copy-vars :refer [copy-var copy-core-var macrofy]])))

#?(:clj (set! *warn-on-reflection* true))

(def clojure-core-ns sci.impl.utils/clojure-core-ns)

(defn ->*
  [_ _ x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

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

(defn as->*
  [_ _ expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

(defn comment*
  [_ _ & _body])

(defn dotimes*
  [_ _ bindings & body]
  (assert (vector? bindings))
  (assert (= 2 (count bindings)))
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (~sci.impl.utils/allowed-loop [~i 0]
                                     (when (< ~i n#)
                                       ~@body
                                       (~sci.impl.utils/allowed-recur (unchecked-inc ~i)))))))

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

(defn declare*
  "defs the supplied var names with no bindings, useful for making forward declarations."
  [_ _ & names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true)) names)))

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

(def assert-var (sci.impl.utils/dynamic-var '*assert* true {:ns clojure-core-ns}))

(defn assert*
  ([_&form _ x]
   (when @assert-var
     `(when-not ~x
        (throw (#?(:clj AssertionError. :cljs js/Error.) (str "Assert failed: " (pr-str '~x)))))))
  ([_&form _ x message]
   (when @assert-var
     `(when-not ~x
        (throw (#?(:clj AssertionError. :cljs js/Error.) (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

(defn areduce* [_ _ a idx ret init expr]
  `(let [a# ~a l# (alength a#)]
     (loop  [~idx 0 ~ret ~init]
       (if (< ~idx l#)
         (recur (unchecked-inc-int ~idx) ~expr)
         ~ret))))

(defn amap* [_ _ a idx ret expr]
  `(let [a# ~a l# (alength a#)
         ~ret (aclone a#)]
     (loop  [~idx 0]
       (if (< ~idx  l#)
         (do
           (aset ~ret ~idx ~expr)
           (recur (unchecked-inc ~idx)))
         ~ret))))

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
  (let [syms (map first fnspecs)]
    `(let ~(vec (interleave syms (repeat '(clojure.core/-new-var))))
       ~@(map (fn [sym fn-spec]
                `(clojure.core/alter-var-root ~sym (constantly (fn ~sym ~@(rest fn-spec)))))
              syms fnspecs)
       (let ~(vec (interleave syms (map (fn [sym]
                                          `(clojure.core/var-get ~sym))
                                        syms)))
         ~@body))))

(defn with-local-vars* [form _ name-vals-vec & body]
  (when-not (vector? name-vals-vec)
    (sci.impl.utils/throw-error-with-location (str "with-local-vars requires a vector for its bindings")
                                              form))
  (when-not (even? (count name-vals-vec))
    (sci.impl.utils/throw-error-with-location (str "with-local-vars requires an even number of forms in binding vector")
                                              form))
  `(let [~@(interleave (take-nth 2 name-vals-vec)
                       (repeat '(clojure.core/-new-dynamic-var)))]
     (clojure.core/push-thread-bindings (hash-map ~@name-vals-vec))
     (try
       ~@body
       (finally (clojure.core/pop-thread-bindings)))))

(defn vswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in."
  [_ _ vol f & args]
  (let [v vol]
    `(vreset! ~v (~f (deref ~v) ~@args))))

(def memfn
  "Expands into code that creates a fn that expects to be passed an
  object and any args and calls the named instance method on the
  object passing the args. Use when you want to treat a JavaScript
  method as a first-class fn."
  ^:sci/macro (fn [_ _ name & args]
                (let [t (with-meta (gensym "target")
                          (meta name))]
                  `(fn [~t ~@args]
                     (. ~t (~name ~@args))))))

(defn delay*
  [_ _ & body]
  #?(:clj `(new clojure.lang.Delay (fn [] ~@body))
     :cljs `(new cljs.core/Delay (fn [] ~@body))))

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

(defn lazy-cat* [_ _ & colls]
  `(concat ~@(map #(list `lazy-seq %) colls)))

(defn has-root-impl [sci-var]
  (sci.impl.vars/hasRoot sci-var))

;;;; Namespaces / vars

(defn sci-ns-name [^sci.lang.Namespace ns]
  (types/getName ns))

(defn sci-alias [ctx alias-sym ns-sym]
  (swap! (:env ctx)
         (fn [env]
           (let [current-ns (sci.impl.utils/current-ns-name)]
             (assoc-in env [:namespaces current-ns :aliases alias-sym] ns-sym))))
  nil)

(defn sci-create-ns [ctx ns-sym]
  (sci.impl.utils/namespace-object (:env ctx) ns-sym true nil))

(defn sci-find-ns [ctx ns-sym]
  (assert (symbol? ns-sym))
  (sci.impl.utils/namespace-object (:env ctx) ns-sym false nil))

(defn sci-in-ns [ctx ns-sym]
  (assert (symbol? ns-sym))
  #_(when-not (sci-find-ns ctx ns-sym)
      (sci-create-ns ctx ns-sym))
  (sci.impl.utils/set-namespace! ctx ns-sym {}))

(defn sci-the-ns [ctx x]
  (if (instance? #?(:clj sci.lang.Namespace
                    :cljs sci.lang/Namespace) x) x
      (or (sci-find-ns ctx x)
          (throw (new #?(:clj Exception :cljs js/Error)
                      (str "No namespace: " x " found"))))))

(defn sci-ns-aliases [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        aliases (get-in @(:env ctx) [:namespaces name :aliases])]
    (zipmap (keys aliases)
            (map (fn [sym]
                   (sci.lang/->Namespace sym nil))
                 (vals aliases)))))

(defn clean-ns [m]
  (dissoc m :aliases :imports :obj :refer :refers))

(defn sci-ns-interns [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (clean-ns m)]
    m))

(defn sci-ns-publics [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        m (get-in @(:env ctx) [:namespaces name])
        m (clean-ns m)]
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
        class-opts (:class->opts env)
        all-aliased (concat (keys global-imports) (keys namespace-imports))
        all-imports (concat (vals global-imports) (vals namespace-imports))]
    (zipmap all-aliased (map (comp :class #(get class-opts %)) all-imports))))

(defn sci-ns-refers [ctx sci-ns]
  (let [sci-ns (sci-the-ns ctx sci-ns)
        name (sci-ns-name sci-ns)
        env @(:env ctx)
        refers (get-in env [:namespaces name :refers])
        clojure-core (get-in env [:namespaces 'clojure.core])
        clojure-core (clean-ns clojure-core)]
    (merge clojure-core refers)))

(defn sci-ns-map [ctx sci-ns]
  (merge (sci-ns-interns ctx sci-ns)
         (sci-ns-refers ctx sci-ns)
         (sci-ns-imports ctx sci-ns)))

(defn sci-ns-unmap [ctx sci-ns sym]
  (assert (symbol? sym)) ; protects :aliases, :imports, :obj, etc.
  (swap! (:env ctx)
         (fn [env]
           (let [sci-ns (sci-the-ns ctx sci-ns)
                 name (sci-ns-name sci-ns)]
             (update-in env [:namespaces name]
                        (fn [the-ns-map]
                          (cond (contains? (:refers the-ns-map) sym)
                                (-> (update the-ns-map :refers dissoc sym)
                                    ;; remove lingering var that may have been
                                    ;; overwritten before, see #637
                                    (dissoc the-ns-map sym))
                                (contains? the-ns-map sym)
                                (dissoc the-ns-map sym)
                                (or
                                 (contains? (:imports env) sym)
                                 (contains? (:imports the-ns-map) sym))
                                ;; nil marks the imported class as unmapped
                                (update the-ns-map :imports assoc sym nil)
                                :else the-ns-map))))))
  nil)

(defn sci-ns-unalias [ctx sci-ns sym]
  (swap! (:env ctx)
         (fn [env]
           (update-in env [:namespaces (sci-ns-name (sci-the-ns ctx sci-ns)) :aliases] dissoc sym)))
  nil)

(defn sci-all-ns [ctx]
  (let [env (:env ctx)
        namespaces (get @env :namespaces)
        public (remove (fn [[_ v]]
                         (:private v)) namespaces)]
    (map #(sci.impl.utils/namespace-object env % true nil) (keys public))))

(defn sci-remove-ns [ctx sym]
  (let [env (:env ctx)]
    (swap! env update :namespaces dissoc sym)
    nil))

(defn sci-intern
  ;; in this case the var will become unbound
  ([ctx ns var-sym]
   (let [ns (sci-the-ns ctx ns)
         ns-name (sci-ns-name ns)
         env (:env ctx)]
     (or (get-in @env [:namespaces ns-name var-sym])
         (let [var-name (symbol (str ns-name) (str var-sym))
               new-var (sci.impl.utils/new-var var-name nil (assoc (meta var-sym) :ns ns))]
           (sci.impl.vars/unbind new-var)
           (swap! env assoc-in [:namespaces ns-name var-sym] new-var)
           new-var))))
  ([ctx ns var-sym val]
   (let [ns (sci-the-ns ctx ns)
         ns-name (sci-ns-name ns)
         env (:env ctx)]
     (or (when-let [v (get-in @env [:namespaces ns-name var-sym])]
           (sci.impl.vars/bindRoot v val)
           v)
         (let [var-name (symbol (str ns-name) (str var-sym))
               new-var (sci.impl.utils/new-var var-name val (assoc (meta var-sym) :ns ns))]
           (swap! env assoc-in [:namespaces ns-name var-sym] new-var)
           new-var)))))

(defn sci-bound?
  [sci-var]
  ;; see https://github.com/clojure/clojure/blob/cbb3fdf787a00d3c1443794b97ed7fe4bef8e888/src/jvm/clojure/lang/Var.java#L190
  (or (sci.impl.vars/hasRoot sci-var)
      (some? (sci.impl.vars/get-thread-binding sci-var))
      false))

;;;; End eval and read-string

;;;; Require + resolve

(defn require [sci-ctx & args]
  (apply @sci.impl.utils/eval-require-state sci-ctx args))

(defn use [sci-ctx & args]
  (apply @sci.impl.utils/eval-use-state sci-ctx args))

(defn sci-resolve
  ([sci-ctx sym]
   (@sci.impl.utils/eval-resolve-state sci-ctx (:bindings sci-ctx) sym))
  ([sci-ctx env sym]
   (@sci.impl.utils/eval-resolve-state sci-ctx (:bindings sci-ctx) env sym)))

(defn sci-refer [sci-ctx & args]
  (apply @sci.impl.utils/eval-refer-state sci-ctx args))

(defn sci-refer-clojure [_ _ & filters]
  `(clojure.core/refer '~'clojure.core ~@filters))

(defn sci-ns-resolve
  ([sci-ctx ns sym]
   (sci.impl.vars/with-bindings {sci.impl.utils/current-ns (sci-the-ns sci-ctx ns)}
     (sci-resolve sci-ctx sym)))
  ([sci-ctx ns env sym]
   (sci.impl.vars/with-bindings {sci.impl.utils/current-ns (sci-the-ns sci-ctx ns)}
     (sci-resolve sci-ctx env sym))))

(defn sci-requiring-resolve
  ([sci-ctx sym]
   (if (qualified-symbol? sym)
     (or (sci-resolve sci-ctx sym)
         (let [namespace (-> sym namespace symbol)]
           (require sci-ctx namespace)
           (sci-resolve sci-ctx sym)))
     (throw (new #?(:clj IllegalArgumentException
                    :cljs js/Error)
                 (str "Not a qualified symbol: " sym))))))

(defn sci-find-var [sci-ctx sym]
  (if (qualified-symbol? sym)
    (let [nsname (-> sym namespace symbol)
          sym' (-> sym name symbol)]
      (if-let [namespace (-> sci-ctx :env deref :namespaces (get nsname))]
        (get namespace sym')
        (throw (new #?(:clj IllegalArgumentException
                       :cljs js/Error)
                    (str "No such namespace: " nsname)))))
    (throw (new #?(:clj IllegalArgumentException
                   :cljs js/Error)
                (str "Not a qualified symbol: " sym)))))

;;;; End require + resolve

;;;; Binding vars

(defn with-bindings*
  "Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then calls f with the supplied arguments.
  Pops the installed bindings after f returned. Returns whatever f returns."
  [binding-map f & args]
  ;; important: outside try
  (sci.impl.vars/push-thread-bindings binding-map)
  (try
    (apply f args)
    (finally
      (sci.impl.vars/pop-thread-bindings))))

(defn sci-with-bindings
  [_ _ binding-map & body]
  `(clojure.core/with-bindings* ~binding-map (fn [] ~@body)))

(defn sci-binding
  [form _ bindings & body]
  (when-not (vector? bindings)
    (sci.impl.utils/throw-error-with-location (str "binding requires a vector for its bindings")
                                              form))
  (when-not (even? (count bindings))
    (sci.impl.utils/throw-error-with-location (str "binding requires an even number of forms in binding vector")
                                              form))
  (let [var-ize (fn [var-vals]
                  (loop [ret [] vvs (seq var-vals)]
                    (if vvs
                      (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                              (next (next vvs)))
                      (seq ret))))]
    `(let []
       ;; important: outside try
       (clojure.core/push-thread-bindings (hash-map ~@(var-ize bindings)))
       (try
         ~@body
         (finally
           (clojure.core/pop-thread-bindings))))))

(defn bound-fn*
  "Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place."
  [f]
  (let [bindings (sci.impl.vars/get-thread-bindings)]
    (fn [& args]
      (apply with-bindings* bindings f args))))

(defn sci-bound-fn
  "Returns a function defined by the given fntail, which will install the
  same bindings in effect as in the thread at the time bound-fn was called.
  This may be used to define a helper function which runs on a different
  thread, but needs the same bindings in place."
  [_ _ & fntail]
  `(clojure.core/bound-fn* (fn ~@fntail)))

(defn sci-thread-bound? [& vars]
  (every? #(sci.impl.vars/get-thread-binding %) vars))

(defn sci-with-redefs-fn
  [binding-map func]
  (let [root-bind (fn [m]
                    (doseq [[a-var a-val] m]
                      (sci.impl.vars/bindRoot a-var a-val)))
        old-vals (zipmap (keys binding-map)
                         (map #(sci.impl.vars/getRawRoot %) (keys binding-map)))]
    (try
      (root-bind binding-map)
      (func)
      (finally
        (root-bind old-vals)))))

(defn sci-with-redefs
  [_ _ bindings & body]
  `(clojure.core/with-redefs-fn
     ~(zipmap (map #(list `var %) (take-nth 2 bindings))
              (take-nth 2 (next bindings)))
     (fn [] ~@body)))

;;;; End binding vars

;;;; Patch for symbol to make it work with sci vars

(defn symbol*
  "Returns a Symbol with the given namespace and name. Arity-1 works
  on strings, keywords, and vars."
  ([name]
   (if (sci.impl.utils/var? name) (let [m (meta name)
                                        ns (:ns m)
                                        nm (:name m)]
                                    (when (and ns nm)
                                      (symbol (str (sci-ns-name ns))
                                              (str (clojure.core/name nm)))))
       (symbol name)))
  ([ns name] (symbol ns name)))

;;;;

;;;; Macroexpand

(defn macroexpand* [ctx expr]
  (@sci.impl.utils/macroexpand* ctx expr))

(defn macroexpand-1* [ctx expr]
  (@sci.impl.utils/macroexpand-1* ctx expr))

;;;;

#?(:clj
   (def clojure-lang
     {:private true
      :obj (sci.lang/->Namespace 'clojure.lang nil)
      ;; IDeref as protocol instead of class
      'IDeref core-protocols/deref-protocol
      'deref core-protocols/deref
      ;; IAtom as protocol instead of class
      'IAtom core-protocols/swap-protocol
      'swap core-protocols/swap
      'reset core-protocols/reset
      'compareAndSet core-protocols/compareAndSet
      'IAtom2 core-protocols/iatom2-protocol
      'resetVals core-protocols/resetVals
      'swapVals core-protocols/swapVals}))

;;;; Record impl

(defn -create-type [data]
  (new sci.lang.Type data nil nil))

#_(defn -reg-key! [rec-type k v]
    (when (instance? sci.lang.Type rec-type)
      (types/setVal rec-type (assoc (types/getVal rec-type) k v))
      rec-type))

(def sci-impl-records
  {:obj (sci.lang/->Namespace 'sci.impl.records nil)
   :private true
   'toString sci.impl.records/to-string
   '-create-record-type -create-type
   ;; what do we use this for again?
   ;; '-reg-key! -reg-key!
   '->record-impl sci.impl.records/->record-impl})

(def sci-impl-deftype
  {:obj (sci.lang/->Namespace 'sci.impl.deftype nil)
   :private true
   'toString sci.impl.deftype/to-string
   '-create-type -create-type
   '->type-impl sci.impl.deftype/->type-impl
   '-inner-impl sci.impl.types/getVal
   '-mutate sci.impl.types/-mutate
   'type types/type-impl})

(def sci-impl-protocols
  {:obj (sci.lang/->Namespace 'sci.impl.protocols nil)
   :private true
   'type->str sci.impl.protocols/type->str})

;;;; REPL vars

(def *1 (copy-core-var *1))

(def *2 (copy-core-var *2))

(def *3 (copy-core-var *3))

(def *e (copy-core-var *e))

;;;; Patch for CLJS type

#?(:cljs
   (defn type [x]
     (or (get (meta x) :type)
         (cljs.core/type x))))

;;;; Clojure 1.11.0 kwargs

#?(:clj (defmacro when-<-clojure-1.11.0 [& body]
          (let [{:keys [:major :minor]} *clojure-version*]
            (when-not (or (> major 1)
                          (and (= major 1)
                               (>= minor 11)))
              `(do ~@body)))))

#?(:clj
   (when-<-clojure-1.11.0
    (defn seq-to-map-for-destructuring
      "Builds a map from a seq as described in
  https://clojure.org/reference/special_forms#keyword-arguments"
      {:added "1.11"}
      [s]
      (if (next s)
        (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array s))
        (if (seq s) (first s) clojure.lang.PersistentArrayMap/EMPTY)))))

;; #?(:cljs
;;    (defn -js-this []
;;      (js* "this")))

;; #?(:cljs
;;    (defn this-as
;;      [_ _ name & body]
;;      `(let [~name (clojure.core/-js-this)]
;;         ~@body)))

#?(:clj (def clojure-version-var
          (sci.impl.utils/dynamic-var
           '*clojure-version* (update clojure.core/*clojure-version*
                                      :qualifier (fn [qualifier]
                                                   (if qualifier
                                                     (str qualifier "-SCI")
                                                     "SCI")))
           {:ns clojure-core-ns
            :doc "The version info for Clojure core, as a map containing :major :minor \n  :incremental and :qualifier keys. Feature releases may increment \n  :minor and/or :major, bugfix releases will increment :incremental. \n  Possible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\""})))

#?(:clj (defn
          clojure-version
          "Returns clojure version as a printable string."
          []
          (let [*clojure-version* @clojure-version-var]
            (str (:major *clojure-version*)
                 "."
                 (:minor *clojure-version*)
                 (when-let [i (:incremental *clojure-version*)]
                   (str "." i))
                 (when-let [q (:qualifier *clojure-version*)]
                   (when (pos? (count q)) (str "-" q)))
                 (when (:interim *clojure-version*)
                   "-SNAPSHOT")))))

#?(:clj
   (defmulti print-method (fn [x _w] (type x))
     :hierarchy
     (reify clojure.lang.IRef
       (deref [_] (throw (java.lang.SecurityException.
                          "Print-method is not allowed by default since it mutates the global runtime. Add it to SCI ctx via {:namespaces {'clojure.core print-method}}"))))))

#?(:clj
   (defmulti print-dup (fn [x _w] (class x))
     :hierarchy
     (reify clojure.lang.IRef
       (deref [_] (throw (java.lang.SecurityException.
                          "Print-dup is not allowed by default since it mutates the global runtime. Add it to SCI ctx via {:namespaces {'clojure.core print-dup}}"))))))

#?(:cljs
   (defn alter-meta!
     "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
     [iref f & args]
     (let [m (meta iref)]
       (if-not (:sci/built-in m)
         (apply cljs.core/alter-meta! iref f args)
         (throw (ex-info (str "Built-in var " iref " is read-only.")
                         {:var iref}))))))

(defn- let** [expr _ bindings & body]
  (when-not (vector? bindings)
    (utils/throw-error-with-location "let requires a vector for its binding" expr))
  (when-not (even? (count bindings))
    (utils/throw-error-with-location "let requires an even number of forms in binding vector" expr))
  `(let* ~(destructure/destructure bindings)
         ~@body))

(defn loop**
  [expr _ bindings & body]
  (when-not (vector? bindings)
    (utils/throw-error-with-location "let requires a vector for its binding" expr))
  (when-not (even? (count bindings))
    (utils/throw-error-with-location "let requires an even number of forms in binding vector" expr))
  (let [db (if (every? symbol? bindings)
             bindings
             (destructure/destructure bindings))]
    (if (identical? db bindings)
      `(loop* ~bindings ~@body)
      (let [vs (take-nth 2 (drop 1 bindings))
            bs (take-nth 2 bindings)
            gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
            bfs (reduce (fn [ret [b v g]]
                          (if (symbol? b)
                            (conj ret g v)
                            (conj ret g v b g)))
                        [] (map vector bs vs gs))]
        `(let ~bfs
           (loop* ~(vec (interleave gs gs))
                  (let ~(vec (interleave bs gs))
                    ~@body)))))))

(defn or*
  "This is a macro for compatiblity. Only there for docs and macroexpand, faster impl in analyzer.cljc"
  ([_ _] nil)
  ([_ _ x] x)
  ([_ _ x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

(defn and*
  "This is a macro for compatiblity. Only there for docs and macroexpand, faster impl in analyzer.cljc"
  ([_ _] true)
  ([_ _ x] x)
  ([_ _ x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defn case**
  "This is a macro for compatiblity. Only there for docs and macroexpand, faster impl in analyzer.cljc"
  [_ _ & body] `(case* ~@body))

(defn loaded-libs** [syms]
  (utils/dynamic-var
   '*loaded-libs* (#?(:clj ref :cljs atom)
                   (into (sorted-set)
                         syms))
   {:doc "A ref to a sorted set of symbols representing loaded libs"
    :ns clojure-core-ns
    :private true
    :sci/built-in true}))

(defn loaded-libs* [ctx]
  (-> ctx :env deref :namespaces
      (get 'clojure.core)
      (get '*loaded-libs*)
      deref deref))

(defn -add-loaded-lib [ctx name]
  (load/add-loaded-lib (:env ctx) name)
  nil)

(defn ns*
  [_form _ name & references]
  (let [process-reference
        (fn [[kname & args :as expr]]
          (with-meta `(~(symbol "clojure.core" (clojure.core/name kname))
                       ~@(map #(list 'quote %) args))
            (meta expr)))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata   (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        #_#_gen-class-clause (first (filter #(= :gen-class (first %)) references))
        #_#_gen-class-call
          (when gen-class-clause
            (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
        references (remove #(= :gen-class (first %)) references)
                                        ;ns-effect (clojure.core/in-ns name)
        name-metadata (meta name)
        exp `(do
               (clojure.core/in-ns '~name)
               ~@(when name-metadata
                   `(clojure.core/alter-meta! (find-ns '~name) (constantly  ~name-metadata)))
               #_~@(when gen-class-call (list gen-class-call))
               #_~@(when (and (not= name 'clojure.core) (not-any? #(= :refer-clojure (first %)) references))
                     `((clojure.core/refer '~'clojure.core)))
               ~@(map process-reference references)
               (if (= '~name 'clojure.core)
                 nil
                 (do (clojure.core/-add-loaded-lib '~name) nil)))]
    ;;(println exp)
    exp))

(defn lazy-seq*
  [_ _ & body]
  #?(:clj  (list 'new 'clojure.lang.LazySeq (list* '^{:once true} fn* [] body))
     :cljs `(new cljs.core/LazySeq nil (fn [] ~@body) nil nil)))

(macros/usetime

 (def clojure-core
   {:obj clojure-core-ns
    '*ns* sci.impl.utils/current-ns
    ;; io
    '*in* sci.impl.io/in
    '*out* sci.impl.io/out
    '*err* sci.impl.io/err
    '*file* sci.impl.utils/current-file
    '*flush-on-newline* sci.impl.io/flush-on-newline
    #?@(:cljs ['*print-fn* sci.impl.io/print-fn
               '*print-err-fn* sci.impl.io/print-err-fn])
    '*print-length* sci.impl.io/print-length
    '*print-level* sci.impl.io/print-level
    '*print-meta* sci.impl.io/print-meta
    '*print-namespace-maps* sci.impl.io/print-namespace-maps
    '*print-readably* sci.impl.io/print-readably
    '*print-dup* sci.impl.io/print-dup-var
    #?@(:cljs ['*print-newline* sci.impl.io/print-newline])
    'newline (copy-var sci.impl.io/newline clojure-core-ns {:name 'newline})
    'flush (copy-core-var sci.impl.io/flush)
    'pr (copy-var sci.impl.io/pr clojure-core-ns {:name 'pr})
    'prn (copy-core-var sci.impl.io/prn)
    'print (copy-core-var sci.impl.io/print)
    'println (copy-core-var sci.impl.io/println)
    'pr-str (copy-core-var sci.impl.io/pr-str)
    'prn-str (copy-core-var sci.impl.io/prn-str)
    'print-str (copy-core-var #?(:cljs sci.impl.io/print-str :clj print-str))
    #?@(:clj ['print-method (copy-core-var print-method)])
    #?@(:clj ['print-dup (copy-core-var print-dup)])
    #?@(:clj ['printf (copy-core-var sci.impl.io/printf)])
    'with-out-str (macrofy 'with-out-str sci.impl.io/with-out-str)
    #?@(:clj ['with-in-str (macrofy 'with-in-str sci.impl.io/with-in-str)
              'read-line (copy-core-var sci.impl.io/read-line)])
    ;; end io
    ;; read
    '*data-readers* parser/data-readers
    '*default-data-reader-fn* parser/default-data-reader-fn
    '*read-eval* parser/read-eval
    '*reader-resolver* parser/reader-resolver
    'read (copy-var read clojure-core-ns {:ctx true :copy-meta-from 'clojure.core/read})
    'read-string (copy-var read-string clojure-core-ns {:copy-meta-from 'clojure.core/read-string :ctx true})
    #?@(:clj ['reader-conditional (copy-core-var reader-conditional)])
    #?@(:clj ['reader-conditional? (copy-core-var reader-conditional?)])
    ;; end read
    ;; REPL variables
    '*1 *1
    '*2 *2
    '*3 *3
    '*e *e
    ;; end REPL variables
    ;; clojure version
    #?@(:clj ['*clojure-version* clojure-version-var
              'clojure-version (copy-core-var clojure-version)])
    ;; end clojure version
    ;; multimethods
    'defmulti (macrofy 'defmulti sci.impl.multimethods/defmulti
                       clojure-core-ns true)
    'defmethod (macrofy 'defmethod sci.impl.multimethods/defmethod)
    'get-method (copy-core-var get-method)
    'methods (copy-core-var methods)
    'multi-fn-add-method-impl (copy-core-var sci.impl.multimethods/multi-fn-add-method-impl)
    'multi-fn?-impl (copy-core-var sci.impl.multimethods/multi-fn?-impl)
    'multi-fn-impl (copy-core-var sci.impl.multimethods/multi-fn-impl)
    'prefer-method (copy-core-var prefer-method)
    'prefers (copy-core-var prefers)
    'remove-method (copy-core-var remove-method)
    'remove-all-methods (copy-core-var remove-all-methods)
    ;; end multimethods
    ;; protocols
    'defprotocol (macrofy 'defprotocol sci.impl.protocols/defprotocol
                          clojure-core-ns true)
    'extend (copy-var sci.impl.protocols/extend clojure-core-ns {:name 'extend :ctx true})
    'extends? (copy-core-var sci.impl.protocols/extends?)
    'extend-type (macrofy 'extend-type sci.impl.protocols/extend-type
                          clojure-core-ns true)
    'extend-protocol (macrofy 'extend-protocol sci.impl.protocols/extend-protocol
                              clojure-core-ns true)
    '-reified-methods (new-var '-reified-methods #(types/getMethods %))
    'reify* (new-var 'reify* reify/reify* clojure-core-ns true)
    'reify (macrofy 'reify reify/reify clojure-core-ns true)
    'protocol-type-impl (new-var 'protocol-type-impl types/type-impl)
    #?@(:clj ['proxy* (new-var 'proxy* proxy/proxy* true)
              'proxy (macrofy 'proxy proxy/proxy clojure-core-ns true)])
    'satisfies? (copy-var sci.impl.protocols/satisfies? clojure-core-ns {:name 'satisfies?})
    ;; end protocols
    ;; IDeref as protocol
    'deref (copy-var core-protocols/deref* clojure-core-ns {:name 'deref})
    #?@(:cljs ['-deref (new-var '-deref core-protocols/-deref)
               'IDeref core-protocols/deref-protocol])
    ;; end IDeref as protocol
    ;; IAtom / ISwap as protocol
    'swap! (copy-var core-protocols/swap!* clojure-core-ns {:name 'swap!})
    'compare-and-set! #?(:clj (copy-var core-protocols/compare-and-set!* clojure-core-ns {:name 'compare-and-set!})
                         :cljs (copy-core-var compare-and-set!))
    #?@(:cljs ['IReset core-protocols/reset-protocol
               'ISwap core-protocols/swap-protocol
               '-swap! (new-var '-swap! core-protocols/-swap!)
               '-reset! (new-var '-reset! core-protocols/-reset!)])
    ;; in CLJS swap-vals! and reset-vals! are going through the other protocols
    #?@(:clj ['swap-vals! (copy-var core-protocols/swap-vals!* clojure-core-ns {:name 'swap-vals!})
              'reset-vals! (copy-var core-protocols/reset-vals!* clojure-core-ns {:name 'reset-vals!})])

    #?@(:cljs ['IRecord (utils/new-var 'IRecord {:protocol IRecord :ns clojure-core-ns}
                                       {:ns clojure-core-ns})])
    ;; cljs data structures
    #?@(:cljs ['Delay (copy-var Delay clojure-core-ns)])
    #?@(:cljs ['PersistentQueue (copy-var PersistentQueue clojure-core-ns)])
    #?@(:cljs ['LazySeq (copy-var LazySeq clojure-core-ns)])
    ;; end cljs data structures
    ;; private
    'has-root-impl (copy-core-var has-root-impl)
    ;; used in with-local-vars
    '-new-dynamic-var (new-var '-new-dynamic-var #(sci.impl.utils/new-var (gensym) nil {:dynamic true}))
    ;; used in let-fn
    '-new-var (new-var '-new-var #(sci.impl.utils/new-var (gensym) nil))
    '-add-loaded-lib (copy-var -add-loaded-lib clojure-core-ns {:ctx true})
    ;; end private
    '.. (macrofy '.. double-dot)
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
    '-> (macrofy '-> ->*)
    '->> (macrofy '->> ->>*)
    'as-> (macrofy 'as-> as->*)
    'comment (macrofy 'comment comment*)
    'add-watch (copy-core-var add-watch)
    'remove-watch (copy-core-var remove-watch)
    'aclone (copy-core-var aclone)
    'aget (copy-core-var aget)
    'alias (copy-var sci-alias clojure-core-ns {:name 'alias :ctx true})
    'all-ns (copy-var sci-all-ns clojure-core-ns {:name 'all-ns :ctx true})
    'alter-meta! (copy-core-var alter-meta!)
    'alter-var-root (copy-core-var sci.impl.vars/alter-var-root)
    'amap (macrofy 'amap amap*)
    'ancestors (copy-var hierarchies/ancestors* clojure-core-ns {:name 'ancestors :ctx true})
    'and (macrofy 'and and*)
    'aset (copy-core-var aset)
    #?@(:clj ['aset-boolean (copy-core-var aset-boolean)
              'aset-byte (copy-core-var aset-byte)
              'aset-char (copy-core-var aset-char)
              'aset-double (copy-core-var aset-double)
              'aset-float (copy-core-var aset-float)
              'aset-int (copy-core-var aset-int)
              'aset-long (copy-core-var aset-long)
              'aset-short (copy-core-var aset-short)])
    'alength #?(:clj (copy-var alength clojure-core-ns {:inlined
                                                        (fn [arr]
                                                          (java.lang.reflect.Array/getLength arr))})
                :cljs (copy-core-var alength))
    'any? (copy-core-var any?)
    'apply (copy-core-var apply)
    'areduce (macrofy 'areduce areduce*)
    #?@(:cljs ['array? (copy-core-var array?)])
    #?@(:cljs ['array (copy-core-var array)])
    'array-map (copy-core-var array-map)
    '*assert* assert-var
    'assert (macrofy 'assert assert*)
    'assoc (copy-core-var assoc)
    'assoc! (copy-core-var assoc!)
    'assoc-in (copy-core-var assoc-in)
    'associative? (copy-core-var associative?)
    'atom (copy-core-var atom)
    #?@(:clj ['bean (copy-core-var bean)])
    'binding (macrofy 'binding sci-binding)
    'binding-conveyor-fn (copy-core-var sci.impl.vars/binding-conveyor-fn)
    'bit-and-not (copy-core-var bit-and-not)
    #?@(:clj ['bit-clear (copy-core-var bit-clear)])
    'bit-set (copy-core-var bit-set)
    'bit-shift-left (copy-core-var bit-shift-left)
    'bit-shift-right (copy-core-var bit-shift-right)
    'bit-xor (copy-core-var bit-xor)
    'bound? (copy-var sci-bound? clojure-core-ns {:name 'bound?})
    'boolean (copy-core-var boolean)
    'boolean? (copy-core-var boolean?)
    'booleans (copy-core-var booleans)
    'butlast (copy-core-var butlast)
    'bytes (copy-core-var bytes)
    'bit-test (copy-core-var bit-test)
    'bit-and (copy-core-var bit-and)
    'bound-fn (macrofy 'bound-fn sci-bound-fn)
    'bound-fn* (copy-var bound-fn* clojure-core-ns)
    'bounded-count (copy-core-var bounded-count)
    'bit-or (copy-core-var bit-or)
    'bit-flip (copy-core-var bit-flip)
    'bit-not (copy-core-var bit-not)
    'byte (copy-core-var byte)
    'cat (copy-core-var cat)
    'case (macrofy 'case case**)
    'char (copy-core-var char)
    'char? (copy-core-var char?)
    #?@(:clj ['class? (copy-core-var class?)])
    #?@(:cljs ['clj->js (copy-core-var clj->js)])
    'cond (macrofy 'cond cond*)
    'cond-> (macrofy 'cond-> cond->*)
    'cond->> (macrofy 'cond->> cond->>*)
    'condp (macrofy 'condp condp*)
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
    'declare (macrofy 'declare declare*)
    'dedupe (copy-core-var dedupe)
    'defmacro (macrofy 'defmacro fns/defmacro*)
    'defn  (macrofy 'defn fns/defn*)
    'defn- (macrofy 'defn- defn-*)
    'defonce (macrofy 'defonce defonce*)
    'defrecord (macrofy 'defrecord sci.impl.records/defrecord
                        clojure-core-ns true)
    'deftype (macrofy 'deftype sci.impl.deftype/deftype
                      clojure-core-ns true)
    'delay (macrofy 'delay delay*)
    'delay? (copy-core-var delay?)
    #?@(:clj ['deliver (copy-core-var deliver)])
    #?@(:cljs ['demunge (copy-core-var cljs.core/demunge)])
    'derive (copy-var hierarchies/derive* clojure-core-ns {:name 'derive :ctx true})
    'descendants (copy-var hierarchies/descendants* clojure-core-ns {:name 'descendants :ctx true})
    'dissoc (copy-core-var dissoc)
    'dissoc! (copy-core-var dissoc!)
    'distinct (copy-core-var distinct)
    'distinct? (copy-core-var distinct?)
    'disj (copy-core-var disj)
    'disj! (copy-core-var disj!)
    'doall (copy-core-var doall)
    'dorun (copy-core-var dorun)
    'doseq   (macrofy 'doseq doseq-macro/expand-doseq)
    'dotimes (macrofy 'dotimes dotimes*)
    'doto (macrofy 'doto doto*)
    'double (copy-core-var double)
    'double-array (copy-core-var double-array)
    'double? (copy-core-var double?)
    'drop (copy-core-var drop)
    'drop-last (copy-core-var drop-last)
    'drop-while (copy-core-var drop-while)
    'doubles (copy-core-var doubles)
    'eduction (copy-core-var eduction)
    '->Eduction (copy-core-var ->Eduction)
    'empty (copy-core-var empty)
    'empty? (copy-core-var empty?)
    #?@(:clj ['enumeration-seq (copy-core-var enumeration-seq)])
    'eval (copy-var eval clojure-core-ns {:copy-meta-from 'clojure.core/eval
                                          :ctx true})
    'even? (copy-core-var even?)
    'every? (copy-core-var every?)
    'every-pred (copy-core-var every-pred)
    'ensure-reduced (copy-core-var ensure-reduced)
    'ex-data (copy-core-var ex-data)
    'ex-info (copy-core-var ex-info)
    'ex-message (copy-core-var ex-message)
    'ex-cause (copy-core-var ex-cause)
    'find-ns (copy-var sci-find-ns clojure-core-ns {:ctx true :name 'find-ns})
    'create-ns (copy-var sci-create-ns clojure-core-ns {:ctx true :name 'create-ns})
    'in-ns (copy-var sci-in-ns clojure-core-ns {:ctx true :name 'in-ns})
    'find-var (copy-var sci-find-var clojure-core-ns {:name 'find-var
                                                      :ctx true})
    'first (copy-core-var first)
    'float? (copy-core-var float?)
    'floats (copy-core-var floats)
    'fn (macrofy 'fn fns/fn**)
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
    'for (macrofy 'for for-macro/expand-for)
    'force (copy-core-var force)
    'get (copy-core-var get)
    'get-thread-binding-frame-impl (new-var 'get-thread-binding-frame-impl sci.impl.vars/get-thread-binding-frame)
    #?@(:clj ['get-thread-bindings (copy-var sci.impl.vars/get-thread-bindings clojure-core-ns {:name 'get-thread-bindings})])
    'get-in (copy-core-var get-in)
    'group-by (copy-core-var group-by)
    'gensym (copy-core-var gensym)
    'halt-when (copy-core-var halt-when)
    'hash (copy-core-var hash)
    'hash-combine (copy-core-var hash-combine)
    'hash-map (copy-core-var hash-map)
    'hash-set (copy-core-var hash-set)
    'hash-unordered-coll (copy-core-var hash-unordered-coll)
    'ident? (copy-core-var ident?)
    'identical? (copy-core-var identical?)
    'identity (copy-core-var identity)
    'if-let (macrofy 'if-let if-let*)
    'if-some (macrofy 'if-some if-some*)
    'if-not (macrofy 'if-not if-not*)
    'ifn? (copy-core-var ifn?)
    'inc (copy-core-var inc)
    'inst? (copy-core-var inst?)
    'inst-ms (copy-core-var inst-ms)
    'instance? (copy-var protocols/instance-impl clojure-core-ns {:name 'instance?})
    'int-array (copy-core-var int-array)
    'interleave (copy-core-var interleave)
    'intern (copy-var sci-intern clojure-core-ns {:name 'intern :ctx true})
    'into (copy-core-var into)
    'iterate (copy-core-var iterate)
    #?@(:clj ['iterator-seq (copy-core-var iterator-seq)])
    'int (copy-core-var int)
    'int? (copy-core-var int?)
    'interpose (copy-core-var interpose)
    'indexed? (copy-core-var indexed?)
    'integer? (copy-core-var integer?)
    'ints (copy-core-var ints)
    'into-array (copy-core-var into-array)
    'isa? (copy-var hierarchies/isa?* clojure-core-ns {:ctx true
                                                       :name 'isa?})
    #?@(:cljs ['js->clj (copy-core-var js->clj)])
    #?@(:cljs ['js-obj (copy-core-var js-obj)])
    #?@(:cljs ['js-keys (copy-core-var js-keys)])
    #?@(:cljs ['js-delete (copy-core-var js-delete)])
    'juxt (copy-core-var juxt)
    'keep (copy-core-var keep)
    'keep-indexed (copy-core-var keep-indexed)
    'key (copy-core-var key)
    'keys (copy-core-var keys)
    'keyword (copy-core-var keyword)
    'keyword? (copy-core-var keyword?)
    #?@(:cljs ['keyword-identical? (copy-core-var keyword-identical?)])
    'last (copy-core-var last)
    'lazy-seq (macrofy 'lazy-seq lazy-seq*)
    'lazy-cat (macrofy 'lazy-cat lazy-cat*)
    'let (macrofy 'let let**)
    'letfn (macrofy 'letfn letfn*)
    'load-string (copy-var load-string clojure-core-ns {:copy-meta-from 'clojure.core/load-string :ctx true})
    'loaded-libs (copy-var loaded-libs* clojure-core-ns {:name 'loaded-libs :ctx true})
    'loop (macrofy 'loop loop**)
    'long (copy-core-var long)
    'list (copy-core-var list)
    'list? (copy-core-var list?)
    'longs (copy-core-var longs)
    'list* (copy-core-var list*)
    'long-array (copy-core-var long-array)
    'macroexpand (copy-var macroexpand* clojure-core-ns {:name 'macroexpand
                                                         :ctx true})
    'macroexpand-1 (copy-var macroexpand-1* clojure-core-ns {:name 'macroexpand-1
                                                             :ctx true})
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
    'memfn (copy-var memfn clojure-core-ns {:macro true})
    'memoize (copy-core-var memoize)
    'merge (copy-core-var merge)
    'merge-with (copy-core-var merge-with)
    'min (copy-core-var min)
    'min-key (copy-core-var min-key)
    'munge (copy-core-var munge)
    'mod (copy-core-var mod)
    'name (copy-core-var name)
    'namespace (copy-core-var namespace)
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
    'ns (macrofy 'ns ns*)
    'ns-resolve (copy-var sci-ns-resolve clojure-core-ns {:ctx true :name 'ns-resolve})
    'number? (copy-core-var number?)
    'not-empty (copy-core-var not-empty)
    'not-any? (copy-core-var not-any?)
    'next (copy-core-var next)
    'nnext (copy-core-var nnext)
    'ns-aliases (copy-var sci-ns-aliases clojure-core-ns {:ctx true :name 'ns-aliases})
    'ns-imports (copy-var sci-ns-imports clojure-core-ns {:ctx true :name 'ns-imports})
    'ns-interns (copy-var sci-ns-interns clojure-core-ns {:ctx true :name 'ns-interns})
    'ns-publics (copy-var sci-ns-publics clojure-core-ns {:ctx true :name 'ns-publics})
    'ns-refers (copy-var sci-ns-refers clojure-core-ns {:ctx true :name 'ns-refers})
    'ns-map (copy-var sci-ns-map clojure-core-ns {:name 'ns-map :ctx true})
    'ns-unmap (copy-var sci-ns-unmap clojure-core-ns {:ctx true :name 'ns-unmap})
    'ns-unalias (copy-var sci-ns-unalias clojure-core-ns {:ctx true :name 'ns-unalias})
    'ns-name (copy-var sci-ns-name clojure-core-ns {:name 'ns-name})
    'odd? (copy-core-var odd?)
    #?@(:cljs ['object? (copy-core-var object?)])
    'object-array (copy-core-var object-array)
    'or (macrofy 'or or*)
    'parents (copy-var hierarchies/parents* clojure-core-ns {:name 'parents :ctx true})
    'peek (copy-core-var peek)
    'pop (copy-core-var pop)
    'pop! (copy-core-var pop!)
    'pop-thread-bindings (copy-core-var sci.impl.vars/pop-thread-bindings)
    'pos? (copy-core-var pos?)
    'pos-int? (copy-core-var pos-int?)
    'partial (copy-core-var partial)
    'partition (copy-core-var partition)
    'partition-all (copy-core-var partition-all)
    'partition-by (copy-core-var partition-by)
    'persistent! (copy-core-var persistent!)
    #?@(:clj ['promise (copy-core-var promise)])
    'push-thread-bindings (copy-var sci.impl.vars/push-thread-bindings clojure-core-ns {:name 'push-thread-bindings})
    'qualified-ident? (copy-core-var qualified-ident?)
    'qualified-symbol? (copy-core-var qualified-symbol?)
    'qualified-keyword? (copy-core-var qualified-keyword?)
    'quot (copy-core-var quot)
    #?@(:cljs ['random-uuid (copy-core-var random-uuid)])
    're-seq (copy-core-var re-seq)
    'refer (copy-var sci-refer clojure-core-ns {:name 'refer :ctx true})
    'refer-clojure (macrofy 'refer-clojure sci-refer-clojure)
    're-find (copy-core-var re-find)
    #?@(:clj ['re-groups (copy-core-var re-groups)])
    're-pattern (copy-core-var re-pattern)
    #?@(:clj ['re-matcher (copy-core-var re-matcher)])
    're-matches (copy-core-var re-matches)
    'realized? (copy-core-var realized?)
    'rem (copy-core-var rem)
    'remove (copy-core-var remove)
    'remove-ns (copy-var sci-remove-ns clojure-core-ns {:ctx true :name 'remove-ns})
    'require (copy-var require clojure-core-ns {:ctx true :copy-meta-from 'clojure.core/require})
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
    'reset! (copy-var core-protocols/reset!* clojure-core-ns {:name 'reset!})
    'reset-thread-binding-frame-impl (new-var 'reset-thread-binding-frame-impl sci.impl.vars/reset-thread-binding-frame)
    'resolve (copy-var sci-resolve clojure-core-ns {:name 'resolve :ctx true})
    'reversible? (copy-core-var reversible?)
    'rsubseq (copy-core-var rsubseq)
    'reductions (copy-core-var reductions)
    'rand (copy-core-var rand)
    'replace (copy-core-var replace)
    'rseq (copy-core-var rseq)
    'random-sample (copy-core-var random-sample)
    'repeat (copy-core-var repeat)
    'requiring-resolve (copy-var sci-requiring-resolve clojure-core-ns {:ctx true :name 'requiring-resolve})
    'run! (copy-core-var run!)
    'set? (copy-core-var set?)
    'sequential? (copy-core-var sequential?)
    'select-keys (copy-core-var select-keys)
    #?@(:clj ['short-array (copy-core-var short-array)])
    'simple-keyword? (copy-core-var simple-keyword?)
    'simple-symbol? (copy-core-var simple-symbol?)
    'some? (copy-core-var some?)
    'some-> (macrofy 'some-> some->*)
    'some->> (macrofy 'some->> some->>*)
    'string? (copy-core-var string?)
    'str (copy-core-var str)
    'second (copy-core-var second)
    'set (copy-core-var set)
    'seq (copy-core-var seq)
    #?@(:clj ['seq-to-map-for-destructuring (copy-var seq-to-map-for-destructuring clojure-core-ns)])
    'seq? (copy-core-var seq?)
    'short (copy-core-var short)
    'shuffle (copy-core-var shuffle)
    'sort (copy-core-var sort)
    'sort-by (copy-core-var sort-by)
    ;; #?@(:cljs ['-js-this -js-this
    ;;            'this-as (macrofy 'this-as this-as clojure-core-ns)])
    'test (copy-core-var test)
    'thread-bound? (copy-var sci-thread-bound? clojure-core-ns {:name 'thread-bound?})
    'subs (copy-core-var subs)
    #?@(:clj ['supers (copy-core-var supers)])
    'symbol (copy-var symbol* clojure-core-ns {:name 'symbol})
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
    'tagged-literal (copy-core-var tagged-literal)
    'tagged-literal? (copy-core-var tagged-literal?)
    'take (copy-core-var take)
    'take-last (copy-core-var take-last)
    'take-nth (copy-core-var take-nth)
    'take-while (copy-core-var take-while)
    'the-ns (copy-var sci-the-ns clojure-core-ns {:name 'the-ns :ctx true})
    'trampoline (copy-core-var trampoline)
    'transduce (copy-core-var transduce)
    'transient (copy-core-var transient)
    'tree-seq (copy-core-var tree-seq)
    'type (copy-var sci.impl.types/type-impl2 clojure-core-ns {:name 'type})
    'true? (copy-core-var true?)
    'to-array (copy-core-var to-array)
    'to-array-2d (copy-core-var to-array-2d)
    'update (copy-core-var update)
    'update-in (copy-core-var update-in)
    'uri? (copy-core-var uri?)
    'uuid? (copy-core-var uuid?)
    'unchecked-dec (copy-core-var unchecked-dec)
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
    #?@(:cljs ['undefined? (copy-core-var undefined?)])
    'underive (copy-var hierarchies/underive* clojure-core-ns {:name 'underive :ctx true})
    'unquote (doto (sci.impl.utils/new-var 'unquote nil {:ns clojure-core-ns})
               (sci.impl.vars/unbind))
    'use (copy-var use clojure-core-ns {:ctx true :copy-meta-from 'clojure.core/use})
    'val (copy-core-var val)
    'vals (copy-core-var vals)
    'var? (copy-var sci.impl.utils/var? clojure-core-ns {:name 'var?})
    'var-get (copy-var sci.impl.vars/var-get clojure-core-ns {:name 'var-get})
    'var-set (copy-var sci.impl.vars/var-set clojure-core-ns {:name 'var-set})
    'vary-meta (copy-core-var vary-meta)
    'vec (copy-core-var vec)
    'vector (copy-core-var vector)
    'vector? (copy-core-var vector?)
    'volatile! (copy-core-var volatile!)
    'vreset! (copy-core-var vreset!)
    'vswap! (macrofy 'vswap! vswap!)
    'when-first (macrofy 'when-first when-first*)
    'when-let (macrofy 'when-let when-let*)
    'when-some (macrofy 'when-some when-some*)
    'when (macrofy 'when when*)
    'when-not (macrofy 'when-not when-not*)
    'while (macrofy 'while while*)
    'with-bindings (macrofy 'with-bindings sci-with-bindings)
    'with-bindings* (copy-var with-bindings* clojure-core-ns)
    'with-local-vars (macrofy 'with-local-vars with-local-vars*)
    'with-meta (copy-core-var with-meta)
    'with-open (macrofy 'with-open with-open*)
    'with-redefs-fn (copy-var sci-with-redefs-fn clojure-core-ns {:name 'with-redefs-fn})
    'with-redefs (macrofy 'with-redefs sci-with-redefs)
    'zipmap (copy-core-var zipmap)
    'zero? (copy-core-var zero?)
    #?@(:clj ['+' (copy-core-var +')
              '-' (copy-core-var -')
              '*' (copy-core-var *')
              'boolean-array (copy-core-var boolean-array)
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
              'xml-seq (copy-core-var xml-seq)])
    #?@(:cljs ['-write (copy-var -write clojure-core-ns)])})

 (defn dir-fn
   [ctx ns]
   (let [current-ns (sci.impl.utils/current-ns-name)
         the-ns (sci-the-ns ctx
                            (get (sci-ns-aliases ctx current-ns) ns ns))]
     (sort (map first (sci-ns-publics ctx the-ns)))))

 (defn dir
   [_ _ nsname]
   `(doseq [v# (clojure.repl/dir-fn '~nsname)]
      (println v#)))

 (defn print-doc
   [m]
   (let [arglists (:arglists m)
         doc (:doc m)
         macro? (:macro m)]
     (sci.impl.io/println "-------------------------")
     (sci.impl.io/println (str (when-let [ns* (:ns m)]
                                 (str (sci-ns-name ns*) "/"))
                               (:name m)))
     (when arglists (sci.impl.io/println arglists))
     (when macro? (sci.impl.io/println "Macro"))
     (when doc (sci.impl.io/println " " doc))))

 (defn doc
   [_ _ sym]
   `(if-let [var# (resolve '~sym)]
      (when (var? var#)
        (~'clojure.repl/print-doc (meta var#)))
      (if-let [ns# (find-ns '~sym)]
        (~'clojure.repl/print-doc (assoc (meta ns#)
                                         :name (ns-name ns#))))))

 (defn find-doc
   "Prints documentation for any var whose documentation or name
   contains a match for re-string-or-pattern"
   [ctx re-string-or-pattern]
   (let [re (re-pattern re-string-or-pattern)
         ms (concat (mapcat #(sort-by :name (map meta (vals (sci-ns-interns ctx %))))
                            (sci-all-ns ctx))
                    (map #(assoc (meta %)
                                 :name (sci-ns-name %)) (sci-all-ns ctx))
                    #_(map special-doc (keys special-doc-map)))]
     (doseq [m ms
             :when (and (:doc m)
                        (or (re-find re (:doc m))
                            (re-find re (str (:name m)))))]
       (print-doc m))))

 (defn apropos
   "Given a regular expression or stringable thing, return a seq of all
   public definitions in all currently-loaded namespaces that match the
   str-or-pattern."
   [ctx str-or-pattern]
   (let [matches? (if (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) str-or-pattern)
                    #(re-find str-or-pattern (str %))
                    #(clojure.string/includes? (str %) (str str-or-pattern)))]
     (sort (mapcat (fn [ns]
                     (let [ns-name (str ns)]
                       (map #(symbol ns-name (str %))
                            (filter matches? (keys (sci-ns-publics ctx ns))))))
                   (sci-all-ns ctx)))))

 #_(defn source-fn
     "Returns a string of the source code for the given symbol, if it can
   find it.  This requires that the symbol resolve to a Var defined in
   a namespace for which the .clj is in the classpath.  Returns nil if
   it can't find the source.  For most REPL usage, 'source' is more
   convenient.

   Example: (source-fn 'filter)"
     [x]
     (when-let [v (resolve x)]
       (when-let [filepath (:file (meta v))]
         (when-let [strm (.getResourceAsStream (RT/baseLoader) filepath)]
           (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
             (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
             (let [text (StringBuilder.)
                   pbr (proxy [PushbackReader] [rdr]
                         (read [] (let [i (proxy-super read)]
                                    (.append text (char i))
                                    i)))
                   read-opts (if (.endsWith ^String filepath "cljc") {:read-cond :allow} {})]
               (if (= :unknown *read-eval*)
                 (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
                 (read read-opts (PushbackReader. pbr)))
               (str text)))))))

 (defn source-fn
   "Returns a string of the source code for the given symbol, if it can
   find it.  This requires that the symbol resolve to a Var defined in
   a namespace for which the .clj is in the classpath.  Returns nil if
   it can't find the source.  For most REPL usage, 'source' is more
   convenient.

   Example: (source-fn 'filter)"
   [ctx x]
   (when-let [v (sci-resolve ctx x)]
     (let [{:keys [#?(:clj :file) :line :ns]} (meta v)]
       (when (and line ns)
         (when-let [source (or #?(:clj (when file
                                         (let [f (jio/file file)]
                                           (when (.exists f) (slurp f)))))
                               (when-let [load-fn (:load-fn @(:env ctx))]
                                 (:source (load-fn {:namespace (sci-ns-name ns)}))))]
           (let [lines (clojure.string/split source #"\n")
                 line (dec line)
                 start (clojure.string/join "\n" (drop line lines))
                 reader (read/source-logging-reader start)
                 res (parser/parse-next ctx reader {:source true})]
             (:source (meta res))))))))

 (defn source
   "Prints the source code for the given symbol, if it can find it.
   This requires that the symbol resolve to a Var defined in a
   namespace for which the .clj is in the classpath.

   Example: (source filter)"
   [_ _ n]
   `(println (or (~'clojure.repl/source-fn '~n) (str "Source not found"))))

 #?(:clj
    (defn root-cause
      "Returns the initial cause of an exception or error by peeling off all of
   its wrappers"
      {:added "1.3"}
      [^Throwable t]
      (loop [cause t]
        (if (and (instance? clojure.lang.Compiler$CompilerException cause)
                 (not= (.source ^clojure.lang.Compiler$CompilerException cause) "NO_SOURCE_FILE"))
          cause
          (if-let [cause (.getCause cause)]
            (recur cause)
            cause)))))

 #?(:clj
    (defn demunge
      "Given a string representation of a fn class,
   as in a stack trace element, returns a readable version."
      {:added "1.3"}
      [fn-name]
      (clojure.lang.Compiler/demunge fn-name)))

 #?(:clj
    (defn stack-element-str
      "Returns a (possibly unmunged) string representation of a StackTraceElement"
      {:added "1.3"}
      [^StackTraceElement el]
      (let [file (.getFileName el)
            clojure-fn? (and file (or (.endsWith file ".clj")
                                      (.endsWith file ".cljc")
                                      (= file "NO_SOURCE_FILE")))]
        (str (if clojure-fn?
               (demunge (.getClassName el))
               (str (.getClassName el) "." (.getMethodName el)))
             " (" (.getFileName el) ":" (.getLineNumber el) ")"))))

 #?(:clj
    (defn pst
      "Prints a stack trace of the exception, to the depth requested. If none supplied, uses the root cause of the
   most recent repl exception (*e), and a depth of 12."
      {:added "1.3"}
      ([ctx] (pst ctx 12))
      ([ctx e-or-depth]
       (if (instance? Throwable e-or-depth)
         (pst ctx e-or-depth 12)
         (when-let [e (get-in @(:env ctx) [:namespaces 'clojure.core '*e])]
           (pst ctx (root-cause @e) e-or-depth))))
      ([ctx ^Throwable e depth]
       (sci.impl.vars/with-bindings {sci.impl.io/out @sci.impl.io/err}
         (sci.impl.io/println (str (-> e class .getSimpleName) " "
                                   (.getMessage e)
                                   (when-let [info (ex-data e)] (str " " (pr-str info)))))
         (let [st (.getStackTrace e)
               cause (.getCause e)]
           (doseq [el (take depth
                            (remove #(#{"clojure.lang.RestFn" "clojure.lang.AFn"}
                                      (.getClassName ^StackTraceElement %))
                                    st))]
             (sci.impl.io/println (str \tab (stack-element-str el))))
           (when cause
             (sci.impl.io/println "Caused by:")
             (pst ctx cause (min depth
                                 (+ 2 (- (count (.getStackTrace cause))
                                         (count st)))))))))))

 (def clojure-repl-namespace (sci.lang/->Namespace 'clojure.repl nil))

 (def clojure-repl
   {:obj clojure-repl-namespace
    'dir-fn (new-var 'dir-fn dir-fn clojure-repl-namespace true)
    'dir (macrofy 'dir dir clojure-repl-namespace)
    'print-doc (with-meta print-doc {:private true})
    'doc (macrofy 'doc doc clojure-repl-namespace)
    'find-doc (new-var 'find-doc find-doc clojure-repl-namespace true)
    'apropos (new-var 'apropos apropos clojure-repl-namespace true)
    'source (macrofy 'source source clojure-repl-namespace)
    'source-fn (new-var 'source-fn source-fn clojure-repl-namespace true)
    #?@(:clj ['pst (new-var 'pst pst clojure-repl-namespace true)
              'stack-element-str (new-var 'stack-element-str stack-element-str clojure-repl-namespace)
              'demunge (new-var 'demunge demunge clojure-repl-namespace)])})

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

 (def clojure-template-namespace (sci.lang/->Namespace 'clojure.template nil))

 (def clojure-template
   {:obj clojure-template-namespace
    'apply-template (copy-var apply-template clojure-template-namespace)
    'do-template (macrofy 'do-template do-template clojure-template-namespace)})

 (def clojure-string-namespace (sci.lang/->Namespace 'clojure.string nil))
 (def clojure-set-namespace (sci.lang/->Namespace 'clojure.set nil))
 (def clojure-walk-namespace (sci.lang/->Namespace 'clojure.walk nil))
 (def clojure-edn-namespace (sci.lang/->Namespace 'clojure.edn nil))

 (def macroexpand-all
   (sci.lang.Var. (fn [ctx form]
                    (clojure.walk/prewalk
                     (fn [x]
                       (if (seq? x)
                         (@sci.impl.utils/macroexpand* ctx x) x))
                     form))
                  'macroexpand-all
                  {:ns clojure-walk-namespace
                   :name 'macroexpand-all
                   :doc "Recursively performs all possible macroexpansions in form."}
                  false
                  true
                  nil))

 (def clojure-walk-ns
   {:obj clojure-walk-namespace
    'walk (copy-var clojure.walk/walk clojure-walk-namespace)
    'postwalk (copy-var clojure.walk/postwalk clojure-walk-namespace)
    'prewalk (copy-var clojure.walk/prewalk clojure-walk-namespace)
    #?@(:clj ['postwalk-demo (copy-var clojure.walk/postwalk-demo clojure-walk-namespace)
              'prewalk-demo (copy-var clojure.walk/prewalk-demo clojure-walk-namespace)])
    'keywordize-keys (copy-var clojure.walk/keywordize-keys clojure-walk-namespace)
    'stringify-keys (copy-var clojure.walk/stringify-keys clojure-walk-namespace)
    'prewalk-replace (copy-var clojure.walk/prewalk-replace clojure-walk-namespace)
    'postwalk-replace (copy-var clojure.walk/postwalk-replace clojure-walk-namespace)
    'macroexpand-all macroexpand-all})

 (def namespaces
   {#?@(:clj ['clojure.lang clojure-lang])
    'clojure.core clojure-core
    'clojure.string {:obj clojure-string-namespace
                     'blank? (copy-var clojure.string/blank? clojure-string-namespace)
                     'capitalize (copy-var clojure.string/capitalize clojure-string-namespace)
                     'ends-with? (copy-var clojure.string/ends-with? clojure-string-namespace)
                     'escape (copy-var clojure.string/escape clojure-string-namespace)
                     'includes? (copy-var clojure.string/includes? clojure-string-namespace)
                     'index-of (copy-var clojure.string/index-of clojure-string-namespace)
                     'join (copy-var clojure.string/join clojure-string-namespace)
                     'last-index-of (copy-var clojure.string/last-index-of clojure-string-namespace)
                     'lower-case (copy-var clojure.string/lower-case clojure-string-namespace)
                     'replace (copy-var clojure.string/replace clojure-string-namespace)
                     'replace-first (copy-var clojure.string/replace-first clojure-string-namespace)
                     'reverse (copy-var clojure.string/reverse clojure-string-namespace)
                     'split (copy-var clojure.string/split clojure-string-namespace)
                     'split-lines (copy-var clojure.string/split-lines clojure-string-namespace)
                     'starts-with? (copy-var clojure.string/starts-with? clojure-string-namespace)
                     'trim (copy-var clojure.string/trim clojure-string-namespace)
                     'trim-newline (copy-var clojure.string/trim-newline clojure-string-namespace)
                     'triml (copy-var clojure.string/triml clojure-string-namespace)
                     'trimr (copy-var clojure.string/trimr clojure-string-namespace)
                     'upper-case (copy-var clojure.string/upper-case clojure-string-namespace)
                     #?@(:clj ['re-quote-replacement (copy-var clojure.string/re-quote-replacement clojure-string-namespace)])}
    'clojure.set {:obj clojure-set-namespace
                  'difference (copy-var clojure.set/difference clojure-set-namespace)
                  'index (copy-var clojure.set/index clojure-set-namespace)
                  'intersection (copy-var clojure.set/intersection clojure-set-namespace)
                  'join (copy-var clojure.set/join clojure-set-namespace)
                  'map-invert (copy-var clojure.set/map-invert clojure-set-namespace)
                  'project (copy-var clojure.set/project clojure-set-namespace)
                  'rename (copy-var clojure.set/rename clojure-set-namespace)
                  'rename-keys (copy-var clojure.set/rename-keys clojure-set-namespace)
                  'select (copy-var clojure.set/select clojure-set-namespace)
                  'subset? (copy-var clojure.set/subset? clojure-set-namespace)
                  'superset? (copy-var clojure.set/superset? clojure-set-namespace)
                  'union (copy-var clojure.set/union clojure-set-namespace)}
    'clojure.walk clojure-walk-ns
    'clojure.template clojure-template
    'clojure.repl clojure-repl
    'clojure.edn {:obj clojure-edn-namespace
                  'read (copy-var #?(:clj  clojure.edn/read
                                     :cljs cljs.reader/read) clojure-edn-namespace)
                  'read-string (copy-var
                                #?(:clj  clojure.edn/read-string
                                   :cljs cljs.reader/read-string) clojure-edn-namespace)}
    'sci.impl.records sci-impl-records
    'sci.impl.deftype sci-impl-deftype
    'sci.impl.protocols sci-impl-protocols}))
