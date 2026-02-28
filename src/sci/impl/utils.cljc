(ns sci.impl.utils
  {:no-doc true}
  (:refer-clojure :exclude [eval demunge var? macroexpand macroexpand-1 munge-str])
  (:require [clojure.string :as str]
            #?(:cljs [goog.object :as gobject])
            [sci.impl.macros :as macros]
            [sci.impl.types :as t]
            [sci.impl.vars :as vars]
            [sci.lang :as lang])
  #?(:cljs (:import [goog.string StringBuffer]))
  #?(:cljs (:require-macros [sci.impl.utils :refer [kw-identical? dotimes+]])))

#?(:clj (set! *warn-on-reflection* true))

(derive :sci.error/parse :sci/error)

(defn constant? [x]
  (or (nil? x)
      (number? x)
      (string? x)
      (keyword? x)
      (boolean? x)
      #?(:clj
         (instance? java.util.regex.Pattern x)
         :cljs
         (instance? js/RegExp x))))

(defmacro kw-identical? [k v]
  (macros/?
   :clj `(identical? ~k ~v)
   :cljs `(cljs.core/keyword-identical? ~k ~v)))

;; NOTE: we could add a unique object to the context instead of using this
;; global one, which would be an even safer solution
(def recur #?(:clj (Object.)
              :cljs (js/Object.)))

(declare current-file current-ns)

(def ^:dynamic *top-level-location* nil)

(defn throw-error-with-location
  ([msg iobj] (throw-error-with-location msg iobj {}))
  ([msg iobj data]
   (let [{:keys [:line :column :file]} (meta iobj)
         file (or file @current-file)]
     (throw (ex-info msg (merge {:type :sci/error
                                 :line (or line (:line *top-level-location*))
                                 :column (or column (:column *top-level-location*))
                                 :file file} data))))))

(def ^:dynamic *in-try* false)

(defn macro? [f]
  (when-some [m (meta f)]
    (or (:sci/macro m)
        (:macro m))))

#?(:cljs
   (def allowed-append "used for allowing interop in with-out-str"
     (symbol "append")))

(defn demunge [s]
  #?(:clj (clojure.lang.Compiler/demunge s)
     :cljs (cljs.core/demunge s)))

#?(:clj
   (defn rewrite-ex-msg [ex-msg env fm]
     (when ex-msg
       (if-let [[_ printed-fn] (re-matches #"Wrong number of args \(\d+\) passed to: (.*)" ex-msg)]
         (let [fn-pat #"(sci\.impl\.)?fns/fun/arity-([0-9])+--\d+"
               [fn-match prefix arity] (re-find fn-pat ex-msg)
               friendly-name (when arity (str "function of arity " arity))]
           (if (:name fm)
             (let [ns (symbol (str (:ns fm)))
                   var-name (:name fm)
                   var (get-in @env [:namespaces ns var-name])
                   fstr (when var (let [varf (if (instance? clojure.lang.IDeref var)
                                               (deref var)
                                               var)
                                        varf (or
                                              ;; resolve macro inner fn for comparison
                                              (some-> varf meta :sci.impl/inner-fn)
                                              varf)
                                        fstr (clojure.lang.Compiler/demunge (str varf))
                                        fstr (first (str/split fstr #"@"))
                                        fstr (if prefix
                                               fstr
                                               (str/replace fstr #"^sci\.impl\." ""))]
                                    fstr))]
               (cond (and fstr printed-fn (= fstr printed-fn))
                     (str/replace ex-msg printed-fn
                                  (str (:ns fm) "/" (:name fm)))
                     friendly-name (str/replace ex-msg fn-match friendly-name)
                     :else ex-msg))
             ex-msg))
         ex-msg))))

(defn rethrow-with-location-of-node
  ([ctx ^Throwable e raw-node] (rethrow-with-location-of-node ctx (:bindings ctx) e raw-node))
  ([ctx _bindings ^Throwable e raw-node]
   (if (let [in-try #?(:clj (or *in-try*
                                (not= (:main-thread-id ctx)
                                      (.getId (Thread/currentThread))))
                       :cljs *in-try*)]
         (if (kw-identical? in-try :sci/error)
           ;; preserve location information
           false
           in-try))
     ;; we are inside a try/catch, do not preserve error location
     (throw e)
     (let [stack (t/stack raw-node)
           ;; _ (prn :stack stack)
           #?@(:clj [fm (:sci.impl/f-meta stack)])
           env (:env ctx)
           id (:id ctx)
           d (ex-data e)
           st (or (when-let [st (:sci.impl/callstack d)]
                    st)
                  (volatile! '()))]
       (when stack
         (vswap! st conj stack))
       (let [d (ex-data e)
               ;; st (:sci.impl/callstack d)
             wrapping-sci-error? (and (isa? (:type d) :sci/error)
                                      (:sci.impl/callstack d))]
         (if wrapping-sci-error?
           (throw e)
           (let [ex-msg #?(:clj (.getMessage e)
                           :cljs (.-message e))
                 {:keys [:line :column :file]}
                 (or stack
                     (some-> env deref
                             :sci.impl/callstack (get id)
                             deref last meta)
                     #_(meta node))]
             (if (and line column)
               (let [ex-msg #?(:clj (rewrite-ex-msg ex-msg env fm)
                               :cljs ex-msg)
                     phase (:phase d)
                     new-exception
                     (let [new-d (cond-> {:type :sci/error
                                          :line line
                                          :column column
                                          :message ex-msg
                                          :sci.impl/callstack st
                                          :file file}
                                   phase (assoc :phase phase))]
                       (ex-info ex-msg new-d e))]
                 (throw new-exception))
               (throw e)))))))))

(defn- iobj? [obj]
  (and #?(:clj (instance? clojure.lang.IObj obj)
          :cljs (implements? IWithMeta obj))
       (meta obj)))

(defn vary-meta*
  "Only adds metadata to obj if d is not nil and if obj already has meta"
  [obj f & args]
  (if (iobj? obj)
    (apply vary-meta obj f args)
    obj))

(defn strip-core-ns [sym]
  (case (namespace sym)
    ("clojure.core" "cljs.core") (symbol (name sym))
    sym))

(def allowed-loop (symbol "clojure.core/loop"))
(def allowed-recur (symbol "recur"))
(def var-unbound #?(:clj (Object.)
                    :cljs (js/Object.)))

(defn namespace-object
  "Fetches namespaces from env if it exists. Else, if `create?`,
  produces one regardless of the existince of the namespace in env and
  adds it to env before returning it."
  [env ns-sym create? attr-map]
  (let [env* @env
        ns-map (get-in env* [:namespaces ns-sym])]
    (or (:obj ns-map)
        (when (or ns-map create?)
          (let [ns-obj (lang/->Namespace ns-sym attr-map)]
            (swap! env assoc-in [:namespaces ns-sym :obj] ns-obj)
            ns-obj)))))

(defn set-namespace!
  [ctx ns-sym attr-map set-meta?]
  (let [env (:env ctx)
        attr-map (merge (meta ns-sym) attr-map)
        ns-obj (namespace-object env ns-sym true attr-map)]
    (when set-meta? (reset-meta! ns-obj attr-map))
    (t/setVal current-ns ns-obj)))

(def eval-form-state (volatile! nil))
(def eval-resolve-state (volatile! nil))
(def analyze (volatile! nil))

(defn eval [sci-ctx form]
  (@eval-form-state sci-ctx form))

(defn split-when
  "Like partition-by but splits collection only when `pred` returns
  a truthy value. E.g. `(split-when odd? [1 2 3 4 5]) => ((1 2) (3 4) (5))`"
  [pred coll]
  (let [f (complement pred)]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [fst (first s)
             run (cons fst (take-while f (next s)))]
         (cons run (split-when pred (lazy-seq (drop (count run) s)))))))))

(def ana-macros
  '#{do if and or fn fn* def defn
     lazy-seq try defmacro
     expand-dot* expand-constructor new . import in-ns ns var
     set! resolve})

(defn maybe-destructured
  [params body]
  (if (every? symbol? params)
    {:params params
     :body body}
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        {:params new-params
         :body [`(let ~lets
                   ~@body)]}))))

(def unqualify-symbol vars/unqualify-symbol)

(defn make-stack
  ([expr-meta] (make-stack expr-meta false))
  ([expr-meta special?]
   (cond-> (assoc expr-meta
                  :ns @current-ns
                  :file @current-file)
     special? (assoc :special true))))

(defn log [& xs]
  #?(:clj (.println System/err (str/join " " xs))
     :cljs (.log js/console (str/join " " xs))))

(defn dynamic-var
  ([name]
   (dynamic-var name nil (meta name)))
  ([name init-val]
   (dynamic-var name init-val (meta name)))
  ([name init-val meta]
   (let [meta (assoc meta :dynamic true :name (unqualify-symbol name))]
     (sci.lang.Var. init-val name meta false false nil (:ns meta)))))

;; foundational namespaces
(def user-ns (lang/->Namespace 'user nil))

(def clojure-core-ns (lang/->Namespace 'clojure.core nil))

(def current-file
  (dynamic-var '*file* nil
               {:doc "The path of the file being evaluated, as a String.\n\n  When there is no file, e.g. in the REPL, the value is not defined."
                :ns clojure-core-ns}))

(def current-ns
  (dynamic-var '*ns* user-ns
               {:ns clojure-core-ns
                :doc "A sci.lang.Namespace object representing the current namespace."}))

(defn current-ns-name []
  (let [curr-ns @current-ns]
    (if (symbol? curr-ns) curr-ns (t/getName curr-ns))))

(defn init-type!
  "Register a type name in the namespace at analysis time.
   Stores a placeholder Type in :refers so symbol resolution works
   without creating a var (matching Clojure where deftype/defrecord
   creates a class mapping, not a var)."
  [ctx name rec-type]
  (let [cnn (current-ns-name)
        env (:env ctx)
        t (new sci.lang.Type {:sci.impl/type-name rec-type})]
    (swap! env
           (fn [env]
             (update-in env [:namespaces cnn :types] assoc name t))))
  nil)

(defn new-var
  "Returns a new sci var."
  ([name] (doto (new-var name nil nil)
            (vars/unbind)))
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta]
   (let [meta (assoc meta :name (unqualify-symbol name))]
     (sci.lang.Var. init-val name meta false nil nil (:ns meta)))))

(defn var? [x]
  (instance? sci.lang.Var x))

(defn namespace? [x]
  (instance? #?(:clj sci.lang.Namespace
                :cljs sci.lang/Namespace) x))

(defmacro dotimes+ [n body]
  `(do (dotimes [i# ~(dec n)]
         ~body)
       ~body))

;; derived from (keys (. clojure.lang.Compiler specials))
;; (& monitor-exit case* try reify* finally loop* do letfn* if clojure.core/import* new deftype* let* fn* recur set! . var quote catch throw monitor-enter def)
(def special-syms '#{try finally do if new recur quote throw def . var set! let* loop* case* deftype*})

#?(:clj (def warn-on-reflection-var
          (dynamic-var
           '*warn-on-reflection* false
           {:ns clojure-core-ns
            :doc "When set to true, the compiler will emit warnings when reflection is\n  needed to resolve Java method calls or field accesses.\n\n  Defaults to false."})))

#?(:clj (def unchecked-math-var
          (dynamic-var
           '*unchecked-math* clojure.core/*unchecked-math*
           {:ns clojure-core-ns
            :doc "While bound to true, compilations of +, -, *, inc, dec and the\n  coercions will be done without overflow checks. While bound\n  to :warn-on-boxed, same behavior as true, and a warning is emitted\n  when compilation uses boxed math. Default: false."})))

#?(:cljs
   (defn ^string munge-str [name]
     (let [sb (StringBuffer.)]
       (loop [i 0]
         (when (< i (. name -length))
           (let [c (.charAt name i)
                 sub (gobject/get CHAR_MAP c)]
             (if-not (nil? sub)
               (.append sb sub)
               (.append sb c))
             (recur (inc i)))))
       (.toString sb))))
