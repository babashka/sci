(ns sci.impl.copy-vars
  {:no-doc true
   :doc "See doc/impl/copy-var.md for architecture overview."}
  (:require
   [clojure.string :as str]
   [sci.impl.cljs]
   [sci.impl.macros :as macros]
   [sci.impl.utils :as utils :refer [clojure-core-ns]]
   #?(:cljd [sci.lang :as lang] :default [sci.lang]))
  #?(:cljs (:require-macros [sci.impl.copy-vars :refer [copy-var copy-core-var macrofy]])))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

;; The following is produced with:
;; (def inlined (filter (comp :inline meta) (vals (ns-publics 'clojure.core))))
;; (map (comp :name meta) inlined)
(def ^:macro-support inlined-vars
  '#{+' unchecked-remainder-int unchecked-subtract-int dec' short-array bit-shift-right aget = boolean bit-shift-left aclone dec < char unchecked-long unchecked-negate unchecked-inc-int floats pos? boolean-array alength bit-xor unsigned-bit-shift-right neg? unchecked-float num reduced? booleans int-array inc' <= -' * min get long double bit-and-not unchecked-add-int short quot unchecked-double longs unchecked-multiply-int int > unchecked-int unchecked-multiply unchecked-dec double-array float - byte-array zero? unchecked-dec-int rem nth nil? bit-and *' unchecked-add identical? unchecked-divide-int unchecked-subtract / bit-or >= long-array object-array doubles unchecked-byte unchecked-short float-array inc + chars ints bit-not byte max == count char-array compare shorts unchecked-negate-int unchecked-inc unchecked-char bytes})

(def ^:macro-support cljs-resolve #?(:cljd nil :default (resolve 'cljs.analyzer.api/resolve)))

#?(:cljd (def ^:macro-support elide-vars false)
   :clj (def elide-vars (= "true" (System/getenv "SCI_ELIDE_VARS")))
   ;; for self-hosted
   :cljs (def elide-vars false))

(macros/deftime

  (defn ^:macro-support ensure-quote [x]
    (if (and (seq? x) (= 'quote (first x)))
      x
      (list 'quote x)))

  (defn ^:macro-support dequote [x]
    (if (and (seq? x) (= 'quote (first x)))
      (second x)
      x))

  (defn ^:macro-support core-sym [sym]
    (symbol "clojure.core" (name sym)))

  ;; :cljd/clj-host is host-only, var metadata lives in the cljd compiler registry
  #?(:cljd/clj-host
     (defn ^:macro-support cljd-var-meta [sym]
       (let [nses @@(resolve 'cljd.compiler/nses)
             cur @(resolve 'cljd.compiler/*current-ns*)
             look (fn [ns-sym nm]
                    (let [ns-sym (if (contains? nses ns-sym)
                                   ns-sym
                                   (symbol (clojure.string/replace (str ns-sym) #"^clojure\." "cljd.")))]
                      (when-some [e (get-in nses [ns-sym nm])]
                        [ns-sym (:meta e)])))
             sym-ns (some-> (namespace sym) symbol)
             nm (symbol (name sym))]
         (if sym-ns
           (or (look (if (= 'clojure.core sym-ns) 'cljd.core sym-ns) nm) [sym-ns nil])
           (or (look cur nm)
               (look 'cljd.core nm)
               ;; unresolved bare symbol counts as core, like on CLJS
               [nil nil])))))

  (defn ^:macro-support var-meta [&env sym opts]
    (let [sym (dequote sym)
          macro (:macro opts)
          nm (:name opts)
          [fqsym sym m]
          (if (:ns &env)
            ;; CLJS compilation: resolve via cljs analyzer
            (let [r (cljs-resolve &env sym)]
              [(or (:name r) sym) (symbol (name sym)) (merge r (:meta r))])
            ;; CLJ compilation: resolve upfront
            ;; v may be nil for syms passed via macrofy that don't resolve
            #?(:cljd/clj-host (let [[the-ns m] (cljd-var-meta sym)]
                                [(if the-ns
                                   (symbol (str the-ns) (name sym))
                                   (symbol (name sym)))
                                 (symbol (name sym))
                                 m])
               :cljd nil ;; var-meta itself is only called at macro time
               :clj (let [v (resolve sym)]
                      [(if v (symbol v) sym)
                       (symbol (name sym))
                       (meta v)])
               ;; self-hosted CLJS: dead branch, &env always has :ns
               :cljs nil))
          ;; Only exclude symbols that resolve to a non-core namespace (e.g. a
          ;; third-party fn named like a core inlined var). Core macros set up
          ;; via macrofy (ns, lazy-seq, ...) resolve to a bare unqualified symbol
          ;; in CLJS, so a nil namespace counts as core.
          fqsym-ns (namespace fqsym)
          core-ns? (or (nil? fqsym-ns)
                       (= "clojure.core" fqsym-ns)
                       (= "cljd.core" fqsym-ns)
                       (= "cljs.core" fqsym-ns))
          inline (and core-ns? (contains? inlined-vars sym))
          fast-path (and core-ns? (or (= 'or sym)
                                      (= 'and sym)
                                      (= 'ns sym)
                                      (= 'lazy-seq sym)))
          dyn (:dynamic m)
          private (:private m)
          arglists (:arglists m)
          ;; host tags are JVM classes, they don't compile on cljd
          tag #?(:cljd nil :default (:tag m))
          file (:file m)
          line (:line m)
          column (:column m)
          varm (cond-> {:name (or nm (list 'quote sym))}
                 macro (assoc :macro true)
                 inline (assoc :sci.impl/inlined (:inlined opts fqsym))
                 (not elide-vars) (assoc :doc (:doc m))
                 dyn (assoc :dynamic dyn)
                 private (assoc :private private)
                 (and (not macro) (:sci.impl/public opts)
                      (or (:macro m) (:sci/macro m)))
                 (assoc :macro true)
                 (when-not elide-vars arglists)
                 (assoc :arglists (ensure-quote arglists))
                 tag (assoc :tag (list 'quote tag))
                 fast-path (assoc :sci.impl/fast-path (list 'quote sym))
                 (when-not elide-vars file) (assoc :file file)
                 (when-not elide-vars line) (assoc :line line)
                 (when-not elide-vars column) (assoc :column column))]
      varm)))

;; separate deftime form, cljd host eval does not split top-level do
(macros/deftime
  (defmacro macrofy [& args]
    (let [[sym & args] args]
      `(macrofy* ~sym ~@args ~@(repeat (- 2 (count args)) nil) ~(var-meta &env sym nil))))


  ;; Note: self hosted CLJS can't deal with multi-arity macros so this macro is split in 2
  #?(:clj
     (if elide-vars
       (binding [*out* *err*]
         (println "SCI: eliding vars."))
       nil))
  (defmacro copy-var
    [sym ns & [opts]]
    (let [public (:sci.impl/public opts)
          dyn (:dynamic opts)
          meta-sym (or (when-not public (:name opts))
                       (:copy-meta-from opts)
                       sym)
          base-meta (var-meta &env meta-sym opts)
          macro (:macro base-meta)
          varm (cond-> (assoc base-meta :ns ns)
                 (not public) (assoc :sci/built-in true)
                 dyn (assoc :dynamic dyn))
          nm (:name varm)
          ctx (:ctx opts)
          init (:init opts sym)
          ;; Macros: can't take value of a macro symbol directly
          ;; Private vars: can't reference across namespaces with bare symbol
          init (if (and (or macro (:private base-meta))
                        (not (contains? opts :init)))
                 #?(:cljd init
                    :clj `(deref (var ~sym))
                    :cljs init)
                 init)]
      ;; NOTE: emit as little code as possible, so our JS bundle is as small as possible
      (if (and (not macro) elide-vars (not dyn) (not ctx))
        sym
        `(sci.lang/->Var ~init ~nm ~varm false ~ctx nil ~ns))))
  (defmacro copy-core-var
    [sym]
    `(copy-var ~sym clojure-core-ns {:copy-meta-from ~(core-sym sym)}))

  (defmacro avoid-method-too-large [v]
    #?(:cljd v
       :default
       (macros/? :clj
                 `(deref (delay ~v))
                 :cljs v))))

(defn macrofy*
  ([f] (vary-meta f #(assoc % :sci/macro true)))
  ([sym f] (macrofy* sym f nil false))
  ([sym f ns] (macrofy* sym f ns nil))
  ([sym f ns extra-meta]
   (let [ns (or ns clojure-core-ns)]
     (utils/new-var sym f (cond-> {:ns ns
                                   :macro true
                                   :sci/built-in true}
                            (and (not elide-vars)
                                 extra-meta)
                            (merge extra-meta))))))

(defn new-var
  ([sym f] (new-var sym f nil nil))
  ([sym f ns] (new-var sym f ns nil))
  ([sym f ns extra-meta]
   (let [ns (if (true? ns)
              clojure-core-ns
              (or ns clojure-core-ns))]
     (assert (and (not (boolean? ns))
                  (instance? #?(:cljd lang/Namespace
                                :clj sci.lang.Namespace
                                :cljs sci.lang.Namespace) ns)) sym)
     (utils/new-var sym f (cond-> {:ns ns
                                   :sci/built-in true}
                            (and (not elide-vars)
                                 extra-meta)
                            (merge extra-meta))))))

;;;; Native CLJS protocol entries (see doc/ai/adr/0011-native-cljs-protocols.md)

#?(:cljd nil
   :default
   (macros/deftime
    (defn- ^:macro-support protocol-prefix*
      "Munged property prefix for a fully qualified protocol symbol. Mirrors
  cljs.core/protocol-prefix: cljs.core/ILookup => cljs$core$ILookup$"
      [fq-sym]
      (str (-> (str fq-sym)
               (str/replace "." "$")
               (str/replace "/" "$"))
           "$"))

    (defn- ^:macro-support protocol-prop* [s]
      (with-meta (symbol (str "-" s)) {:protocol-prop true}))

    (defn ^:macro-support cljs-protocol-info
      "Analyzer var info when `sym` names a CLJS protocol var and we are
  compiling ClojureScript, nil otherwise. cljs.core/IFn is excluded: it is
  implemented on SciType at the class level (sci-invoke) and its
  arity/variadic call convention does not fit per-arity slot setters."
      [env sym]
      (when (:ns env)
        (let [info #_:clj-kondo/ignore
              (#?(:clj sci.impl.cljs/cljs-resolve
                  :cljs cljs.analyzer.api/resolve) env sym)]
          (when (and (:name info)
                     (:protocol-symbol info)
                     (not= 'cljs.core/IFn (:name info)))
            info))))

    (defn ^:macro-support protocol-entry-form
      "Expansion for copying a CLJS protocol: a map entry with the protocol
  object, a compiled satisfies? fn and per-arity property setters that
  install method impls on a JS prototype (see
  sci.impl.deftype/-install-native-protocol!). The setters are emitted as
  property access forms so Closure renames them consistently with cljs.core
  under :advanced."
      [psym info ns]
      (let [fq (:name info)
            minfo (get-in info [:protocol-info :methods])
            prefix (protocol-prefix* fq)
            methods-form
            (into {}
                  (map (fn [[fname sigs]]
                         [(list 'quote (symbol (name fname)))
                          {:setters
                           (into {}
                                 (map (fn [sig]
                                        (let [n (count sig)
                                              slot (protocol-prop* (str prefix (munge (name fname)) "$arity$" n))]
                                          [n `(fn [o# f#]
                                                (~'set! (. o# ~slot) f#)
                                                nil)])))
                                 sigs)}]))
                  minfo)]
        `{:protocol ~psym
          :name '~fq
          :ns ~ns
          :methods #{}
          :satisfies-fn (fn [x#] (satisfies? ~psym x#))
          :marker-setter (fn [o#]
                           (~'set! (. o# ~(protocol-prop* prefix)) cljs.core/PROTOCOL_SENTINEL)
                           nil)
          :native-methods ~methods-form}))

    (defn ^:macro-support copy-ns-protocol-var?
      "True when a copy-ns public (analyzer var map + meta) names a CLJS
  protocol that should be copied as a protocol entry. cljs.core/IFn is
  excluded, see cljs-protocol-info."
      [var m]
      (and (or (:protocol-symbol var) (:protocol-symbol m))
           (or (:protocol-info var) (:protocol-info m))
           (not= 'cljs.core/IFn (:name var))))

    (defmacro protocol-entry
      "Native protocol entry map for CLJS protocol `psym`. `ns` evaluates to a
  sci namespace object. Compiles to nil outside ClojureScript."
      [psym ns]
      (when-let [info (cljs-protocol-info &env psym)]
        (protocol-entry-form psym info ns)))))
