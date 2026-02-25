(ns sci.impl.copy-vars
  {:no-doc true
   :doc "See doc/impl/copy-var.md for architecture overview."}
  (:require
   [sci.impl.cljs]
   [sci.impl.macros :as macros]
   [sci.impl.utils :as utils :refer [clojure-core-ns]]
   [sci.lang])
  #?(:cljs (:require-macros [sci.impl.copy-vars :refer [copy-var copy-core-var macrofy]])))

#?(:clj (set! *warn-on-reflection* true))

;; The following is produced with:
;; (def inlined (filter (comp :inline meta) (vals (ns-publics 'clojure.core))))
;; (map (comp :name meta) inlined)
(def inlined-vars
  '#{+' unchecked-remainder-int unchecked-subtract-int dec' short-array bit-shift-right aget = boolean bit-shift-left aclone dec < char unchecked-long unchecked-negate unchecked-inc-int floats pos? boolean-array alength bit-xor unsigned-bit-shift-right neg? unchecked-float num reduced? booleans int-array inc' <= -' * min get long double bit-and-not unchecked-add-int short quot unchecked-double longs unchecked-multiply-int int > unchecked-int unchecked-multiply unchecked-dec double-array float - byte-array zero? unchecked-dec-int rem nth nil? bit-and *' unchecked-add identical? unchecked-divide-int unchecked-subtract / bit-or >= long-array object-array doubles unchecked-byte unchecked-short float-array inc + chars ints bit-not byte max == count char-array compare shorts unchecked-negate-int unchecked-inc unchecked-char bytes})

(def cljs-resolve (resolve 'cljs.analyzer.api/resolve))

#?(:clj (def elide-vars (= "true" (System/getenv "SCI_ELIDE_VARS")))
   ;; for self-hosted
   :cljs (def elide-vars false))

(macros/deftime

  (defn ensure-quote [x]
    (if (and (seq? x) (= 'quote (first x)))
      x
      (list 'quote x)))

  (defn dequote [x]
    (if (and (seq? x) (= 'quote (first x)))
      (second x)
      x))

  (defn core-sym [sym]
    (symbol "clojure.core" (name sym)))

  (defn var-meta [&env sym opts]
    (let [sym (dequote sym)
          macro (when opts (:macro opts))
          nm (when opts (:name opts))
          [fqsym sym #?(:clj resolved-var)]
          (if (:ns &env)
            ;; CLJS compilation: no CLJ resolve
            (if (qualified-symbol? sym)
              [sym (symbol (name sym))]
              (if (:sci.impl/public opts)
                [(symbol (name (:name (:ns &env))) (str sym)) sym]
                [(symbol "clojure.core" (str sym)) sym]))
            ;; CLJ compilation: resolve upfront
            #?(:clj (if (qualified-symbol? sym)
                      [sym (symbol (name sym)) (resolve sym)]
                      (if-let [v (resolve sym)]
                        [(symbol v) sym v]
                        [(symbol "clojure.core" (str sym)) sym]))
               ;; self-hosted CLJS: dead branch, &env always has :ns
               :cljs nil))
          inline (contains? inlined-vars sym)
          fast-path (or (= 'or sym)
                        (= 'and sym)
                        (= 'ns sym)
                        (= 'lazy-seq sym))
          varm (merge (cond-> {:name (or nm (list 'quote (symbol (name sym))))}
                        macro (assoc :macro true)
                        inline (assoc :sci.impl/inlined (:inlined opts fqsym)))
                      (macros/? :clj #?(:clj  (let [m (meta resolved-var)
                                                    dyn (:dynamic m)
                                                    private (:private m)
                                                    arglists (:arglists m)
                                                    tag (:tag m)
                                                    file (:file m)
                                                    line (:line m)
                                                    column (:column m)]
                                                (let [macro? (and (not macro) (:sci.impl/public opts)
                                                                  (or (:macro m) (:sci/macro m)))]
                                                  (cond-> (if elide-vars {} {:doc (:doc m)})
                                                    dyn (assoc :dynamic dyn)
                                                    private (assoc :private private)
                                                    macro? (assoc :macro true)
                                                    (if elide-vars false arglists)
                                                    (assoc :arglists (list 'quote (:arglists m)))
                                                    tag (assoc :tag tag)
                                                    fast-path (assoc :sci.impl/fast-path (list 'quote sym))
                                                    (if elide-vars false file) (assoc :file file)
                                                    (if elide-vars false line) (assoc :line line)
                                                    (if elide-vars false column) (assoc :column column))))
                                        :cljs nil)
                                :cljs (let [r (cljs-resolve &env fqsym)
                                            m (:meta r)
                                            dyn (:dynamic m)
                                            private (:private m)
                                            macro? (and (not macro) (:sci.impl/public opts)
                                                        (or (:macro r) (:macro m) (:sci/macro m)))
                                            arglists (or (:arglists m) (:arglists r))]
                                        (cond-> {:arglists (ensure-quote arglists)
                                                 :doc (or (:doc m) (:doc r))}
                                          dyn (assoc :dynamic dyn)
                                          private (assoc :private private)
                                          macro? (assoc :macro true)
                                          arglists (assoc :arglists (ensure-quote arglists))
                                          fast-path (assoc :sci.impl/fast-path (list 'quote sym))))))]
      #_(when (= 'inc sym)
          (prn varm))
      varm))

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
          macro (or (:macro opts) (:macro base-meta))
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
                 #?(:clj `(deref (var ~sym))
                    :cljs init)
                 init)]
      ;; NOTE: emit as little code as possible, so our JS bundle is as small as possible
      (if macro
        `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil ~ns)
        (if elide-vars
          (if (or dyn ctx)
            `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil ~ns)
            sym)
          `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil ~ns)))))
  (defmacro copy-core-var
    [sym]
    `(copy-var ~sym clojure-core-ns {:copy-meta-from ~(core-sym sym)}))

  (defmacro avoid-method-too-large [v]
    (macros/? :clj
              `(deref (delay ~v))
              :cljs v)))

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
                  (instance? sci.lang.Namespace ns)) sym)
     (utils/new-var sym f (cond-> {:ns ns
                                   :sci/built-in true}
                            (and (not elide-vars)
                                 extra-meta)
                            (merge extra-meta))))))
