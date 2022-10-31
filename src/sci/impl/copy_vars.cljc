(ns sci.impl.copy-vars
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
  '#{+' unchecked-remainder-int unchecked-subtract-int dec' short-array bit-shift-right aget = boolean bit-shift-left aclone dec < char unchecked-long unchecked-negate unchecked-inc-int floats pos? boolean-array alength bit-xor unsigned-bit-shift-right neg? unchecked-float num reduced? booleans int-array inc' <= -' * min get long double bit-and-not unchecked-add-int short quot unchecked-double longs unchecked-multiply-int int > unchecked-int unchecked-multiply unchecked-dec double-array float - byte-array zero? unchecked-dec-int rem nth nil? bit-and *' unchecked-add identical? unchecked-divide-int unchecked-subtract / bit-or >= long-array object-array doubles unchecked-byte unchecked-short float-array inc + aset chars ints bit-not byte max == count char-array compare shorts unchecked-negate-int unchecked-inc unchecked-char bytes})

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
    (symbol "clojure.core" (name (dequote sym))))

  (defn var-meta [&env sym opts & _a]
    (let [sym (dequote sym)
          macro (when opts (:macro opts))
          nm (when opts (:name opts))
          inline? (contains? inlined-vars sym)]
      (merge (cond-> {:name (or nm (list 'quote (symbol (name sym))))}
               macro (assoc :macro true)
               inline? (assoc :sci.impl/inlined sym))
             (let [#?@(:clj [the-var (macros/? :clj (resolve sym)
                                               :cljs (atom nil))])]
               (macros/? :clj #?(:clj  (let [m (meta the-var)
                                             dyn (:dynamic m)]
                                         (cond-> {:arglists (list 'quote (:arglists m))
                                                  :doc (:doc m)}
                                           dyn (assoc :dynamic dyn)))
                                 :cljs nil)
                         :cljs (let [r (cljs-resolve &env sym)
                                     m (:meta r)
                                     dyn (:dynamic m)]
                                 (cond-> {:arglists (ensure-quote (or (:arglists m) (:arglists r)))
                                          :doc (or (:doc m) (:doc r))}
                                   dyn (:dynamic m))))))))

  (defmacro macrofy [& args]
    (let [[sym & args] args]
      `(macrofy* ~sym ~@args ~@(repeat (- 3 (count args)) nil) ~(var-meta &env sym nil))))

  (defmacro if-vars-elided [then else]
    (if elide-vars
      then else))
  ;; Note: self hosted CLJS can't deal with multi-arity macros so this macro is split in 2
  (if-vars-elided
      (do
        #?(:clj
           (binding [*out* *err*]
             (println "SCI: eliding vars.")))
        (defmacro copy-var
          [sym _ns & [_opts]] sym)
        (defmacro copy-core-var [sym] sym))
    (do
      (defmacro copy-var
        [sym ns & [opts]]
        (let [macro (when opts (:macro opts))
              #?@(:clj [the-var (macros/? :clj (resolve sym)
                                          :cljs (atom nil))])
              varm (assoc (var-meta &env (:name opts sym) opts)
                          :sci/built-in true
                          :ns ns)
              nm (:name varm)
              ctx (when opts (:ctx opts))]
          ;; NOTE: emit as little code as possible, so our JS bundle is as small as possible
          (if macro
            (macros/? :clj
                      #?(:clj  `(sci.lang.Var. ~(deref the-var) ~nm ~varm false ~ctx)
                         :cljs `(sci.lang.Var. ~sym ~nm ~varm false ~ctx))
                      :cljs `(sci.lang.Var. ~sym ~nm ~varm false ~ctx))
            `(sci.lang.Var. ~sym ~nm ~varm false ~ctx))))
      (defmacro copy-core-var
        [sym]
        `(copy-var ~sym clojure-core-ns {:copy-meta-from ~(core-sym sym)})))))

(defn macrofy*
  ([f] (vary-meta f #(assoc % :sci/macro true)))
  ([sym f] (macrofy* sym f nil false))
  ([sym f ns] (macrofy* sym f ns false))
  ([sym f ns ctx?] (macrofy* sym f ns ctx? nil))
  ([sym f ns ctx? extra-meta]
   (let [ns (or ns clojure-core-ns)]
     (utils/new-var sym f (cond-> {:ns ns
                                            :macro true
                                            :sci/built-in true}
                                     (and (not elide-vars)
                                          extra-meta)
                                     (merge extra-meta))
                             ctx?))))

(defn new-var
  ([sym f] (new-var sym f nil false))
  ([sym f ns] (new-var sym f ns false))
  ([sym f ns ctx?] (new-var sym f ns ctx? nil))
  ([sym f ns ctx? extra-meta]
   (let [ctx? (or ctx? (true? ns))
         ns (if (true? ns)
              clojure-core-ns
              (or ns clojure-core-ns))]
     (assert (and (not (boolean? ns))
                  (instance? sci.lang.Namespace ns)) sym)
     (utils/new-var sym f (cond-> {:ns ns
                                            :sci/built-in true}
                                     (and (not elide-vars)
                                          extra-meta)
                                     (merge extra-meta))
                             ctx?))))
