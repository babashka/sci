(ns sci.impl.core-protocols
  {:no-doc true}
  (:refer-clojure :exclude [inst-ms])
  (:require
   #?@(:cljs [] :default [[sci.ctx-store :as store]])
   [sci.impl.deftype]
   #?(:cljd [sci.impl.multimethods :as mm])
   #?(:cljs [sci.impl.copy-vars :as copy-vars])
   [sci.impl.records]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.lang :as lang])
  #?@(:cljd [] :clj [(:import [sci.impl.records SciRecord]
                              [sci.impl.deftype SciType])]))

#?(:cljd
   (def cljd-core-ns (lang/->Namespace 'cljd.core nil)))
#?(:clj
   (def clj-lang-ns (lang/->Namespace 'clojure.lang nil)))
#?(:cljs
   (def cljs-core-ns (lang/->Namespace 'cljs.core nil)))

;; The protocol method multifns and the protocol vars referencing them are
;; created per context by install-protocol-vars (called from
;; sci.impl.opts/init-env!): a load-time singleton would share guest
;; extensions (host classes, :default) between unrelated contexts.
;; On CLJS sci types implement these protocols natively (per-type protocol
;; slots, see ADR 0012), so there is nothing to install there.

(declare fallback-multis)

#?(:cljs nil :default
   (defn lookup
     "The per-context multifn (or the Inst protocol var) created by
     new-protocol-multis, stored under :protocol-multis on the env. Falls
     back to a pristine instance when no context is bound (host call
     outside of evaluation)."
     [sym]
     (or (when-let [ctx store/*ctx*]
           (get (:protocol-multis (clojure.core/deref (:env ctx))) sym))
         (get (clojure.core/deref fallback-multis) sym))))

;;;; IDeref

;; on cljd built-in multifns are SciMultiFns so records and reify can add
;; methods at runtime, host defmultis have no runtime add on Dart
#?(:cljd
   (defn new-deref-multi []
     (mm/->SciMultiFn '-deref types/type-impl :default
                      (atom {:sci.impl.protocols/reified
                             (fn [ref]
                               ((get (types/getMethods ref) '-deref) ref))
                             :default
                             (fn [ref] (clojure.core/deref ref))})))
   :clj
   (do (defn- deref-reified [ref]
         (let [methods (types/getMethods ref)]
           ((get methods 'deref) ref)))

       (defn new-deref-multi []
         (doto (clojure.lang.MultiFn. "deref" types/type-impl :default
                                      #'clojure.core/global-hierarchy)
           (.addMethod :sci.impl.protocols/reified deref-reified)
           (.addMethod :default clojure.core/deref))))
   ;; on CLJS sci types implement IDeref natively (per-type/per-instance
   ;; protocol slots), so plain cljs.core/deref dispatches into sci impls
   :cljs nil)

;; on CLJS sci types implement the protocols natively, so clojure.core's
;; deref/swap!/reset! are exposed directly and no re-routing wrappers exist
#?(:cljd
   (defn deref* [x]
     (if (satisfies? IDeref x)
       (clojure.core/deref x)
       ((lookup '-deref) x)))
   :clj
   (defn deref*
     ([x]
      (if (instance? clojure.lang.IDeref x)
        (clojure.core/deref x)
        ((lookup 'deref) x)))
     ([x & args]
      (apply clojure.core/deref x args))))

#?(:cljd
   (defn new-deref-protocol [deref-multi]
     (utils/new-var
      'cljd.core.IDeref
      {:protocol IDeref
       :methods #{deref-multi}
       :ns cljd-core-ns}
      {:ns cljd-core-ns}))
   :clj
   (defn new-deref-protocol [deref-multi]
     (utils/new-var
      'clojure.lang.IDeref
      {:class clojure.lang.IDeref
       :methods #{deref-multi}
       :ns clj-lang-ns}
      {:ns clj-lang-ns}))
   :cljs
   (def deref-protocol
     (utils/new-var
      'cljs.core.IDeref
      (copy-vars/protocol-entry IDeref cljs-core-ns)
      {:ns cljs-core-ns})))

;;;; end IDeref

;;;; IAtom

;; ;; You can use multiarity in multimethods
;; (defmulti foo (fn [x & _] x))

;; (defmethod foo :default [_ & _] "DEFAULT VALUE DISPACHED")

;; ;; Like a standar multi-arity function
;; (defmethod foo :bar
;;   ([_ _] "ONE ARGUMENT")
;;   ([_ _ _] "TWO ARGUMENTs")
;;   ([_ _ _ _] "THREE ARGUMENTs")
;;   ([_ _ _ _ & more] (cl-format nil "~d ARGUMENTS" (+ 3 (count more)))))

#?(:cljd
   (defn new-swap-multi []
     (mm/->SciMultiFn '-swap! types/type-impl :default
                      (atom {:sci.impl.protocols/reified
                             (fn [ref f & args]
                               (apply (get (types/getMethods ref) '-swap!) ref f args))
                             :default
                             (fn [ref f & args]
                               (apply clojure.core/swap! ref f args))}))))
#?(:cljd
   (defn new-reset-multi []
     (mm/->SciMultiFn '-reset! types/type-impl :default
                      (atom {:sci.impl.protocols/reified
                             (fn [ref v]
                               ((get (types/getMethods ref) '-reset!) ref v))
                             :default
                             (fn [ref v] (reset! ref v))}))))

;;;; Protocol methods

#?(:clj
   (defn- swap-reified
     ([ref f]
      (let [methods (types/getMethods ref)]
        ((get methods 'swap) ref f)))
     ([ref f a1]
      (let [methods (types/getMethods ref)]
        ((get methods 'swap) ref f a1)))
     ([ref f a1 a2]
      (let [methods (types/getMethods ref)]
        ((get methods 'swap) ref f a1 a2)))
     ([ref f a1 a2 & args]
      (let [methods (types/getMethods ref)]
        (apply (get methods 'swap) ref f a1 a2 args)))))

#?(:clj
   (defn- reset-reified [ref v]
     (let [methods (types/getMethods ref)]
       ((get methods 'reset) ref v))))

#?(:clj
   (defn- compareAndSet-reified [ref old new]
     (let [methods (types/getMethods ref)]
       ((get methods 'compareAndSet) ref old new))))

#?(:clj
   (defn- swapVals-reified
     ([ref f]
      (let [methods (types/getMethods ref)]
        ((get methods 'swapVals) ref f)))
     ([ref f a1]
      (let [methods (types/getMethods ref)]
        ((get methods 'swapVals) ref f a1)))
     ([ref f a1 a2]
      (let [methods (types/getMethods ref)]
        ((get methods 'swapVals) ref f a1 a2)))
     ([ref f a1 a2 & args]
      (let [methods (types/getMethods ref)]
        (apply (get methods 'swapVals) ref f a1 a2 args)))))

#?(:clj
   (defn- resetVals-reified [ref v]
     (let [methods (types/getMethods ref)]
       ((get methods 'resetVals) ref v))))

#?(:clj
   (defn- swap-default [ref f & args]
     ;; TODO: optimize arities
     (apply clojure.core/swap! ref f args)))

#?(:clj
   (do
     (defn new-swap-multi []
       (doto (clojure.lang.MultiFn. "swap" types/type-impl :default
                                    #'clojure.core/global-hierarchy)
         (.addMethod :sci.impl.protocols/reified swap-reified)
         (.addMethod :default swap-default)))
     (defn new-reset-multi []
       (doto (clojure.lang.MultiFn. "reset" types/type-impl :default
                                    #'clojure.core/global-hierarchy)
         (.addMethod :sci.impl.protocols/reified reset-reified)
         (.addMethod :default clojure.core/reset!)))
     (defn new-compareAndSet-multi []
       (doto (clojure.lang.MultiFn. "compareAndSet" types/type-impl :default
                                    #'clojure.core/global-hierarchy)
         (.addMethod :sci.impl.protocols/reified compareAndSet-reified)
         (.addMethod :default clojure.core/compare-and-set!)))
     (defn new-swapVals-multi []
       (doto (clojure.lang.MultiFn. "swapVals" types/type-impl :default
                                    #'clojure.core/global-hierarchy)
         (.addMethod :sci.impl.protocols/reified swapVals-reified)
         (.addMethod :default clojure.core/swap-vals!)))
     (defn new-resetVals-multi []
       (doto (clojure.lang.MultiFn. "resetVals" types/type-impl :default
                                    #'clojure.core/global-hierarchy)
         (.addMethod :sci.impl.protocols/reified resetVals-reified)
         (.addMethod :default clojure.core/reset-vals!)))))

;;;; Re-routing

#?(:cljd nil :cljs nil :clj
   (defn swap!* [ref f & args]
     (if
         ;; fast-path for host IAtom
         (instance? clojure.lang.IAtom ref)
       (if args
         (apply clojure.core/swap! ref f args)
         (clojure.core/swap! ref f))
       (let [swap (lookup 'swap)]
         (if args
           (apply swap ref f args)
           (swap ref f))))))

#?(:cljd
   (defn swap!* [ref f & args]
     (if (or (instance? cljd.core/Atom ref)
             (satisfies? ISwap ref))
       (if args
         (apply clojure.core/swap! ref f args)
         (clojure.core/swap! ref f))
       (let [-swap! (lookup '-swap!)]
         (if args
           (apply -swap! ref f args)
           (-swap! ref f))))))

#?(:cljd
   (defn reset!* [ref v]
     (if (or (instance? cljd.core/Atom ref)
             (satisfies? IReset ref))
       (clojure.core/reset! ref v)
       ((lookup '-reset!) ref v)))
   :clj
   (defn reset!* [ref v]
     (if (instance? clojure.lang.IAtom ref)
       (clojure.core/reset! ref v)
       ((lookup 'reset) ref v))))

#?(:clj
   (defn compare-and-set!* [ref old new]
     (if (instance? clojure.lang.IAtom ref)
       ;; fast-path for host IAtoms
       (clojure.core/compare-and-set! ref old new)
       ((lookup 'compareAndSet) ref old new))))

#?(:clj
   (defn swap-vals!* [ref f & args]
     (if (instance? clojure.lang.IAtom ref)
       (apply clojure.core/swap-vals! ref f args)
       (apply (lookup 'swapVals) ref f args))))

#?(:clj
   (defn reset-vals!* [ref v]
     (if (instance? clojure.lang.IAtom ref)
       (clojure.core/reset-vals! ref v)
       ((lookup 'resetVals) ref v))))

;;;; Protocol vars

#?(:cljd
   (defn new-swap-protocol [swap-multi]
     (utils/new-var
      'cljd.core.ISwap
      {:protocol ISwap
       :methods #{swap-multi}
       :ns cljd-core-ns}
      {:ns cljd-core-ns}))
   :clj
   (defn new-swap-protocol [swap-multi reset-multi cas-multi]
     (utils/new-var
      'clojure.lang.IAtom
      {:class clojure.lang.IAtom
       :methods #{swap-multi reset-multi cas-multi}
       :ns clj-lang-ns}
      {:ns clj-lang-ns}))
   :cljs
   (def swap-protocol
     (utils/new-var
      'cljs.core.ISwap
      (copy-vars/protocol-entry ISwap cljs-core-ns)
      {:ns cljs-core-ns})))

#?(:cljd
   (defn new-reset-protocol [reset-multi]
     (utils/new-var
      'cljd.core.IReset
      {:protocol IReset
       :methods #{reset-multi}
       :ns cljd-core-ns}
      {:ns cljd-core-ns})))

#?(:cljs
   (def reset-protocol
     (utils/new-var
      'cljs.core.IReset
      (copy-vars/protocol-entry IReset cljs-core-ns)
      {:ns cljs-core-ns})))

#?(:clj
   (defn new-iatom2-protocol [swap-multi reset-multi cas-multi swapVals-multi resetVals-multi]
     (utils/new-var
      'clojure.lang.IAtom2
      {:class clojure.lang.IAtom2
       :methods #{swap-multi reset-multi cas-multi swapVals-multi resetVals-multi}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})))

;;;; end IAtom

;;;; IPrintWithWriter (CLJS only)

#?(:cljs
   (def print-writer-protocol
     (utils/new-var
      'cljs.core.IPrintWithWriter
      (copy-vars/protocol-entry IPrintWithWriter cljs-core-ns)
      {:ns cljs-core-ns})))

;;;; end IPrintWithWriter

;;;; IFn

#?(:cljd
   (def ifn-protocol
     (utils/new-var
      'cljd.core.IFn
      {:protocol IFn
       :methods #{types/sci-invoke}
       :ns cljd-core-ns}
      {:ns cljd-core-ns})))

#?(:cljs
   (def ifn-protocol
     (utils/new-var
      'cljs.core.IFn
      {:protocol IFn
       :methods #{types/sci-invoke}
       :ns cljs-core-ns}
      {:ns cljs-core-ns})))

#?(:clj
   (def ifn-protocol
     (utils/new-var
      'IFn
      {:protocol clojure.lang.IFn
       :methods #{types/sci-invoke types/sci-apply-to}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})))

(defn sci-ifn? [x]
  (cond
    (fn? x) true
    #?(:cljd (satisfies? types/SciTypeInstance x)
       :clj (instance? sci.impl.types.SciTypeInstance x)
       :cljs (cljs.core/implements? types/SciTypeInstance x))
    ;; cljd has no get-method, query the SciMultiFn method-table directly
    #?(:cljd (boolean (mm/get-method-impl types/sci-invoke (types/type-impl x)))
       :default (boolean (get-method types/sci-invoke (types/type-impl x))))
    #?@(:cljd [(instance? types/Reified x)
               (boolean (get (types/getMethods x) '-invoke))]
        :clj [(instance? clojure.lang.IFn x) true]
        :cljs [(instance? types/Reified x)
               (boolean (get (types/getMethods x) '-invoke))])
    :else #?(:cljd (ifn? x)
             :clj false
             :cljs (ifn? x))))

;;;; end IFn

;;;; Inst

;; JVM only: on CLJS cljs.core/Inst is exposed as a native protocol entry, see
;; the protocol-vars call in sci.impl.namespaces.

#?(:clj
   (do
     (defn- inst-ms-reified [x]
       (let [methods (types/getMethods x)]
         ((get methods 'inst-ms*) x)))

     (defn new-inst-ms-multi []
       (doto (clojure.lang.MultiFn. "inst-ms*" types/type-impl :default
                                    #'clojure.core/global-hierarchy)
         (.addMethod :sci.impl.protocols/reified inst-ms-reified)
         ;; host types (java.util.Date, java.time.Instant) and anything that
         ;; extended clojure.core/Inst outside of sci
         (.addMethod :default clojure.core/inst-ms)))

     (defn inst-ms [x]
       ((lookup 'inst-ms*) x))

     (defn new-inst-protocol [inst-ms-multi]
       (utils/new-var
        'Inst
        {:protocol clojure.core/Inst
         :methods #{inst-ms-multi}
         :ns utils/clojure-core-ns}
        {:ns utils/clojure-core-ns}))))

;;;; end Inst

;;;; Per-context installation

#?(:cljd
   (defn new-protocol-multis []
     {'-deref (new-deref-multi)
      '-swap! (new-swap-multi)
      '-reset! (new-reset-multi)})
   :clj
   (defn new-protocol-multis []
     (let [inst-ms-multi (new-inst-ms-multi)]
       {'deref (new-deref-multi)
        'swap (new-swap-multi)
        'reset (new-reset-multi)
        'compareAndSet (new-compareAndSet-multi)
        'swapVals (new-swapVals-multi)
        'resetVals (new-resetVals-multi)
        'inst-ms* inst-ms-multi
        'Inst (new-inst-protocol inst-ms-multi)})))

#?(:cljd
   (defn install-protocol-vars [namespaces multis]
     (let [deref-multi (get multis '-deref)
           swap-multi (get multis '-swap!)
           reset-multi (get multis '-reset!)]
       (update namespaces 'clojure.core assoc
               'IDeref (new-deref-protocol deref-multi)
               '-deref (utils/new-var '-deref deref-multi)
               'ISwap (new-swap-protocol swap-multi)
               '-swap! (utils/new-var '-swap! swap-multi)
               'IReset (new-reset-protocol reset-multi)
               '-reset! (utils/new-var '-reset! reset-multi))))
   :clj
   (defn install-protocol-vars [namespaces multis]
     (let [deref-multi (get multis 'deref)
           swap-multi (get multis 'swap)
           reset-multi (get multis 'reset)
           cas-multi (get multis 'compareAndSet)
           swapVals-multi (get multis 'swapVals)
           resetVals-multi (get multis 'resetVals)
           inst-ms-multi (get multis 'inst-ms*)]
       (-> namespaces
           (update 'clojure.lang assoc
                   'IDeref (new-deref-protocol deref-multi)
                   'deref (utils/new-var 'deref deref-multi {:ns clj-lang-ns})
                   'IAtom (new-swap-protocol swap-multi reset-multi cas-multi)
                   'swap (utils/new-var 'swap swap-multi {:ns clj-lang-ns})
                   'reset (utils/new-var 'reset reset-multi {:ns clj-lang-ns})
                   'compareAndSet (utils/new-var 'compareAndSet cas-multi {:ns clj-lang-ns})
                   'IAtom2 (new-iatom2-protocol swap-multi reset-multi cas-multi
                                                swapVals-multi resetVals-multi)
                   'swapVals (utils/new-var 'swapVals swapVals-multi {:ns clj-lang-ns})
                   'resetVals (utils/new-var 'resetVals resetVals-multi {:ns clj-lang-ns}))
           (update 'clojure.core assoc
                   'Inst (get multis 'Inst)
                   'inst-ms* (utils/new-var 'inst-ms* inst-ms-multi
                                            {:ns utils/clojure-core-ns
                                             :arglists '([inst])})))))
   :cljs nil)

#?(:cljs nil :default
   (def ^:private fallback-multis
     (delay (new-protocol-multis))))
