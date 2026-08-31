(ns sci.impl.core-protocols
  {:no-doc true}
  (:refer-clojure :exclude [deref -deref -swap! -reset! inst-ms inst-ms*])
  (:require
   [sci.impl.deftype]
   #?(:cljd [sci.impl.multimethods :as mm])
   #?(:cljs [sci.impl.copy-vars :as copy-vars])
   [sci.impl.records]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.impl.world :as world]
   [sci.lang :as lang])
  #?@(:cljd [] :clj [(:import [sci.impl.records SciRecord]
                              [sci.impl.deftype SciType])]))

;;;; IDeref

(def ^:private untracked-value
  #?(:cljd (Object.) :clj (Object.) :cljs (js/Object.)))

(defn- tracked-value [x]
  (world/value x untracked-value))

;; on cljd built-in multifns are SciMultiFns so records and reify can add
;; methods at runtime, host defmultis have no runtime add on Dart
#?(:cljd
   (def -deref
     (mm/->SciMultiFn '-deref types/type-impl :default
                      (atom {:sci.impl.protocols/reified
                             (fn [ref]
                               ((get (types/getMethods ref) '-deref) ref))
                             :default
                             (fn [ref] (clojure.core/deref ref))})))
   :clj
   (do (defmulti deref types/type-impl)

       (defmethod deref :sci.impl.protocols/reified [ref]
         (let [methods (types/getMethods ref)]
           ((get methods 'deref) ref)))

       (defmethod deref :default [ref]
         (clojure.core/deref ref)))
   ;; on CLJS sci types implement IDeref natively (per-type/per-instance
   ;; protocol slots), so plain cljs.core/deref dispatches into sci impls
   :cljs nil)

;; On CLJS, SCI types implement protocols natively; the wrapper only adds
;; world-relative routing before falling back to cljs.core/deref.
#?(:cljd
   (defn deref* [x]
     (if (and (world/primary-world?) (satisfies? IDeref x))
       (clojure.core/deref x)
       (let [v (tracked-value x)]
         (if-not (identical? untracked-value v)
           v
           (if (satisfies? IDeref x)
             (clojure.core/deref x)
             (-deref x))))))
   :clj
   (defn deref*
     ([x]
      (if (and (world/primary-world?)
               (instance? clojure.lang.IDeref x))
         (clojure.core/deref x)
         (let [v (tracked-value x)]
           (if-not (identical? untracked-value v)
             v
             (if (instance? clojure.lang.IDeref x)
               (clojure.core/deref x)
               (deref x))))))
     ([x & args]
      (apply clojure.core/deref x args)))
   :cljs
   (defn deref* [x]
     (if (world/primary-world?)
       (clojure.core/deref x)
       (let [v (tracked-value x)]
         (if-not (identical? untracked-value v)
           v
           (clojure.core/deref x))))))

(defn atom*
  "Creates an atom whose state is owned by the active SCI world. The host atom
  is retained as a stable handle and as the carrier for metadata, validators,
  and watches."
  [x & options]
  (let [a (apply clojure.core/atom x options)]
    (world/register! a x)
    a))

(defn volatile!*
  "Creates a volatile whose value is owned by the active SCI world."
  [x]
  (let [v (clojure.core/volatile! x)]
    (world/register! v x)
    v))

(defn vreset!*
  [v x]
  (if (world/mutable-primary-world?)
    (clojure.core/vreset! v x)
    (if (world/tracked? v)
      (if (world/primary-world?)
        (let [new (clojure.core/vreset! v x)]
          (world/reset-value! v new)
          new)
        (world/reset-value! v x))
      (clojure.core/vreset! v x))))

(defn vswap!*
  [v f & args]
  (if (world/mutable-primary-world?)
    (clojure.core/vreset! v (apply f (clojure.core/deref v) args))
    (if (world/tracked? v)
      (vreset!* v (apply f (tracked-value v) args))
      (clojure.core/vreset! v (apply f (clojure.core/deref v) args)))))

(defn- validate-ref! [ref v]
  (when-let [validator (get-validator ref)]
    (when-not (validator v)
      (throw #?(:cljd (StateError. "Invalid reference state")
                :clj (IllegalStateException. "Invalid reference state")
                :cljs (js/Error. "Invalid reference state")))))
  nil)

(defn- notify-ref! [ref old new]
  #?(:clj (.notifyWatches ^clojure.lang.ARef ref old new)
     :default nil)
  nil)

(defn- swap-tracked! [ref f args]
  (if (world/primary-world?)
    (let [new (if args
                (apply clojure.core/swap! ref f args)
                (clojure.core/swap! ref f))]
      (world/reset-value! ref new)
      new)
    (world/swap-value! ref f args
                       #(validate-ref! ref %)
                       #(notify-ref! ref %1 %2))))

(defn- reset-tracked! [ref v]
  (if (world/primary-world?)
    (let [new (clojure.core/reset! ref v)]
      (world/reset-value! ref new)
      new)
    (world/reset-tracked! ref v
                          #(validate-ref! ref %)
                          #(notify-ref! ref %1 %2))))

(defn- compare-and-set-tracked!* [ref old new]
  (if (world/primary-world?)
    (let [changed? (clojure.core/compare-and-set! ref old new)]
      (when changed?
        (world/reset-value! ref new))
      changed?)
    (world/compare-and-set-tracked! ref old new
                                    #(validate-ref! ref %)
                                    #(notify-ref! ref %1 %2))))

#?(:cljd
   (def cljd-core-ns (lang/->Namespace 'cljd.core nil)))
#?(:clj
   (def clj-lang-ns (lang/->Namespace 'clojure.lang nil)))
#?(:cljs
   (def cljs-core-ns (lang/->Namespace 'cljs.core nil)))

(def deref-protocol
  #?(:cljd
     (utils/new-var
      'cljd.core.IDeref
      {:protocol IDeref
       :methods #{-deref}
       :ns cljd-core-ns}
      {:ns cljd-core-ns})
     :clj
     (utils/new-var
      'clojure.lang.IDeref
      {:class clojure.lang.IDeref
       :methods #{deref}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})
     :cljs
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
   (def -swap!
     (mm/->SciMultiFn '-swap! types/type-impl :default
                      (atom {:sci.impl.protocols/reified
                             (fn [ref f & args]
                               (apply (get (types/getMethods ref) '-swap!) ref f args))
                             :default
                             (fn [ref f & args]
                               (apply clojure.core/swap! ref f args))})))
   :clj (defmulti swap types/type-impl))
#?(:cljd
   (def -reset!
     (mm/->SciMultiFn '-reset! types/type-impl :default
                      (atom {:sci.impl.protocols/reified
                             (fn [ref v]
                               ((get (types/getMethods ref) '-reset!) ref v))
                             :default
                             (fn [ref v] (reset! ref v))})))
   :clj (defmulti reset types/type-impl))
#?(:clj (defmulti compareAndSet types/type-impl))
#?(:clj (defmulti swapVals types/type-impl))
#?(:clj (defmulti resetVals types/type-impl))

;;;; Protocol methods

#?(:clj
   (defmethod swap :sci.impl.protocols/reified
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
   (defmethod reset :sci.impl.protocols/reified [ref v]
     (let [methods (types/getMethods ref)]
       ((get methods 'reset) ref v))))

#?(:clj
   (defmethod compareAndSet :sci.impl.protocols/reified [ref old new]
     (let [methods (types/getMethods ref)]
       ((get methods 'compareAndSet) ref old new))))

#?(:clj
   (defmethod swapVals :sci.impl.protocols/reified
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
   (defmethod resetVals :sci.impl.protocols/reified [ref v]
     (let [methods (types/getMethods ref)]
       ((get methods 'resetVals) ref v))))

;;;; Defaults

#?(:clj
   (defmethod swap :default [ref f & args]
     ;; TODO: optimize arities
     (apply clojure.core/swap! ref f args)))

#?(:clj
   (defmethod reset :default [ref v]
     (reset! ref v)))

#?(:clj
   (defmethod compareAndSet :default [ref old new]
     (compare-and-set! ref old new)))

#?(:clj
   (defmethod swapVals :default [ref & args]
     (apply swap-vals! ref args)))

#?(:clj
   (defmethod resetVals :default [ref v]
     (reset-vals! ref v)))

;;;; Re-routing

#?(:clj
   (defn swap!* [ref f & args]
     (if (and (world/mutable-primary-world?)
              (instance? clojure.lang.IAtom ref))
       (apply clojure.core/swap! ref f args)
       (if (world/tracked? ref)
         (swap-tracked! ref f args)
         (if (instance? clojure.lang.IAtom ref)
           (if args
             (apply clojure.core/swap! ref f args)
             (clojure.core/swap! ref f))
           (if args
             (apply swap ref f args)
             (swap ref f))))))
   :cljs
   (defn swap!* [ref f & args]
     (if (world/mutable-primary-world?)
       (apply clojure.core/swap! ref f args)
       (if (world/tracked? ref)
         (swap-tracked! ref f args)
         (if args
           (apply clojure.core/swap! ref f args)
           (clojure.core/swap! ref f))))))

#?(:cljd
   (defn swap!* [ref f & args]
     (if (and (world/mutable-primary-world?)
              (instance? cljd.core/Atom ref))
       (apply clojure.core/swap! ref f args)
       (if (world/tracked? ref)
         (swap-tracked! ref f args)
         (if (or (instance? cljd.core/Atom ref)
                 (satisfies? ISwap ref))
           (if args
             (apply clojure.core/swap! ref f args)
             (clojure.core/swap! ref f))
           (if args
             (apply -swap! ref f args)
             (-swap! ref f)))))))

#?(:cljd
   (defn reset!* [ref v]
     (if (and (world/mutable-primary-world?)
              (instance? cljd.core/Atom ref))
       (clojure.core/reset! ref v)
       (if (world/tracked? ref)
         (reset-tracked! ref v)
         (if (or (instance? cljd.core/Atom ref)
                 (satisfies? IReset ref))
           (clojure.core/reset! ref v)
           (-reset! ref v)))))
   :clj
   (defn reset!* [ref v]
     (if (and (world/mutable-primary-world?)
              (instance? clojure.lang.IAtom ref))
       (clojure.core/reset! ref v)
       (if (world/tracked? ref)
         (reset-tracked! ref v)
         (if (instance? clojure.lang.IAtom ref)
           (clojure.core/reset! ref v)
           (reset ref v)))))
   :cljs
   (defn reset!* [ref v]
     (if (world/mutable-primary-world?)
       (clojure.core/reset! ref v)
       (if (world/tracked? ref)
         (reset-tracked! ref v)
         (clojure.core/reset! ref v)))))

#?(:cljd
   (defn compare-and-set!* [ref old new]
     (if (and (world/mutable-primary-world?)
              (instance? cljd.core/Atom ref))
       (clojure.core/compare-and-set! ref old new)
       (if (world/tracked? ref)
         (compare-and-set-tracked!* ref old new)
         (clojure.core/compare-and-set! ref old new))))
   :clj
   (defn compare-and-set!* [ref old new]
     (if (and (world/mutable-primary-world?)
              (instance? clojure.lang.IAtom ref))
       (clojure.core/compare-and-set! ref old new)
       (if (world/tracked? ref)
         (compare-and-set-tracked!* ref old new)
         (if (instance? clojure.lang.IAtom ref)
           ;; fast-path for host IAtoms
           (clojure.core/compare-and-set! ref old new)
           (compareAndSet ref old new)))))
   :cljs
   (defn compare-and-set!* [ref old new]
     (if (world/mutable-primary-world?)
       (clojure.core/compare-and-set! ref old new)
       (if (world/tracked? ref)
         (compare-and-set-tracked!* ref old new)
         (clojure.core/compare-and-set! ref old new)))))

#?(:clj
   (defn swap-vals!* [ref f & args]
     (if (and (world/mutable-primary-world?)
              (instance? clojure.lang.IAtom ref))
       (apply clojure.core/swap-vals! ref f args)
       (if (world/tracked? ref)
       (if (world/primary-world?)
         (let [ret (apply clojure.core/swap-vals! ref f args)]
           (world/reset-value! ref (nth ret 1))
           ret)
         (let [old (world/value ref nil)
               new (swap-tracked! ref f args)]
           [old new]))
       (if (instance? clojure.lang.IAtom ref)
         (apply clojure.core/swap-vals! ref f args)
         (apply swapVals ref f args))))))

#?(:clj
   (defn reset-vals!* [ref v]
     (if (and (world/mutable-primary-world?)
              (instance? clojure.lang.IAtom ref))
       (clojure.core/reset-vals! ref v)
       (if (world/tracked? ref)
       (if (world/primary-world?)
         (let [ret (clojure.core/reset-vals! ref v)]
           (world/reset-value! ref (nth ret 1))
           ret)
         (let [old (world/value ref nil)
               new (reset-tracked! ref v)]
           [old new]))
       (if (instance? clojure.lang.IAtom ref)
         (clojure.core/reset-vals! ref v)
         (resetVals ref v))))))

;;;; Protocol vars

(def swap-protocol
  #?(:cljd
     (utils/new-var
      'cljd.core.ISwap
      {:protocol ISwap
       :methods #{-swap!}
       :ns cljd-core-ns}
      {:ns cljd-core-ns})
     :clj
     (utils/new-var
      'clojure.lang.IAtom
      {:class clojure.lang.IAtom
       :methods #{swap, reset, compareAndSet}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})
     :cljs
     (utils/new-var
      'cljs.core.ISwap
      (copy-vars/protocol-entry ISwap cljs-core-ns)
      {:ns cljs-core-ns})))

#?(:cljd
   (def reset-protocol
     (utils/new-var
      'cljd.core.IReset
      {:protocol IReset
       :methods #{-reset!}
       :ns cljd-core-ns}
      {:ns cljd-core-ns})))

#?(:cljs
   (def reset-protocol
     (utils/new-var
      'cljs.core.IReset
      (copy-vars/protocol-entry IReset cljs-core-ns)
      {:ns cljs-core-ns})))

#?(:clj
   (def iatom2-protocol
     (utils/new-var
      'clojure.lang.IAtom2
      {:class clojure.lang.IAtom2
       :methods #{swap, reset, compareAndSet, swapVals, resetVals}
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
     (defmulti inst-ms* types/type-impl)

     (defmethod inst-ms* :sci.impl.protocols/reified [x]
       (let [methods (types/getMethods x)]
         ((get methods 'inst-ms*) x)))

     ;; host types (java.util.Date, java.time.Instant) and anything that
     ;; extended clojure.core/Inst outside of sci
     (defmethod inst-ms* :default [x]
       (clojure.core/inst-ms x))

     (defn inst-ms [x]
       (inst-ms* x))

     (def inst-protocol
       (utils/new-var
        'Inst
        {:protocol clojure.core/Inst
         :methods #{inst-ms*}
         :ns utils/clojure-core-ns}
        {:ns utils/clojure-core-ns}))))

;;;; end Inst
