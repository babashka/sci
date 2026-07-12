(ns sci.impl.core-protocols
  {:no-doc true}
  (:refer-clojure :exclude [deref -deref -swap! -reset!])
  (:require
   [sci.impl.deftype]
   #?(:cljd [sci.impl.multimethods :as mm])
   #?(:cljs [sci.impl.copy-vars :as copy-vars])
   [sci.impl.records]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.lang :as lang])
  #?@(:cljd [] :clj [(:import [sci.impl.records SciRecord]
                              [sci.impl.deftype SciType])]))

;;;; IDeref

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

;; on CLJS sci types implement the protocols natively, so clojure.core's
;; deref/swap!/reset! are exposed directly and no re-routing wrappers exist
#?(:cljd
   (defn deref* [x]
     (if (satisfies? IDeref x)
       (clojure.core/deref x)
       (-deref x)))
   :clj
   (defn deref*
     ([x]
      (if (instance? clojure.lang.IDeref x)
        (clojure.core/deref x)
        (deref x)))
     ([x & args]
      (apply clojure.core/deref x args))))

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

#?(:cljd nil :cljs nil :clj
   (defn swap!* [ref f & args]
     (if
         ;; fast-path for host IAtom
         (instance? clojure.lang.IAtom ref)
       (if args
         (apply clojure.core/swap! ref f args)
         (clojure.core/swap! ref f))
       (if args
         (apply swap ref f args)
         (swap ref f)))))

#?(:cljd
   (defn swap!* [ref f & args]
     (if (or (instance? cljd.core/Atom ref)
             (satisfies? ISwap ref))
       (if args
         (apply clojure.core/swap! ref f args)
         (clojure.core/swap! ref f))
       (if args
         (apply -swap! ref f args)
         (-swap! ref f)))))

#?(:cljd
   (defn reset!* [ref v]
     (if (or (instance? cljd.core/Atom ref)
             (satisfies? IReset ref))
       (clojure.core/reset! ref v)
       (-reset! ref v)))
   :clj
   (defn reset!* [ref v]
     (if (instance? clojure.lang.IAtom ref)
       (clojure.core/reset! ref v)
       (reset ref v))))

#?(:clj
   (defn compare-and-set!* [ref old new]
     (if (instance? clojure.lang.IAtom ref)
       ;; fast-path for host IAtoms
       (clojure.core/compare-and-set! ref old new)
       (compareAndSet ref old new))))

#?(:clj
   (defn swap-vals!* [ref f & args]
     (if (instance? clojure.lang.IAtom ref)
       (apply clojure.core/swap-vals! ref f args)
       (apply swapVals ref f args))))

#?(:clj
   (defn reset-vals!* [ref v]
     (if (instance? clojure.lang.IAtom ref)
       (clojure.core/reset-vals! ref v)
       (resetVals ref v))))

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
