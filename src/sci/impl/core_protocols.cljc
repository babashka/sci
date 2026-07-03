(ns sci.impl.core-protocols
  {:no-doc true}
  (:refer-clojure :exclude [deref -deref -swap! -reset!])
  (:require
   [sci.impl.deftype]
   [sci.impl.records]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.lang :as lang])
  #?@(:cljd [] :clj [(:import [sci.impl.records SciRecord]
                              [sci.impl.deftype SciType])]))

;;;; IDeref

(defmulti #?(:cljd -deref :clj deref :cljs -deref) types/type-impl)

(defmethod #?(:cljd -deref :clj deref :cljs -deref) :sci.impl.protocols/reified [ref]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:cljd '-deref :clj 'deref :cljs '-deref)) ref)))

;; on cljd defmethod must stay top level, ideref-default is only used to
;; build the internal defaults set
#?(:cljd
   (defmethod -deref :default [ref]
     (clojure.core/deref ref)))

(def ideref-default
  #?(:cljd nil
     :clj (defmethod deref :default [ref]
            (clojure.core/deref ref))
     :cljs (defmethod -deref :default [ref]
             (clojure.core/deref ref))))

(defn deref*
  ([x]
   #?(:cljd (if (satisfies? IDeref x)
              (clojure.core/deref x)
              (-deref x))
      :clj (if (instance? clojure.lang.IDeref x)
             (clojure.core/deref x)
             (deref x))
      :cljs (if (or (instance? Atom x)
                    (implements? IDeref x))
              (clojure.core/deref x)
              (-deref x))))
  #?@(:cljd [] :clj
      [([x & args]
        (apply clojure.core/deref x args))]))

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
      {:protocol IDeref
       :methods #{-deref}
       :ns cljs-core-ns}
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

(defmulti #?(:cljd -swap! :clj swap :cljs -swap!) types/type-impl)
(defmulti #?(:cljd -reset! :clj reset :cljs -reset!) types/type-impl)
#?(:clj (defmulti compareAndSet types/type-impl))
#?(:clj (defmulti swapVals types/type-impl))
#?(:clj (defmulti resetVals types/type-impl))

;;;; Protocol methods

;; single arity: cljd defmethod does not support multi-arity
#?(:cljd
   (defmethod -swap! :sci.impl.protocols/reified [ref f & args]
     (let [methods (types/getMethods ref)]
       (apply (get methods '-swap!) ref f args))))

#?(:cljd nil
   :default
   (defmethod #?(:clj swap :cljs -swap!) :sci.impl.protocols/reified
     ([ref f]
      (let [methods (types/getMethods ref)]
        ((get methods #?(:clj 'swap :cljs '-swap!)) ref f)))
     ([ref f a1]
      (let [methods (types/getMethods ref)]
        ((get methods #?(:clj 'swap :cljs '-swap!)) ref f a1)))
     ([ref f a1 a2]
      (let [methods (types/getMethods ref)]
        ((get methods #?(:clj 'swap :cljs '-swap!)) ref f a1 a2)))
     ([ref f a1 a2 & args]
      (let [methods (types/getMethods ref)]
        (apply (get methods #?(:clj 'swap :cljs '-swap!)) ref f a1 a2 args)))))

(defmethod #?(:cljd -reset! :clj reset :cljs -reset!) :sci.impl.protocols/reified [ref v]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:cljd '-reset! :clj 'reset :cljs '-reset!)) ref v)))

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

#?(:cljd
   (defmethod -swap! :default [ref f & args]
     (apply clojure.core/swap! ref f args)))

#?(:cljd
   (defmethod -reset! :default [ref v]
     (reset! ref v)))

(def iatom-defaults
  #?(:cljd []
     :default
     [(defmethod #?(:clj swap :cljs -swap!) :default [ref f & args]
        ;; TODO: optimize arities
        (apply clojure.core/swap! ref f args))

      (defmethod #?(:clj reset :cljs -reset!) :default [ref v]
        (reset! ref v))

      #?(:clj
         (defmethod compareAndSet :default [ref old new]
           (compare-and-set! ref old new)))

      #?(:clj
         (defmethod swapVals :default [ref & args]
           (apply swap-vals! ref args)))

      #?(:clj
         (defmethod resetVals :default [ref v]
           (reset-vals! ref v)))]))

;;;; Re-routing

(defn swap!* [ref f & args]
  (if
      ;; fast-path for host IAtom
      #?(:cljd (or (instance? cljd.core/Atom ref)
                   (satisfies? ISwap ref))
         :cljs (or (instance? Atom ref)
                   (implements? ISwap ref))
         :clj (instance? clojure.lang.IAtom ref))
    (if args
      (apply clojure.core/swap! ref f args)
      (clojure.core/swap! ref f))
    (if args
      (apply #?(:cljd -swap! :clj swap :cljs -swap!) ref f args)
      (#?(:cljd -swap! :clj swap :cljs -swap!) ref f))))

(defn reset!* [ref v]
  (if
      ;; fast-path for host IAtoms
      #?(:cljd (or (instance? cljd.core/Atom ref)
                   (satisfies? IReset ref))
         :cljs (or (instance? Atom ref)
                   (implements? IReset ref))
         :clj (instance? clojure.lang.IAtom ref))
    (clojure.core/reset! ref v)
    (#?(:cljd -reset! :clj reset :cljs -reset!) ref v)))

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
      {:protocol ISwap
       :methods #{-swap!}
       :ns cljs-core-ns}
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
      {:protocol IReset
       :methods #{-reset!}
       :ns cljs-core-ns}
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
   (defmethod types/sci-pr-writer :sci.impl.protocols/reified [this w opts]
     (if-let [pm (get (types/getMethods this) '-pr-writer)]
       (pm this w opts)
       (-write w "#object[sci.impl.types.Reified]"))))

#?(:cljs
   (defn -pr-writer* [this writer opts]
     (cljs.core/-pr-writer this writer opts)))

#?(:cljs
   (def print-writer-protocol
     (utils/new-var
      'cljs.core.IPrintWithWriter
      {:protocol IPrintWithWriter
       :methods #{types/sci-pr-writer}
       :ns cljs-core-ns}
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
    ;; no get-method on cljd, sci-invoke methods are never registered there yet
    #?(:cljd false
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

(def defaults (set (conj iatom-defaults ideref-default)))
