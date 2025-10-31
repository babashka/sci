(ns sci.impl.core-protocols
  {:no-doc true}
  (:refer-clojure :exclude [deref -deref -swap! -reset!])
  (:require
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.lang :as lang]))

;;;; IDeref

(defmulti #?(:cljs -deref :default deref) types/type-impl)

(defmethod #?(:cljs -deref :default deref) :sci.impl.protocols/reified [ref]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:cljs '-deref :default 'deref)) ref)))

(def ideref-default
  (defmethod #?(:cljs -deref :default deref) :default [ref]
    (clojure.core/deref ref)))

(defn deref*
  ([x]
   #?(:cljs (if (or (instance? Atom x)
                    (implements? IDeref x))
              (clojure.core/deref x)
              (-deref x))
      :default (if (instance? clojure.lang.IDeref x)
                 (clojure.core/deref x)
                 (deref x))))
  #?@(:cljs []
      :default [([x & args] (apply clojure.core/deref x args))]))

#?(:cljs nil :default
   (def clj-lang-ns (lang/->Namespace 'clojure.lang nil)))
#?(:cljs
   (def cljs-core-ns (lang/->Namespace 'cljs.core nil)))

(def deref-protocol
  #?(:cljs
     (utils/new-var
      'cljs.core.IDeref
      {:protocol IDeref
       :methods #{-deref}
       :ns cljs-core-ns}
      {:ns cljs-core-ns})
     :default
     (utils/new-var
      'clojure.lang.IDeref
      {:class clojure.lang.IDeref
       :methods #{deref}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})))

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

(defmulti #?(:cljs -swap! :default swap) types/type-impl)
(defmulti #?(:cljs -reset! :default reset) types/type-impl)
#?(:cljs nil :default (defmulti compareAndSet types/type-impl))
#?(:cljs nil :default (defmulti swapVals types/type-impl))
#?(:cljs nil :default (defmulti resetVals types/type-impl))

;;;; Protocol methods

(defmethod #?(:cljs -swap! :default swap) :sci.impl.protocols/reified
  ([ref f]
   (let [methods (types/getMethods ref)]
     ((get methods #?(:cljs '-swap! :default 'swap)) ref f)))
  ([ref f a1]
   (let [methods (types/getMethods ref)]
     ((get methods #?(:cljs '-swap! :default 'swap)) ref f a1)))
  ([ref f a1 a2]
   (let [methods (types/getMethods ref)]
     ((get methods #?(:cljs '-swap! :default 'swap)) ref f a1 a2)))
  ([ref f a1 a2 & args]
   (let [methods (types/getMethods ref)]
     (apply (get methods #?(:cljs '-swap! :default 'swap)) ref f a1 a2 args))))

(defmethod #?(:cljs -reset! :default reset) :sci.impl.protocols/reified [ref v]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:cljs '-reset! :default 'reset)) ref v)))

#?(:cljs nil :default
   (defmethod compareAndSet :sci.impl.protocols/reified [ref old new]
     (let [methods (types/getMethods ref)]
       ((get methods 'compareAndSet) ref old new))))

#?(:cljs nil :default
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

#?(:cljs nil :default
   (defmethod resetVals :sci.impl.protocols/reified [ref v]
     (let [methods (types/getMethods ref)]
       ((get methods 'resetVals) ref v))))

;;;; Defaults

(def iatom-defaults
  [(defmethod #?(:cljs -swap! :default swap) :default [ref f & args]
     ;; TODO: optimize arities
     (apply clojure.core/swap! ref f args))

   (defmethod #?(:cljs -reset! :default reset) :default [ref v]
     (reset! ref v))

   #?@(:cljs []
       :default
       [(defmethod compareAndSet :default [ref old new]
          (compare-and-set! ref old new))
        (defmethod swapVals :default [ref & args]
          (apply swap-vals! ref args))
        (defmethod resetVals :default [ref v]
          (reset-vals! ref v))])])

;;;; Re-routing

(defn swap!* [ref f & args]
  (if
      ;; fast-path for host IAtom
      #?(:cljs (or (instance? Atom ref)
                   (implements? ISwap ref))
         :default (instance? clojure.lang.IAtom ref))
    (if args
      (apply clojure.core/swap! ref f args)
      (clojure.core/swap! ref f))
    (if args
      (apply #?(:cljs -swap! :default swap) ref f args)
      (#?(:cljs -swap! :default swap) ref f))))

(defn reset!* [ref v]
  (if
      ;; fast-path for host IAtoms
      #?(:cljs (or (instance? Atom ref)
                   (implements? IReset ref))
         :default (instance? clojure.lang.IAtom ref))
    (clojure.core/reset! ref v)
    (#?(:cljs -reset! :default reset) ref v)))

#?(:cljs nil :default
   (defn compare-and-set!* [ref old new]
     (if (instance? clojure.lang.IAtom ref)
       ;; fast-path for host IAtoms
       (clojure.core/compare-and-set! ref old new)
       (compareAndSet ref old new))))

#?(:cljs nil :default
   (defn swap-vals!* [ref f & args]
     (if (instance? clojure.lang.IAtom ref)
       (apply clojure.core/swap-vals! ref f args)
       (apply swapVals ref f args))))

#?(:cljs nil :default
   (defn reset-vals!* [ref v]
     (if (instance? clojure.lang.IAtom ref)
       (clojure.core/reset-vals! ref v)
       (resetVals ref v))))

;;;; Protocol vars

(def swap-protocol
  #?(:cljs
     (utils/new-var
      'cljs.core.ISwap
      {:protocol ISwap
       :methods #{-swap!}
       :ns cljs-core-ns}
      {:ns cljs-core-ns})
     :default
     (utils/new-var
      'clojure.lang.IAtom
      {:class clojure.lang.IAtom
       :methods #{swap, reset, compareAndSet}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})))

#?(:cljs
   (def reset-protocol
     (utils/new-var
      'cljs.core.IReset
      {:protocol IReset
       :methods #{-reset!}
       :ns cljs-core-ns}
      {:ns cljs-core-ns})))

#?(:cljs nil :default
   (def iatom2-protocol
     (utils/new-var
      'clojure.lang.IAtom2
      {:class clojure.lang.IAtom2
       :methods #{swap, reset, compareAndSet, swapVals, resetVals}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})))

;;;; end IAtom

(def defaults (set (conj iatom-defaults ideref-default)))
