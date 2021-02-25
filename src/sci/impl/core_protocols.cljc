(ns sci.impl.core-protocols
  {:no-doc true}
  (:refer-clojure :exclude [deref -deref -swap! -reset!])
  (:require [sci.impl.types :as types]
            [sci.impl.vars :as vars]))


;;;; IDeref

(defmulti #?(:clj deref :cljs -deref) types/type-impl)

(defmethod #?(:clj deref :cljs -deref) :sci.impl.protocols/reified [ref]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:clj 'deref :cljs '-deref)) ref)))

(def ideref-default
  (defmethod #?(:clj deref :cljs -deref) :default [ref]
    (clojure.core/deref ref)))

(defn deref*
  ([x]
   #?(:clj (deref x)
      :cljs (-deref x)))
  #?(:clj
     ([x & args]
      (apply clojure.core/deref x args))))

#?(:clj
   (def clj-lang-ns (vars/->SciNamespace 'clojure.lang nil)))
#?(:cljs
   (def cljs-core-ns (vars/->SciNamespace 'cljs.core nil)))

(def deref-protocol
  #?(:clj
     (vars/new-var
      'clojure.lang.IDeref
      {:class clojure.lang.IDeref
       :methods #{deref}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})
     :cljs
     (vars/new-var
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

(defmulti #?(:clj swap :cljs -swap!) types/type-impl)
(defmulti #?(:clj reset :cljs -reset!) types/type-impl)
#?(:clj (defmulti compareAndSet types/type-impl))
#?(:clj (defmulti swapVals types/type-impl))
#?(:clj (defmulti resetVals types/type-impl))

;;;; Protocol methods

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
     (apply (get methods #?(:clj 'swap :cljs '-swap!)) ref f a1 a2 args))))

(defmethod #?(:clj reset :cljs -reset!) :sci.impl.protocols/reified [ref v]
  (let [methods (types/getMethods ref)]
    ((get methods #?(:clj 'reset :cljs '-reset!)) ref v)))

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

(def iatom-defaults
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
        (reset-vals! ref v)))])

;;;; Re-routing

(defn swap!* [ref f & args]
  ;; TODO: optimize arities - maybe test first how much this matters at all
  ;; For CLJ I guess we can directly use the multimethods
  (if args
    (apply #?(:clj swap :cljs -swap!) ref f args)
    (#?(:clj swap :cljs -swap!) ref f)))

(defn reset!* [ref v]
  (#?(:clj reset :cljs -reset!) ref v))

#?(:clj
   (defn compare-and-set!* [ref old new]
     (compareAndSet ref old new)))

#?(:clj
   (defn swap-vals!* [ref f & args]
     (apply swapVals ref f args)))

#?(:clj
   (defn reset-vals!* [ref v]
     (resetVals ref v)))

;;;; Protocol vars

(def swap-protocol
  #?(:clj
     (vars/new-var
      'clojure.lang.IAtom
      {:class clojure.lang.IAtom
       :methods #{swap, reset, compareAndSet}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})
     :cljs
     (vars/new-var
      'cljs.core.ISwap
      {:protocol ISwap
       :methods #{-swap!}
       :ns cljs-core-ns}
      {:ns cljs-core-ns})))

#?(:cljs
   (def reset-protocol
     (vars/new-var
      'cljs.core.IReset
      {:protocol IReset
       :methods #{-reset!}
       :ns cljs-core-ns}
      {:ns cljs-core-ns})))

#?(:clj
   (def iatom2-protocol
     (vars/new-var
      'clojure.lang.IAtom2
      {:class clojure.lang.IAtom2
       :methods #{swap, reset, compareAndSet, swapVals, resetVals}
       :ns clj-lang-ns}
      {:ns clj-lang-ns})))

;;;; end IAtom

(def defaults (set (conj iatom-defaults ideref-default)))
