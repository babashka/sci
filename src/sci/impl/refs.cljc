(ns sci.impl.refs
  "Self-describing mutable references whose state is relative to an SCI world."
  (:refer-clojure :exclude [atom])
  (:require [sci.fork :as fork]
            [sci.impl.types :as types]
            [sci.impl.world :as world]))

(def ^:private meta-index 0)
(def ^:private validator-index 1)
(def ^:private watches-index 2)

(declare atom-value atom-reset! atom-reset-vals! atom-swap! atom-swap-vals!
         atom-compare-and-set! atom-meta atom-alter-meta! atom-reset-meta!
         atom-validator atom-set-validator! atom-watches atom-add-watch!
         atom-remove-watch! atom-notify-watches!)

(deftype SciAtom [home registry value-slot control-slot initial-value initial-control]
  fork/Forkable
  (fork-value [this] this)

  #?@(:clj
      [clojure.lang.IDeref
       (deref [this]
         (atom-value home registry value-slot initial-value))

       clojure.lang.IAtom2
       (reset [this new-value]
         (atom-reset! this home registry value-slot control-slot
                      initial-value initial-control new-value))
       (compareAndSet [this old-value new-value]
         (atom-compare-and-set! this home registry value-slot control-slot
                                initial-value initial-control
                                old-value new-value))
       (swap [this f]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f nil))
       (swap [this f x]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f [x]))
       (swap [this f x y]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f [x y]))
       (swap [this f x y more]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f (list* x y more)))
       (resetVals [this new-value]
         (atom-reset-vals! this home registry value-slot control-slot
                           initial-value initial-control new-value))
       (swapVals [this f]
         (atom-swap-vals! this home registry value-slot control-slot
                          initial-value initial-control f nil))
       (swapVals [this f x]
         (atom-swap-vals! this home registry value-slot control-slot
                          initial-value initial-control f [x]))
       (swapVals [this f x y]
         (atom-swap-vals! this home registry value-slot control-slot
                          initial-value initial-control f [x y]))
       (swapVals [this f x y more]
         (atom-swap-vals! this home registry value-slot control-slot
                          initial-value initial-control f (list* x y more)))

       clojure.lang.IReference
       (meta [this]
         (atom-meta home registry control-slot initial-control))
       (alterMeta [this f args]
         (atom-alter-meta! home registry control-slot initial-control f args))
       (resetMeta [this m]
         (atom-reset-meta! home registry control-slot initial-control m))

       clojure.lang.IRef
       (setValidator [this validator]
         (atom-set-validator! home registry value-slot control-slot
                              initial-value initial-control validator))
       (getValidator [this]
         (atom-validator home registry control-slot initial-control))
       (getWatches [this]
         (atom-watches home registry control-slot initial-control))
       (addWatch [this key f]
         (atom-add-watch! home registry control-slot initial-control key f)
         this)
       (removeWatch [this key]
         (atom-remove-watch! home registry control-slot initial-control key)
         this)]

      :cljs
      [IAtom
       IDeref
       (-deref [this]
         (atom-value home registry value-slot initial-value))
       IReset
       (-reset! [this new-value]
         (atom-reset! this home registry value-slot control-slot
                      initial-value initial-control new-value))
       ISwap
       (-swap! [this f]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f nil))
       (-swap! [this f x]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f [x]))
       (-swap! [this f x y]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f [x y]))
       (-swap! [this f x y more]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f (list* x y more)))
       IMeta
       (-meta [this]
         (atom-meta home registry control-slot initial-control))
       IWatchable
       (-notify-watches [this old-value new-value]
         (atom-notify-watches! this home registry control-slot initial-control
                               old-value new-value))
       (-add-watch [this key f]
         (atom-add-watch! home registry control-slot initial-control key f)
         this)
       (-remove-watch [this key]
         (atom-remove-watch! home registry control-slot initial-control key)
         this)
       IEquiv
       (-equiv [this other] (identical? this other))
       IHash
       (-hash [this] (goog/getUid this))]

      :cljd
      [IDeref
       (-deref [this]
         (atom-value home registry value-slot initial-value))
       IReset
       (-reset! [this new-value]
         (atom-reset! this home registry value-slot control-slot
                      initial-value initial-control new-value))
       ISwap
       (-swap! [this f]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f nil))
       (-swap! [this f x]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f [x]))
       (-swap! [this f x y]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f [x y]))
       (-swap! [this f x y more]
         (atom-swap! this home registry value-slot control-slot
                     initial-value initial-control f (list* x y more)))
       IMeta
       (-meta [this]
         (atom-meta home registry control-slot initial-control))
       types/IResetMeta
       (-reset-meta! [this m]
         (atom-reset-meta! home registry control-slot initial-control m))
       IWatchable
       (-add-watch [this key f]
         (atom-add-watch! home registry control-slot initial-control key f)
         this)
       (-remove-watch [this key]
         (atom-remove-watch! home registry control-slot initial-control key)
         this)]))

(defn sci-atom? [x]
  (instance? SciAtom x))

(defn- control-value [home registry control-slot initial-control]
  (world/managed-value home registry control-slot initial-control))

(defn- control-value-in [selected-world control-slot initial-control]
  (world/managed-value-in selected-world control-slot initial-control))

(defn atom-value [home registry value-slot initial-value]
  (world/managed-value home registry value-slot initial-value))

(defn atom-meta [home registry control-slot initial-control]
  (nth (control-value home registry control-slot initial-control) meta-index))

(defn atom-validator [home registry control-slot initial-control]
  (nth (control-value home registry control-slot initial-control)
       validator-index))

(defn atom-watches [home registry control-slot initial-control]
  (nth (control-value home registry control-slot initial-control) watches-index))

(defn- invalid-state! []
  (throw #?(:cljd (StateError. "Invalid reference state")
            :clj (IllegalStateException. "Invalid reference state")
            :cljs (js/Error. "Validator rejected reference state"))))

(defn- validate-in! [selected-world control-slot initial-control value]
  (let [control (control-value-in selected-world control-slot initial-control)]
    (when-let [validator (nth control validator-index)]
      (when-not (validator value)
        (invalid-state!)))
    control))

(defn atom-notify-watches!
  ([this control old-value new-value]
   (doseq [[key f] (nth control watches-index)]
     (f key this old-value new-value))
   nil)
  ([this home registry control-slot initial-control old-value new-value]
   (atom-notify-watches!
    this (control-value home registry control-slot initial-control)
    old-value new-value)))

(defn atom-swap!
  [this home registry value-slot control-slot initial-value initial-control f args]
  (world/managed-swap!
   home registry value-slot initial-value f args
   #(validate-in! %1 control-slot initial-control %2)
   #(atom-notify-watches! this %2 %3 %4)))

(defn atom-swap-vals!
  [this home registry value-slot control-slot initial-value initial-control f args]
  (let [result (volatile! nil)]
    (world/managed-swap!
     home registry value-slot initial-value
     (fn [old]
       (let [new (apply f old args)]
         (vreset! result [old new])
         new))
     nil
     #(validate-in! %1 control-slot initial-control %2)
     #(atom-notify-watches! this %2 %3 %4))
    @result))

(defn atom-reset!
  [this home registry value-slot control-slot initial-value initial-control new-value]
  (world/managed-reset!
   home registry value-slot initial-value new-value
   #(validate-in! %1 control-slot initial-control %2)
   #(atom-notify-watches! this %2 %3 %4)))

(defn atom-reset-vals!
  [this home registry value-slot control-slot initial-value initial-control new-value]
  (let [result (volatile! nil)]
    (world/managed-swap!
     home registry value-slot initial-value
     (fn [old]
       (vreset! result [old new-value])
       new-value)
     nil
     #(validate-in! %1 control-slot initial-control %2)
     #(atom-notify-watches! this %2 %3 %4))
    @result))

(defn atom-compare-and-set!
  [this home registry value-slot control-slot initial-value initial-control
   old-value new-value]
  (world/managed-compare-and-set!
   home registry value-slot initial-value old-value new-value
   #(validate-in! %1 control-slot initial-control %2)
   #(atom-notify-watches! this %2 %3 %4)))

(defn- alter-control!
  [home registry control-slot initial-control f]
  (world/managed-swap! home registry control-slot initial-control f nil
                       (fn [_ _] nil) (fn [_ _ _ _] nil)))

(defn atom-alter-meta! [home registry control-slot initial-control f args]
  (nth (alter-control!
        home registry control-slot initial-control
        #(assoc % meta-index (apply f (nth % meta-index) args)))
       meta-index))

(defn atom-reset-meta! [home registry control-slot initial-control m]
  (nth (alter-control! home registry control-slot initial-control
                       #(assoc % meta-index m))
       meta-index))

(defn atom-set-validator!
  [home registry value-slot control-slot initial-value initial-control validator]
  (when (and validator
             (not (validator (atom-value home registry value-slot initial-value))))
    (invalid-state!))
  (alter-control! home registry control-slot initial-control
                  #(assoc % validator-index validator))
  nil)

(defn atom-add-watch! [home registry control-slot initial-control key f]
  (alter-control! home registry control-slot initial-control
                  #(assoc-in % [watches-index key] f)))

(defn atom-remove-watch! [home registry control-slot initial-control key]
  (alter-control! home registry control-slot initial-control
                  #(update % watches-index dissoc key)))

(defn atom
  "Create an atom whose value and control state fork with its SCI world."
  [x & options]
  ;; Let the host implementation parse options and enforce its initial
  ;; validator semantics; it is only a short-lived construction aid.
  (let [template (apply clojure.core/atom x options)
        control [(meta template) (get-validator template) {}]
        {:keys [home registry slots]}
        (world/register-managed! :atom [x control] [0])]
    (SciAtom. home registry (nth slots 0) (nth slots 1) x control)))

(defn get-validator* [ref]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-validator (.-home ref) (.-registry ref)
                      (.-control-slot ref) (.-initial-control ref)))
    (get-validator ref)))

(defn set-validator!* [ref validator]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-set-validator! (.-home ref) (.-registry ref)
                           (.-value-slot ref) (.-control-slot ref)
                           (.-initial-value ref) (.-initial-control ref)
                           validator))
    (set-validator! ref validator)))

(defn alter-meta!* [ref f & args]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-alter-meta! (.-home ref) (.-registry ref)
                        (.-control-slot ref) (.-initial-control ref) f args))
    (apply alter-meta! ref f args)))

(defn reset-meta!* [ref m]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-reset-meta! (.-home ref) (.-registry ref)
                        (.-control-slot ref) (.-initial-control ref) m))
    (reset-meta! ref m)))
