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

(deftype SciAtom [home registry value-slot control-slot]
  fork/Forkable
  (fork-value [this] this)

  #?@(:clj
      [clojure.lang.IDeref
       (deref [this]
         (atom-value home registry value-slot))

       clojure.lang.IAtom2
       (reset [this new-value]
         (atom-reset! this home registry value-slot control-slot new-value))
       (compareAndSet [this old-value new-value]
         (atom-compare-and-set! this home registry value-slot control-slot
                                old-value new-value))
       (swap [this f]
         (atom-swap! this home registry value-slot control-slot f nil))
       (swap [this f x]
         (atom-swap! this home registry value-slot control-slot f [x]))
       (swap [this f x y]
         (atom-swap! this home registry value-slot control-slot f [x y]))
       (swap [this f x y more]
         (atom-swap! this home registry value-slot control-slot f
                     (list* x y more)))
       (resetVals [this new-value]
         (atom-reset-vals! this home registry value-slot control-slot new-value))
       (swapVals [this f]
         (atom-swap-vals! this home registry value-slot control-slot f nil))
       (swapVals [this f x]
         (atom-swap-vals! this home registry value-slot control-slot f [x]))
       (swapVals [this f x y]
         (atom-swap-vals! this home registry value-slot control-slot f [x y]))
       (swapVals [this f x y more]
         (atom-swap-vals! this home registry value-slot control-slot f
                          (list* x y more)))

       clojure.lang.IReference
       (meta [this]
         (atom-meta home registry control-slot))
       (alterMeta [this f args]
         (atom-alter-meta! home registry control-slot f args))
       (resetMeta [this m]
         (atom-reset-meta! home registry control-slot m))

       clojure.lang.IRef
       (setValidator [this validator]
         (atom-set-validator! home registry value-slot control-slot validator))
       (getValidator [this]
         (atom-validator home registry control-slot))
       (getWatches [this]
         (atom-watches home registry control-slot))
       (addWatch [this key f]
         (atom-add-watch! home registry control-slot key f)
         this)
       (removeWatch [this key]
         (atom-remove-watch! home registry control-slot key)
         this)]

      :cljs
      [IAtom
       IDeref
       (-deref [this]
         (atom-value home registry value-slot))
       IReset
       (-reset! [this new-value]
         (atom-reset! this home registry value-slot control-slot new-value))
       ISwap
       (-swap! [this f]
         (atom-swap! this home registry value-slot control-slot f nil))
       (-swap! [this f x]
         (atom-swap! this home registry value-slot control-slot f [x]))
       (-swap! [this f x y]
         (atom-swap! this home registry value-slot control-slot f [x y]))
       (-swap! [this f x y more]
         (atom-swap! this home registry value-slot control-slot f
                     (list* x y more)))
       IMeta
       (-meta [this]
         (atom-meta home registry control-slot))
       IWatchable
       (-notify-watches [this old-value new-value]
         (atom-notify-watches! this home registry control-slot
                               old-value new-value))
       (-add-watch [this key f]
         (atom-add-watch! home registry control-slot key f)
         this)
       (-remove-watch [this key]
         (atom-remove-watch! home registry control-slot key)
         this)
       IEquiv
       (-equiv [this other] (identical? this other))
       IHash
       (-hash [this] (goog/getUid this))]

      :cljd
       [IDeref
       (-deref [this]
         (atom-value home registry value-slot))
       IReset
       (-reset! [this new-value]
         (atom-reset! this home registry value-slot control-slot new-value))
       ISwap
       (-swap! [this f]
         (atom-swap! this home registry value-slot control-slot f nil))
       (-swap! [this f x]
         (atom-swap! this home registry value-slot control-slot f [x]))
       (-swap! [this f x y]
         (atom-swap! this home registry value-slot control-slot f [x y]))
       (-swap! [this f x y more]
         (atom-swap! this home registry value-slot control-slot f
                     (list* x y more)))
       IMeta
       (-meta [this]
         (atom-meta home registry control-slot))
       types/IResetMeta
       (-reset-meta! [this m]
         (atom-reset-meta! home registry control-slot m))
       IWatchable
       (-add-watch [this key f]
         (atom-add-watch! home registry control-slot key f)
         this)
       (-remove-watch [this key]
         (atom-remove-watch! home registry control-slot key)
         this)]))

(defn sci-atom? [x]
  (instance? SciAtom x))

(defn- control-value [home registry control-slot]
  (world/managed-value home registry control-slot))

(defn- control-value-in [selected-world home control-slot]
  (world/managed-value-in selected-world home control-slot))

(defn atom-value [home registry value-slot]
  (world/managed-value home registry value-slot))

(defn atom-meta [home registry control-slot]
  (nth (control-value home registry control-slot) meta-index))

(defn atom-validator [home registry control-slot]
  (nth (control-value home registry control-slot) validator-index))

(defn atom-watches [home registry control-slot]
  (nth (control-value home registry control-slot) watches-index))

(defn- invalid-state! []
  (throw #?(:cljd (StateError. "Invalid reference state")
            :clj (IllegalStateException. "Invalid reference state")
            :cljs (js/Error. "Validator rejected reference state"))))

(defn- validate-in! [selected-world home control-slot value]
  (let [control (control-value-in selected-world home control-slot)]
    (when-let [validator (nth control validator-index)]
      (when-not (validator value)
        (invalid-state!)))
    control))

(defn atom-notify-watches!
  ([this control old-value new-value]
   (doseq [[key f] (nth control watches-index)]
     (f key this old-value new-value))
   nil)
  ([this home registry control-slot old-value new-value]
   (atom-notify-watches!
    this (control-value home registry control-slot)
    old-value new-value)))

(defn atom-swap!
  [this home registry value-slot control-slot f args]
  (world/managed-swap!
   home registry value-slot f args
   #(validate-in! %1 home control-slot %2)
   #(atom-notify-watches! this %2 %3 %4)))

(defn atom-swap-vals!
  [this home registry value-slot control-slot f args]
  (let [result (volatile! nil)]
    (world/managed-swap!
     home registry value-slot
     (fn [old]
       (let [new (apply f old args)]
         (vreset! result [old new])
         new))
     nil
     #(validate-in! %1 home control-slot %2)
     #(atom-notify-watches! this %2 %3 %4))
    @result))

(defn atom-reset!
  [this home registry value-slot control-slot new-value]
  (world/managed-reset!
   home registry value-slot new-value
   #(validate-in! %1 home control-slot %2)
   #(atom-notify-watches! this %2 %3 %4)))

(defn atom-reset-vals!
  [this home registry value-slot control-slot new-value]
  (let [result (volatile! nil)]
    (world/managed-swap!
     home registry value-slot
     (fn [old]
       (vreset! result [old new-value])
       new-value)
     nil
     #(validate-in! %1 home control-slot %2)
     #(atom-notify-watches! this %2 %3 %4))
    @result))

(defn atom-compare-and-set!
  [this home registry value-slot control-slot old-value new-value]
  (world/managed-compare-and-set!
   home registry value-slot old-value new-value
   #(validate-in! %1 home control-slot %2)
   #(atom-notify-watches! this %2 %3 %4)))

(defn- alter-control!
  [home registry control-slot f]
  (world/managed-swap! home registry control-slot f nil
                       (fn [_ _] nil) (fn [_ _ _ _] nil)))

(defn atom-alter-meta! [home registry control-slot f args]
  (nth (alter-control!
        home registry control-slot
        #(assoc % meta-index (apply f (nth % meta-index) args)))
       meta-index))

(defn atom-reset-meta! [home registry control-slot m]
  (nth (alter-control! home registry control-slot
                       #(assoc % meta-index m))
       meta-index))

(defn atom-set-validator!
  [home registry value-slot control-slot validator]
  (when (and validator
             (not (validator (atom-value home registry value-slot))))
    (invalid-state!))
  (alter-control! home registry control-slot
                  #(assoc % validator-index validator))
  nil)

(defn atom-add-watch! [home registry control-slot key f]
  (alter-control! home registry control-slot
                  #(assoc-in % [watches-index key] f)))

(defn atom-remove-watch! [home registry control-slot key]
  (alter-control! home registry control-slot
                  #(update % watches-index dissoc key)))

(defn atom
  "Create an atom whose value and control state fork with its SCI world."
  [x & options]
  ;; Let the host implementation parse options and enforce its initial
  ;; validator semantics; it is only a short-lived construction aid.
  (let [template (apply clojure.core/atom x options)
        control [(meta template) (get-validator template) {}]
        {:keys [home registry slots managed-index]}
        (world/register-managed! :atom [x control] [0])]
    (world/attach-managed-owner!
     registry managed-index
     (SciAtom. home registry (nth slots 0) (nth slots 1)))))

(defprotocol ^:private IDelayState
  (-delay-value [state])
  (-delay-realized? [state]))

(deftype RealizedDelayState [value]
  fork/Forkable
  (fork-value [this] this)
  IDelayState
  (-delay-value [_] value)
  (-delay-realized? [_] true))

(deftype FailedDelayState [error]
  fork/Forkable
  (fork-value [this] this)
  IDelayState
  (-delay-value [_] (throw error))
  (-delay-realized? [_] true))

(defn- host-delay [thunk]
  (clojure.core/delay (thunk)))

(deftype PendingDelayState [thunk delegate]
  fork/Forkable
  (fork-value [_]
    (if (realized? delegate)
      (try
        (RealizedDelayState. @delegate)
        (catch #?(:cljd Object :clj Throwable :cljs :default) error
          (FailedDelayState. error)))
      (PendingDelayState. thunk (host-delay thunk))))
  IDelayState
  (-delay-value [_] @delegate)
  (-delay-realized? [_] (realized? delegate)))

(declare delay-value delay-realized?)

(deftype SciDelay [home registry state-slot]
  fork/Forkable
  (fork-value [this] this)

  #?@(:clj
      [clojure.lang.IDeref
       (deref [_] (delay-value home registry state-slot))
       clojure.lang.IPending
       (isRealized [_] (delay-realized? home registry state-slot))]
      :cljs
      [IDeref
       (-deref [_] (delay-value home registry state-slot))
       IPending
       (-realized? [_] (delay-realized? home registry state-slot))
       IEquiv
       (-equiv [this other] (identical? this other))
       IHash
       (-hash [this] (goog/getUid this))]
      :cljd
      [IDeref
       (-deref [_] (delay-value home registry state-slot))
       IPending
       (-realized? [_] (delay-realized? home registry state-slot))]))

(defn sci-delay? [x]
  (instance? SciDelay x))

(defn- selected-delay-state [home registry state-slot]
  (world/managed-value home registry state-slot))

(defn delay-value [home registry state-slot]
  (-delay-value (selected-delay-state home registry state-slot)))

(defn delay-realized? [home registry state-slot]
  (-delay-realized? (selected-delay-state home registry state-slot)))

(defn delay*
  "Create a delay whose realization cache follows the active SCI world."
  [thunk]
  (let [{:keys [home registry slots managed-index]}
        (world/register-managed!
         :delay [(PendingDelayState. thunk (host-delay thunk))] [0])]
    (world/attach-managed-owner!
     registry managed-index
     (SciDelay. home registry (nth slots 0)))))

(defn delay?* [x]
  (or (sci-delay? x) (delay? x)))

(defn force* [x]
  (if (sci-delay? x) @x (force x)))

#?(:clj
   (do
     (deftype PendingPromiseState []
       fork/Forkable
       (fork-value [this] this))

     (deftype DeliveredPromiseState [value]
       fork/Forkable
       (fork-value [this] this))

     (declare promise-value promise-value-with-timeout promise-realized?)

     (deftype SciPromise [home registry state-slot signal]
       fork/Forkable
       (fork-value [this] this)

       clojure.lang.IDeref
       (deref [_]
         (promise-value home registry state-slot signal))

       clojure.lang.IBlockingDeref
       (deref [_ timeout-ms timeout-val]
         (promise-value-with-timeout
          home registry state-slot signal timeout-ms timeout-val))

       clojure.lang.IPending
       (isRealized [_]
         (promise-realized? home registry state-slot)))

     (defn sci-promise? [x]
       (instance? SciPromise x))

     (defn- selected-promise-state [home registry state-slot]
       (world/managed-value home registry state-slot))

     (defn- delivered-promise-state? [state]
       (instance? DeliveredPromiseState state))

     (defn promise-value [home registry state-slot signal]
       (loop []
         (let [state (selected-promise-state home registry state-slot)]
           (if (delivered-promise-state? state)
             (.-value ^DeliveredPromiseState state)
             (do
               ;; Re-read while holding the monitor so delivery cannot occur
               ;; between observing pending and beginning to wait.
               (locking signal
                 (when-not (delivered-promise-state?
                            (selected-promise-state
                             home registry state-slot))
                   (.wait ^Object signal)))
               (recur))))))

     (defn promise-value-with-timeout
       [home registry state-slot signal timeout-ms timeout-val]
       (let [deadline (+ (System/currentTimeMillis)
                         (max 0 (long timeout-ms)))]
         (loop []
           (let [state (selected-promise-state home registry state-slot)]
             (if (delivered-promise-state? state)
               (.-value ^DeliveredPromiseState state)
               (let [remaining (- deadline (System/currentTimeMillis))]
                 (if-not (pos? remaining)
                   timeout-val
                   (do
                     (locking signal
                       (when-not (delivered-promise-state?
                                  (selected-promise-state
                                   home registry state-slot))
                         (.wait ^Object signal (long remaining))))
                     (recur)))))))))

     (defn promise-realized? [home registry state-slot]
       (delivered-promise-state?
        (selected-promise-state home registry state-slot)))

     (defn promise*
       "Create a promise with an independent pending delivery cell per world."
       []
       (let [{:keys [home registry slots managed-index]}
             (world/register-managed!
              :promise [(PendingPromiseState.)] [0])]
         (world/attach-managed-owner!
          registry managed-index
          (SciPromise. home registry (nth slots 0) (Object.)))))

     (defn deliver* [p value]
       (if (sci-promise? p)
         (let [^SciPromise p p]
           (world/managed-swap!
            (.-home p) (.-registry p) (.-state-slot p)
            (fn [state]
              (if (delivered-promise-state? state)
                state
                (DeliveredPromiseState. value)))
            nil (fn [_ _] nil) (fn [_ _ _ _] nil))
           (locking (.-signal p)
             (.notifyAll ^Object (.-signal p)))
           p)
         (deliver p value)))))

(defn memoize*
  "Clojure-compatible memoize whose cache follows the active SCI world."
  [f]
  (if (world/current-world)
    (let [mem (atom {})]
      (fn [& args]
        (if-let [entry (find @mem args)]
          (val entry)
          (let [ret (apply f args)]
            (swap! mem assoc args ret)
            ret))))
    (clojure.core/memoize f)))

(defn get-validator* [ref]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-validator (.-home ref) (.-registry ref)
                      (.-control-slot ref)))
    (get-validator ref)))

(defn set-validator!* [ref validator]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-set-validator! (.-home ref) (.-registry ref)
                           (.-value-slot ref) (.-control-slot ref)
                           validator))
    (set-validator! ref validator)))

(defn alter-meta!* [ref f & args]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-alter-meta! (.-home ref) (.-registry ref)
                        (.-control-slot ref) f args))
    (apply alter-meta! ref f args)))

(defn reset-meta!* [ref m]
  (if (sci-atom? ref)
    (let [^SciAtom ref ref]
      (atom-reset-meta! (.-home ref) (.-registry ref)
                        (.-control-slot ref) m))
    (reset-meta! ref m)))
