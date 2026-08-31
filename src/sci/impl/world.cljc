(ns sci.impl.world
  "Dense, fork-local runtime state for SCI contexts.

  Stable handles are assigned integer slots once per context lineage. A world
  stores slot values densely and a frozen fork copies that array while the
  source world is quiescent. Mutable primitives CAS only their own slot, so
  unrelated atoms never contend on a shared world root."
  (:require [sci.ctx-store :as store]
            [sci.impl.execution :as execution])
  #?(:clj (:import [java.lang.ref WeakReference]
                   [java.util.concurrent.atomic AtomicReferenceArray]
                   [java.util.concurrent.locks ReentrantReadWriteLock])))

(def absent #?(:cljd (Object.) :clj (Object.) :cljs (js/Object.)))

(deftype DenseWorld [cells-holder registry gate persistent?])
(deftype ReadWorld [world primary?])

(defn read-world [world primary?]
  (ReadWorld. world primary?))

(defn- new-cells [n]
  #?(:clj
     (let [a (AtomicReferenceArray. (int n))]
       (dotimes [i n] (.set a i absent))
       a)
     :cljs
     (let [a (object-array n)]
       (dotimes [i n] (aset a i absent))
       a)
     :cljd
     (#/(List/filled dynamic) n absent)))

(defn- cells-length [cells]
  #?(:clj (.length ^AtomicReferenceArray cells)
     :cljs (alength cells)
     :cljd (.-length ^List cells)))

(defn- cells-get [cells slot]
  #?(:clj (.get ^AtomicReferenceArray cells (int slot))
     :cljs (aget cells slot)
     :cljd (aget ^List cells slot)))

(defn- cells-set! [cells slot value]
  #?(:clj (.set ^AtomicReferenceArray cells (int slot) value)
     :cljs (aset cells slot value)
     :cljd (aset ^List cells slot value))
  value)

(defn- cells-cas! [cells slot old new]
  #?(:clj (.compareAndSet ^AtomicReferenceArray cells (int slot) old new)
     :default
     (if (identical? old (cells-get cells slot))
       (do (cells-set! cells slot new) true)
       false)))

(defn- current-cells [^DenseWorld world]
  @(.-cells-holder world))

(defn- with-registry-lock [registry f]
  #?(:clj (locking registry (f))
     :default (f)))

(defn- ensure-capacity! [^DenseWorld world required]
  (let [holder (.-cells-holder world)]
    (when (> required (cells-length @holder))
      (with-registry-lock
        holder
        (fn []
          (let [old @holder
                old-n (cells-length old)]
            (when (> required old-n)
              (let [new-n (loop [n (max 16 old-n)]
                            (if (>= n required) n (recur (* 2 n))))
                    new (new-cells new-n)]
                (dotimes [i old-n]
                  (cells-set! new i (cells-get old i)))
                (vreset! holder new))))))))
  world)

(defn new-world []
  (DenseWorld.
   (volatile! (new-cells 64))
   (atom {:next-slot 0 :descriptors {} :managed-descriptors []})
   #?(:clj (ReentrantReadWriteLock.) :default nil)
   (volatile! false)))

(defn active-world-state []
  #?(:clj (let [state (.get ^ThreadLocal execution/current)]
            (aget ^objects state execution/active-world-index))
     :default (execution/active-world)))

(defn- call-with-active-world [ctx f]
  (let [state #?(:clj (.get ^ThreadLocal execution/current)
                 :default (execution/current-state))
        previous (execution/active-world state)
        previous-scope (execution/binding-scope state)
        active (:sci.impl/read-world ctx)]
    (execution/set-active-world! state active)
    (execution/set-binding-scope! state (:sci.impl/world ctx))
    (execution/refresh-active-bindings! state)
    (try
      (f)
      (finally
        (execution/set-active-world! state previous)
        (execution/set-binding-scope! state previous-scope)
        (execution/refresh-active-bindings! state)))))

(defn with-active-world
  "Run `f` with the context's world installed. On the JVM evaluations take a
  shared gate permit; a fork takes the exclusive permit and therefore observes
  a quiescent source world without adding locks to individual reads."
  [ctx f]
  #?(:clj
     (let [^DenseWorld world (:sci.impl/world ctx)
           lock (.readLock ^ReentrantReadWriteLock (.-gate world))]
       (.lock lock)
       (try
         (call-with-active-world ctx f)
         (finally (.unlock lock))))
     :default
     (call-with-active-world ctx f)))

(defn current-world []
  (:sci.impl/world store/*ctx*))

(defn binding-scope
  "Return the currently executing world for dynamic-binding isolation. Nil
  denotes a host binding established outside managed SCI evaluation."
  []
  (execution/binding-scope))

(defn- descriptor [^DenseWorld world handle]
  (get-in @(.-registry world) [:descriptors handle]))

(defn slot-of
  "Return the value slot assigned to `handle`, or nil when it is not registered
  in this lineage. Intended for analyzer-time resolution."
  [world handle]
  (:value-slot (descriptor world handle)))

(defn- present-slot? [^DenseWorld world slot]
  (and (some? slot)
       (< slot (cells-length (current-cells world)))
       (not (identical? absent (cells-get (current-cells world) slot)))))

(defn registered? [world handle]
  (when-let [desc (descriptor world handle)]
    (present-slot? world (:value-slot desc))))

(defn primary-world? []
  (let [active (active-world-state)]
    (if active
      (.-primary? ^ReadWorld active)
      (true? (:sci.impl/primary? store/*ctx*)))))

(defn mutable-primary-world? []
  (let [active (active-world-state)]
    (and active
         (.-primary? ^ReadWorld active)
         (not @(.-persistent? ^DenseWorld (.-world ^ReadWorld active))))))

(defn tracked? [handle]
  (when-let [^ReadWorld active (active-world-state)]
    (registered? (.-world active) handle)))

(defn- slot-value [^DenseWorld world slot fallback]
  (if (nil? slot)
    fallback
    (let [cells (current-cells world)]
      (if (>= slot (cells-length cells))
        fallback
        (let [v (cells-get cells slot)]
          (if (identical? absent v) fallback v))))))

(defn value [handle fallback]
  (if-let [^ReadWorld active (active-world-state)]
    (let [world (.-world active)]
      (slot-value world (:value-slot (descriptor world handle)) fallback))
    fallback))

(defn var-value-at
  "Read a Var through a slot already resolved by the analyzer."
  [slot fallback]
  (let [^ReadWorld active (active-world-state)]
    (if (or (nil? active) (.-primary? active))
      fallback
      (slot-value (.-world active) slot fallback))))

(defn var-value
  "Primary contexts retain their direct Var realization. Descendants resolve
  the stable Var handle through its dense lineage slot."
  [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (.-primary? ^ReadWorld active))
      fallback
      (let [world (.-world ^ReadWorld active)]
        (slot-value world (:value-slot (descriptor world handle)) fallback)))))

(defn var-meta [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (.-primary? ^ReadWorld active))
      fallback
      (let [world (.-world ^ReadWorld active)]
        (slot-value world (:meta-slot (descriptor world handle)) fallback)))))

(defn namespace-meta [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (.-primary? ^ReadWorld active))
      fallback
      (let [world (.-world ^ReadWorld active)]
        (slot-value world (:value-slot (descriptor world handle)) fallback)))))

(defn var-watches [handle fallback]
  (if-let [^ReadWorld active (active-world-state)]
    (let [world (.-world active)
          {:keys [watch-slot watch-fallback]} (descriptor world handle)]
      (if watch-slot
        (slot-value world watch-slot watch-fallback)
        fallback))
    fallback))

(defn- allocate-descriptor!
  [^DenseWorld world handle kind host-fork? value-fork-fn]
  (let [registry (.-registry world)]
    (with-registry-lock
      registry
      (fn []
        (if-let [desc (get-in @registry [:descriptors handle])]
          desc
          (let [start (:next-slot @registry)
                width (if (= :var kind) 2 1)
                desc (cond-> {:kind kind
                              :value-slot start
                              :host-fork? host-fork?}
                       value-fork-fn (assoc :value-fork-fn value-fork-fn)
                       (= :var kind) (assoc :meta-slot (inc start)))]
            (swap! registry
                   (fn [r]
                     (-> r
                         (assoc :next-slot (+ start width))
                         (assoc-in [:descriptors handle] desc))))
            desc))))))

(defn register-managed!
  "Allocate dense slots whose owner carries the returned descriptor directly.
  Managed descriptors contain no owner reference, so an unreachable SCI ref is
  not retained by the lineage registry. `fork-indexes` names values that need
  host `Forkable`/`:fork-fn` processing when a child world is copied."
  [kind values fork-indexes]
  (let [^DenseWorld world (current-world)]
    (when-not world
      (throw (ex-info "Managed SCI state must be created inside an SCI context."
                      {:kind kind})))
    (let [registry (.-registry world)
          desc (with-registry-lock
                 registry
                 (fn []
                   (let [start (:next-slot @registry)
                         slots (mapv #(+ start %) (range (count values)))
                         fork-slots (mapv #(nth slots %) fork-indexes)
                         managed-index (count (:managed-descriptors @registry))
                         desc {:kind kind
                               :managed-index managed-index
                               :slots slots
                               :fork-slots fork-slots
                               :owner-ref nil}]
                     (swap! registry
                            (fn [r]
                              (-> r
                                  (assoc :next-slot (+ start (count values)))
                                  (update :managed-descriptors conj desc))))
                     desc)))]
      (ensure-capacity! world (:next-slot @registry))
      (doseq [[slot value] (map vector (:slots desc) values)]
        (cells-set! (current-cells world) slot value))
      (assoc desc :home world :registry registry))))

(defn attach-managed-owner!
  "Attach weak ownership after constructing a self-described handle. The
  owner is never retained strongly by the lineage registry."
  [registry managed-index owner]
  (let [owner-ref #?(:clj (WeakReference. owner)
                     :cljs (when (exists? js/WeakRef) (js/WeakRef. owner))
                     ;; Dart weak-reference availability varies with the host
                     ;; SDK. The descriptor remains non-owning until a
                     ;; portable implementation is available.
                     :cljd nil)]
    (when owner-ref
      (with-registry-lock
        registry
        #(swap! registry assoc-in
                [:managed-descriptors managed-index :owner-ref]
                owner-ref)))
    owner))

(defn- live-managed-owner? [owner-ref]
  (or (nil? owner-ref)
      #?(:clj (some? (.get ^WeakReference owner-ref))
         :cljs (some? (.deref owner-ref))
         :cljd true)))

(defn- sweep-managed! [^DenseWorld world]
  (let [cells (current-cells world)]
    (doseq [{:keys [owner-ref slots]}
            (:managed-descriptors @(.-registry world))
            :when (not (live-managed-owner? owner-ref))
            slot slots
            :when (< slot (cells-length cells))]
      (cells-set! cells slot absent))))

(defn- selected-managed-world [^DenseWorld home registry]
  (let [active (active-world-state)]
    (if (and active
             (identical? registry
                         (.-registry ^DenseWorld (.-world ^ReadWorld active))))
      (.-world ^ReadWorld active)
      home)))

(defn managed-value
  "Read a self-described managed slot in the active related world, falling
  back to the owner's creation world outside managed evaluation."
  [home registry slot]
  (let [selected (selected-managed-world home registry)]
    (if (identical? selected home)
      (slot-value selected slot absent)
      (let [value (slot-value selected slot absent)]
        (if (identical? value absent)
          (slot-value home slot absent)
          value)))))

(defn managed-value-in
  "Read a managed slot from an already selected world."
  [world home slot]
  (if (identical? world home)
    (slot-value world slot absent)
    (let [value (slot-value world slot absent)]
      (if (identical? value absent)
        (slot-value home slot absent)
        value))))

(defn- call-with-managed-mutation [^DenseWorld home registry f]
  (let [active (active-world-state)
        related? (and active
                      (identical? registry
                                  (.-registry ^DenseWorld
                                              (.-world ^ReadWorld active))))
        ^DenseWorld world (if related? (.-world ^ReadWorld active) home)]
    #?(:clj
       (if related?
         (f world)
         (let [lock (.readLock ^ReentrantReadWriteLock (.-gate world))]
           (.lock lock)
           (try (f world) (finally (.unlock lock)))))
       :default
       (f world))))

(defn managed-swap!
  "CAS a directly described slot. The logical fallback represents a slot
  allocated in another branch but not yet realized in the selected world."
  [home registry slot f args validate! notify!]
  (call-with-managed-mutation
   home registry
   (fn [^DenseWorld world]
     (ensure-capacity! world (inc slot))
     (loop []
       (let [cells (current-cells world)
             raw-old (cells-get cells slot)
             old (if (identical? absent raw-old)
                   (slot-value home slot absent)
                   raw-old)
             new (apply f old args)
             validation (validate! world new)]
         (if (cells-cas! cells slot raw-old new)
           (do (notify! world validation old new) new)
           (recur)))))))

(defn managed-reset! [home registry slot new validate! notify!]
  (managed-swap! home registry slot (constantly new) nil
                 validate! notify!))

(defn managed-compare-and-set!
  [home registry slot expected new validate! notify!]
  (call-with-managed-mutation
   home registry
   (fn [^DenseWorld world]
     (ensure-capacity! world (inc slot))
     (let [cells (current-cells world)
           raw-old (cells-get cells slot)
           old (if (identical? absent raw-old)
                 (slot-value home slot absent)
                 raw-old)]
       (if-not (identical? expected old)
         false
         (let [validation (validate! world new)]
           (if (cells-cas! cells slot raw-old new)
             (do (notify! world validation old new) true)
             false)))))))

(defn register! [handle value]
  (when-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (allocate-descriptor! world handle :ref true nil)]
      (ensure-capacity! world (inc value-slot))
      (cells-set! (current-cells world) value-slot value)))
  handle)

(defn- allocate-var-watch-slot! [^DenseWorld world handle fallback]
  (let [registry (.-registry world)]
    (with-registry-lock
      registry
      (fn []
        (let [desc (descriptor world handle)]
          (if (:watch-slot desc)
            desc
            (let [slot (:next-slot @registry)
                  desc (assoc desc
                              :watch-slot slot
                              :watch-fallback fallback)]
              (swap! registry
                     (fn [r]
                       (-> r
                           (assoc :next-slot (inc slot))
                           (assoc-in [:descriptors handle] desc))))
              desc)))))))

(defn register-var!
  ([handle value m]
   (register-var! handle value m nil))
  ([handle value m watches]
   (when-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot meta-slot]}
           (allocate-descriptor! world handle :var (not (:sci/built-in m))
                                 (:sci.impl/fork-fn m))]
       (ensure-capacity! world (inc meta-slot))
       (let [cells (current-cells world)]
         (cells-set! cells value-slot value)
         (cells-set! cells meta-slot m))
       (when (seq watches)
         (let [{:keys [watch-slot]}
               (allocate-var-watch-slot! world handle watches)]
           (ensure-capacity! world (inc watch-slot))
           (cells-set! (current-cells world) watch-slot watches)))))
   handle))

(defn register-namespace! [handle m]
  (when-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (allocate-descriptor! world handle :namespace false nil)]
      (ensure-capacity! world (inc value-slot))
      (let [cells (current-cells world)]
        (when (identical? absent (cells-get cells value-slot))
          (cells-set! cells value-slot m)))))
  handle)

(defn- reset-slot! [^DenseWorld world slot value]
  (if (nil? slot)
    ::no-world
    (do
      (ensure-capacity! world (inc slot))
      (cells-set! (current-cells world) slot value))))

(defn reset-value! [handle value]
  (if-let [world (current-world)]
    (reset-slot! world (:value-slot (descriptor world handle)) value)
    ::no-world))

(defn reset-var-meta! [handle m]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [meta-slot]} (or (descriptor world handle)
                                  (allocate-descriptor! world handle :var
                                                        (not (:sci/built-in m))
                                                        (:sci.impl/fork-fn m)))]
      (reset-slot! world meta-slot m))
    ::no-world))

(defn reset-namespace-meta! [handle m]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (or (descriptor world handle)
              (allocate-descriptor! world handle :namespace false nil))]
      (reset-slot! world value-slot m))
    ::no-world))

(defn alter-var-meta! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [meta-slot]} (or (descriptor world handle)
                                  (allocate-descriptor! world handle :var
                                                        (not (:sci/built-in fallback))
                                                        (:sci.impl/fork-fn fallback)))]
      (ensure-capacity! world (inc meta-slot))
      (loop []
        (let [cells (current-cells world)
              raw-old (cells-get cells meta-slot)
              old (if (identical? absent raw-old) fallback raw-old)
              new (apply f old args)]
          (if (cells-cas! cells meta-slot raw-old new) new (recur)))))
    ::no-world))

(defn alter-namespace-meta! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (or (descriptor world handle)
              (allocate-descriptor! world handle :namespace false nil))]
      (ensure-capacity! world (inc value-slot))
      (loop []
        (let [cells (current-cells world)
              raw-old (cells-get cells value-slot)
              old (if (identical? absent raw-old) fallback raw-old)
              new (apply f old args)]
          (if (cells-cas! cells value-slot raw-old new) new (recur)))))
    ::no-world))

(defn alter-var-watches! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [watch-slot]}
          (allocate-var-watch-slot! world handle fallback)]
      (ensure-capacity! world (inc watch-slot))
      (loop []
        (let [cells (current-cells world)
              raw-old (cells-get cells watch-slot)
              old (if (identical? absent raw-old) fallback raw-old)
              new (apply f old args)]
          (if (cells-cas! cells watch-slot raw-old new) new (recur)))))
    ::no-world))

(defn swap-value!
  "Atomically update one tracked slot. Unrelated SCI atoms do not contend and
  cannot cause `f` to be retried."
  [handle f args validate! notify!]
  (let [^DenseWorld world (current-world)
        slot (:value-slot (descriptor world handle))]
    (loop []
      (let [cells (current-cells world)
            old (cells-get cells slot)
            new (apply f old args)]
        (validate! new)
        (if (cells-cas! cells slot old new)
          (do (notify! old new) new)
          (recur))))))

(defn reset-tracked! [handle new validate! notify!]
  (swap-value! handle (constantly new) nil validate! notify!))

(defn compare-and-set-tracked! [handle expected new validate! notify!]
  (let [^DenseWorld world (current-world)
        slot (:value-slot (descriptor world handle))
        cells (current-cells world)
        old (cells-get cells slot)]
    (if-not (identical? expected old)
      false
      (do
        (validate! new)
        (if (cells-cas! cells slot old new)
          (do (notify! old new) true)
          false)))))

(defn active-ctx
  "Use the currently evaluating descendant context for an interpreted closure.
  Unrelated SCI contexts cannot capture one another accidentally."
  [captured]
  (let [active store/*ctx*]
    (if (and active
             (identical? (:sci.impl/lineage captured)
                         (:sci.impl/lineage active)))
      active
      captured)))

(defn- realize-primary! [^DenseWorld world snapshot-value snapshot-var-meta]
  (when-not @(.-persistent? world)
    (doseq [[handle {:keys [kind value-slot meta-slot]}]
            (:descriptors @(.-registry world))]
      (when (present-slot? world value-slot)
        (cells-set! (current-cells world) value-slot (snapshot-value handle))
        (when (= :var kind)
          (cells-set! (current-cells world) meta-slot (snapshot-var-meta handle)))))
    (vreset! (.-persistent? world) true))
  world)

(defn- copy-world [^DenseWorld world fork-value]
  (sweep-managed! world)
  (let [source (current-cells world)
        n (cells-length source)
        target (new-cells n)
        registry (.-registry world)
        registry-state @registry
        descriptors (:descriptors registry-state)
        managed-descriptors (:managed-descriptors registry-state)
        handles (set (keys descriptors))]
    (dotimes [i n]
      (cells-set! target i (cells-get source i)))
    (when fork-value
      (doseq [[_ {:keys [value-slot host-fork? value-fork-fn]}] descriptors
              :when (or host-fork? value-fork-fn)
              :when (< value-slot n)
              :let [v (cells-get target value-slot)]
              :when (not (identical? absent v))]
        (cells-set! target value-slot
                    (cond
                      (contains? handles v) v
                      value-fork-fn (value-fork-fn v)
                      :else (fork-value v))))
      (doseq [{:keys [fork-slots]} managed-descriptors
              slot fork-slots
              :when (< slot n)
              :let [v (cells-get target slot)]
              :when (not (identical? absent v))]
        (cells-set! target slot (fork-value v))))
    (DenseWorld.
     (volatile! target)
     registry
     #?(:clj (ReentrantReadWriteLock.) :default nil)
     (volatile! true))))

(defn fork-world [^DenseWorld world fork-value snapshot-value snapshot-var-meta]
  #?(:clj
     (let [^ReentrantReadWriteLock gate (.-gate world)]
       (when (pos? (.getReadHoldCount gate))
         (throw (IllegalStateException.
                 "Cannot fork a SCI world from inside its active evaluation; suspend it first.")))
       (let [lock (.writeLock gate)]
         (.lock lock)
         (try
           (realize-primary! world snapshot-value snapshot-var-meta)
           (copy-world world fork-value)
           (finally (.unlock lock)))))
     :default
     (do
       (realize-primary! world snapshot-value snapshot-var-meta)
       (copy-world world fork-value))))
