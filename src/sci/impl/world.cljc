(ns sci.impl.world
  "Fork-local runtime state for SCI contexts.

  A world is a persistent value behind an atom. Stable handles (SCI Vars and
  SCI-created mutable primitives) are keys into that value. Forks copy the
  persistent value into a fresh atom, preserving identity and aliasing while
  isolating subsequent writes."
  (:require [sci.ctx-store :as store]))

(defn new-world []
  (atom {:values {}
         :var-meta {}
         ;; Before the first fork, the primary context behaves like ordinary
         ;; mutable SCI. The first fork realizes a persistent snapshot.
         :persistent? false}))

#?(:clj
   (def ^ThreadLocal active-world
     (proxy [ThreadLocal] []
       (initialValue [] nil)))
   :default
   (def active-world (volatile! nil)))

(defn- active-world-state []
  #?(:clj (.get ^ThreadLocal active-world)
     :default @active-world))

(defn with-active-world
  "Run `f` with the context's read realization installed. Primary contexts use
  direct Var/host-ref fields; descendants use their persistent world value."
  [ctx f]
  (let [previous (active-world-state)
        active (:sci.impl/read-world ctx)]
    #?(:clj (.set ^ThreadLocal active-world active)
       :default (vreset! active-world active))
    (try
      (f)
      (finally
        #?(:clj (if (nil? previous)
                  (.remove ^ThreadLocal active-world)
                  (.set ^ThreadLocal active-world previous))
           :default (vreset! active-world previous))))))

(defn current-world []
  (:sci.impl/world store/*ctx*))

(defn registered? [world handle]
  (contains? (:values @world) handle))

(defn primary-world? []
  (let [active (active-world-state)]
    (if active
      (:primary? active)
      (true? (:sci.impl/primary? store/*ctx*)))))

(defn mutable-primary-world? []
  (let [active (active-world-state)]
    (and (:primary? active)
         (not @(:persistent? active)))))

(defn tracked?
  [handle]
  (when-let [world (:world (active-world-state))]
    (contains? (:values @world) handle)))

(defn value
  [handle fallback]
  (if-let [world (:world (active-world-state))]
    (get-in @world [:values handle] fallback)
    fallback))

(defn var-value
  "Primary contexts keep the Var's ordinary mutable root synchronized, which
  preserves the Clojure/SCI fast path. Descendants resolve through the world."
  [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (:primary? active))
      fallback
      (get-in @(:world active) [:values handle] fallback))))

(defn var-meta
  [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (:primary? active))
      fallback
      (get-in @(:world active) [:var-meta handle] fallback))))

(defn register!
  [handle value]
  (when-let [world (current-world)]
    (swap! world assoc-in [:values handle] value))
  handle)

(defn register-var!
  [handle value m]
  (when-let [world (current-world)]
    (let [state @world]
      (when-not (and (primary-world?)
                     (not (:persistent? state))
                     (contains? (:var-meta state) handle))
        (swap! world (fn [state]
                       (-> state
                           (assoc-in [:values handle] value)
                           (assoc-in [:var-meta handle] m)))))))
  handle)

(defn- mutable-primary-state? [state]
  (and (primary-world?) (not (:persistent? state))))

(defn reset-value!
  [handle value]
  (if-let [world (current-world)]
    (do (let [state @world]
          (when-not (and (mutable-primary-state? state)
                         (contains? (:values state) handle))
            (swap! world assoc-in [:values handle] value)))
        value)
    ::no-world))

(defn reset-var-meta!
  [handle m]
  (if-let [world (current-world)]
    (do (let [state @world]
          (when-not (and (mutable-primary-state? state)
                         (contains? (:var-meta state) handle))
            (swap! world assoc-in [:var-meta handle] m)))
        m)
    ::no-world))

(defn alter-var-meta!
  [handle fallback f args]
  (if-let [world (current-world)]
    (let [state @world]
      (if (mutable-primary-state? state)
        (apply f fallback args)
        (get-in (swap! world update-in [:var-meta handle]
                       (fn [m]
                         (apply f (if (nil? m) fallback m) args)))
                [:var-meta handle])))
    ::no-world))

(defn swap-value!
  "Atomically update a tracked handle. `validate!` runs before the CAS and
  `notify!` once after a successful transition. Returns the new value."
  [handle f args validate! notify!]
  (let [world (current-world)]
    (loop []
      (let [state @world
            old (get-in state [:values handle])
            new (apply f old args)]
        (validate! new)
        (if (compare-and-set! world state (assoc-in state [:values handle] new))
          (do (notify! old new)
              new)
          (recur))))))

(defn reset-tracked!
  [handle new validate! notify!]
  (swap-value! handle (constantly new) nil validate! notify!))

(defn compare-and-set-tracked!
  [handle expected new validate! notify!]
  (let [world (current-world)]
    (loop []
      (let [state @world
            old (get-in state [:values handle])]
        (if-not (identical? expected old)
          false
          (do
            (validate! new)
            (if (compare-and-set! world state (assoc-in state [:values handle] new))
              (do (notify! old new)
                  true)
              (recur))))))))

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

(defn fork-world
  [world fork-value snapshot-value snapshot-var-meta]
  (let [realize (fn []
                  (let [state @world]
                    (if (:persistent? state)
                      state
                      (let [handles (keys (:values state))
                            vars (keys (:var-meta state))
                            realized (-> state
                                         (assoc :persistent? true)
                                         (update :values
                                                 (fn [values]
                                                   (reduce (fn [ret handle]
                                                             (assoc ret handle (snapshot-value handle)))
                                                           values handles)))
                                         (update :var-meta
                                                 (fn [metas]
                                                   (reduce (fn [ret v]
                                                             (assoc ret v (snapshot-var-meta v)))
                                                           metas vars))))]
                        (reset! world realized)
                        realized))))
        state #?(:clj (locking world (realize))
                 :default (realize))]
    ;; Without a host copier, forking is only a new mutable tip over the same
    ;; persistent value. The first write in either branch creates divergence.
    (if-not fork-value
      (atom state)
      (let [handles (set (keys (:values state)))]
        (atom
         (update state :values
                 (fn [values]
                   (reduce-kv
                    (fn [ret handle v]
                      ;; World-relative handles must retain identity. Other
                      ;; values cross the explicit cooperation boundary.
                      (assoc ret handle (if (contains? handles v)
                                          v
                                          (fork-value v))))
                    (empty values)
                    values))))))))
