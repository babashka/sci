(ns sci.impl.hierarchies
  {:no-doc true}
  (:require [sci.impl.vars :as vars]))

;;;; Hierarchies

(defn global-hierarchy [ctx]
  (get-in @(:env ctx) [:namespaces 'clojure.core 'global-hierarchy]))

(defn derive*
  ([ctx tag parent]
   (vars/alter-var-root (global-hierarchy ctx)
                        (fn [h]
                          (derive h tag parent)))
   nil)
  ([_ctx h tag parent]
   (derive h tag parent)))

(defn underive*
  ([ctx tag parent]
   (vars/alter-var-root (global-hierarchy ctx)
                        (fn [h]
                          (underive h tag parent)))
   nil)
  ([_ctx h tag parent]
   (underive h tag parent)))

(defn isa?*
  ([ctx child parent]
   (let [h @(global-hierarchy ctx)]
     (isa? h child parent)))
  ([_ctx h child parent]
   (isa? h child parent)))

(defn ancestors*
  ([ctx tag]
   (let [h @(global-hierarchy ctx)]
     (ancestors h tag)))
  ([_ctx h tag]
   (ancestors h tag)))

(defn descendants*
  ([ctx tag]
   (let [h @(global-hierarchy ctx)]
     (descendants h tag)))
  ([_ctx h tag]
   (descendants h tag)))

(defn parents*
  ([ctx tag]
   (let [h @(global-hierarchy ctx)]
     (parents h tag)))
  ([_ctx h tag]
   (parents h tag)))
