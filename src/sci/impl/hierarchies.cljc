(ns sci.impl.hierarchies
  {:no-doc true}
  (:require [sci.impl.vars :as vars]
            [sci.lang]))

;;;; Hierarchies

(defn global-hierarchy [ctx]
  (get-in @(:env ctx) [:namespaces 'clojure.core 'global-hierarchy]))

(defn ->tag [x]
  (if (instance? sci.lang.Type x)
    (symbol (namespace x) (name x))
    x))

(defn derive*
  ([ctx tag parent]
   (let [tag (->tag tag)]
     (vars/alter-var-root (global-hierarchy ctx)
                          (fn [h]
                            (derive h tag parent))))
   nil)
  ([_ctx h tag parent]
   (let [tag (->tag tag)]
     (derive h tag parent))))

(defn underive*
  ([ctx tag parent]
   (let [tag (->tag tag)]
     (vars/alter-var-root (global-hierarchy ctx)
                          (fn [h]
                            (underive h tag parent))))
   nil)
  ([_ctx h tag parent]
   (let [tag (->tag tag)]
     (underive h tag parent))))

(defn isa?*
  ([ctx child parent]
   (let [h @(global-hierarchy ctx)]
     (isa? h (->tag child) parent)))
  ([_ctx h child parent]
   (isa? h (->tag child) parent)))

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
