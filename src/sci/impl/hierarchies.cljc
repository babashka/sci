(ns sci.impl.hierarchies
  {:no-doc true}
  (:require [sci.ctx-store :as store]
            [sci.impl.vars :as vars]
            [sci.lang]))

;;;; Hierarchies

(defn global-hierarchy []
  (get-in @(:env (store/get-ctx)) [:namespaces 'clojure.core 'global-hierarchy]))

(defn ->tag [x]
  (if (instance? sci.lang.Type x)
    (symbol (namespace x) (name x))
    x))

(defn derive*
  ([tag parent]
   (let [tag (->tag tag)]
     (vars/alter-var-root (global-hierarchy)
                          (fn [h]
                            (derive h tag parent))))
   nil)
  ([h tag parent]
   (let [tag (->tag tag)]
     (derive h tag parent))))

(defn underive*
  ([tag parent]
   (let [tag (->tag tag)]
     (vars/alter-var-root (global-hierarchy)
                          (fn [h]
                            (underive h tag parent))))
   nil)
  ([h tag parent]
   (let [tag (->tag tag)]
     (underive h tag parent))))

(defn isa?*
  ([child parent]
   (let [h @(global-hierarchy)]
     (isa? h (->tag child) parent)))
  ([h child parent]
   (isa? h (->tag child) parent)))

(defn ancestors*
  ([tag]
   (let [h @(global-hierarchy)]
     (ancestors h tag)))
  ([h tag]
   (ancestors h tag)))

(defn descendants*
  ([tag]
   (let [h @(global-hierarchy)]
     (descendants h tag)))
  ([h tag]
   (descendants h tag)))

(defn parents*
  ([tag]
   (let [h @(global-hierarchy)]
     (parents h tag)))
  ([h tag]
   (parents h tag)))
