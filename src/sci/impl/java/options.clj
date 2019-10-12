(ns sci.impl.java.options
  (:require [sci.impl.java.options.namespace]))

(set! *warn-on-reflection* true)

;; Two step process because we need the class in the return types of `addBinding`
;; and `addToNamespace`. See https://stackoverflow.com/questions/29329798/clojure-gen-class-returning-own-class.

(gen-class
 :name borkdude.sci.options.Options
 :constructors {[] []}
 :state "state"
 :init "init"
 :prefix "opts-")

(gen-class
 :name borkdude.sci.options.Options
 :constructors {[] []}
 :state "state"
 :init "init"
 :prefix "opts-"
 :methods [["addBinding" [java.lang.String java.lang.Object] borkdude.sci.options.Options]
           ["addNamespace" [borkdude.sci.options.Namespace] borkdude.sci.options.Options]
           ["deny" [java.lang.String] borkdude.sci.options.Options]
           ["allow" [java.lang.String] borkdude.sci.options.Options]
           ["val" [] java.lang.Object]])

(defn opts-init []
  [[] (atom {})])

(defn opts-addBinding [^borkdude.sci.options.Options this k v]
  (let [state (.state this)]
    (swap! state assoc-in [:bindings (symbol k)] v))
  this)

(defn opts-deny [^borkdude.sci.options.Options this name]
  (let [state (.state this)]
    (swap! state update :deny (fnil conj #{}) (symbol name)))
  this)

(defn opts-allow [^borkdude.sci.options.Options this name]
  (let [state (.state this)]
    (swap! state update :allow (fnil conj #{}) (symbol name)))
  this)

(defn opts-addNamespace [^borkdude.sci.options.Options this ^borkdude.sci.options.Namespace ns]
  (let [state (.state this)
        ns-val (.val ns)
        ns-name (:name ns-val)
        vars (:vars ns-val)]
    (swap! state assoc-in [:namespaces ns-name] vars))
  this)

(defn opts-val [^borkdude.sci.options.Options this]
  @(.state this))
