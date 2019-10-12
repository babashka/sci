(ns sci.impl.java.options.namespace)

(set! *warn-on-reflection* true)

(gen-class
 :name borkdude.sci.options.Namespace
 :constructors {[String] []}
 :state "state"
 :init "init"
 :prefix "opts-ns-")

(gen-class
 :name borkdude.sci.options.Namespace
 :constructors {[String] []}
 :state "state"
 :init "init"
 :prefix "opts-ns-"
 :methods [["addVar" [java.lang.String java.lang.Object] borkdude.sci.options.Namespace]
           ["val" [] java.lang.Object]])

(defn opts-ns-init [name]
  [[] (atom {:name (symbol name)})])

(defn opts-ns-addVar [^borkdude.sci.options.Namespace this k v]
  (let [state (.state this)]
    (swap! state assoc-in [:vars (symbol k)] v)
    this))

(defn opts-ns-val [^borkdude.sci.options.Namespace this]
  @(.state this))
