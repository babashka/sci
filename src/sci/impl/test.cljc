(ns sci.impl.test
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   [clojure.string :as str]
   #?(:cljs [goog.object :as gobj])
   [sci.impl.destructure :refer [destructure]]
   [sci.impl.evaluator :as eval]
   [sci.impl.faster :as faster]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.load :as load]
   [sci.impl.macros :as macros]
   [sci.impl.records :as records]
   [sci.impl.resolve :as resolve]
   #?(:clj [sci.impl.types :as types :refer [->Node ->constant]])
   #?(:cljs [sci.impl.types :as types :refer [->constant]])
   [sci.impl.utils :as utils :refer
    [ana-macros constant? kw-identical? macro?
     maybe-destructured rethrow-with-location-of-node set-namespace!]]
   [sci.impl.vars :as vars]
   #?(:cljs [cljs.tagged-literals :refer [JSValue]]))
  #?(:clj (:import [sci.impl Reflector]))
  #?(:cljs
     (:require-macros
      [sci.impl.test :refer [Foo]]
      [sci.impl.types :refer [->Node]])))

(defn foo []
  (->Node 1 nil))

(defmacro Foo []
  `(sci.impl.types/->Node 1 nil))
