(ns sci.impl.interpreter
  {:no-doc true}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-1])
  (:require
   [clojure.tools.reader.reader-types :as r]
   [sci.ctx-store :as store]
   [sci.impl.analyzer :as ana]
   [sci.impl.opts :as opts]
   [sci.impl.parser :as p]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(defn eval-form [ctx form]
  (let [eval-file (:clojure.core/eval-file (meta form))]
    (when eval-file
      (vars/push-thread-bindings {utils/current-file eval-file}))
    (try
      (store/with-ctx ctx
        (if (seq? form)
          (if (= 'do (first form))
            (ana/with-top-level-loc true (meta form)
              (loop [exprs (rest form)
                     ret nil]
                (if (seq exprs)
                  (recur
                   (rest exprs)
                   (eval-form ctx (first exprs)))
                  ret)))
            (let [;; take care of invocation array for let
                  upper-sym (gensym)
                  cb (volatile! {upper-sym {0 {:syms {}}}})
                  ctx (assoc ctx
                             :parents [upper-sym 0]
                             :closure-bindings cb)
                  analyzed (ana/analyze ctx form true)
                  binding-array-size (count (get-in @cb [upper-sym 0 :syms]))
                  bindings (object-array binding-array-size)]
              (if (instance? #?(:clj sci.impl.types.EvalForm
                                :cljs sci.impl.types/EvalForm) analyzed)
                (eval-form ctx (types/getVal analyzed))
                (try (types/eval analyzed ctx bindings)
                     (catch #?(:clj Throwable :cljs js/Error) e
                       (utils/rethrow-with-location-of-node ctx bindings e analyzed))))))
          (let [upper-sym (gensym)
                cb (volatile! {upper-sym {0 {:syms {}}}})
                ctx (assoc ctx
                           :parents [upper-sym 0]
                           :closure-bindings cb)
                analyzed (ana/analyze ctx form)
                binding-array-size (count (get-in @cb [upper-sym 0 :syms]))
                bindings (object-array binding-array-size)]
            (try (types/eval analyzed ctx bindings)
                 (catch #?(:clj Throwable :cljs js/Error) e
                   (utils/rethrow-with-location-of-node ctx bindings e analyzed))))))
      (finally
        (when eval-file
          (vars/pop-thread-bindings))))))

(vreset! utils/eval-form-state eval-form)

(defn eval-string*
  ([ctx s]
   (eval-string* ctx s nil))
  ([ctx s opts]
   (vars/with-bindings {utils/current-ns (or (when opts (:ns opts)) @utils/current-ns)}
     (let [reader (r/indexing-push-back-reader (r/string-push-back-reader s))]
       (loop [ret nil]
         (let [expr (p/parse-next ctx reader)]
           (if (utils/kw-identical? p/eof expr)
             (if (when opts (:sci.impl/eval-string+ opts))
               {:val ret
                :ns @utils/current-ns}
               ret)
             (let [ret (eval-form ctx expr)]
               (recur ret)))))))))

(vreset! utils/eval-string* eval-string*)

;;;; Called from public API

(defn eval-string
  ([s] (eval-string s nil))
  ([s opts]
   (let [init-ctx (opts/init opts)
         ret (eval-string* init-ctx s)]
     ret)))

;;;; Scratch

(comment
  (eval-string "((fn f [x] (if (< x 3) (recur (inc x)) x)) 0)")
  )
