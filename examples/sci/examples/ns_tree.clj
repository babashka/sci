(ns sci.examples.ns-tree
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sci.core :as sci]))

(defn rewrite-ns
  "Rewrites ns form :require clauses into symbols + :reload only."
  [ns]
  (keep (fn [x]
          (if (seq? x)
            (cond (= :require-macros (first x)) nil ;; ignore
                  (= :require (first x))
                  (let [nss (keep (fn [x]
                                    (cond (seqable? x) (first x)
                                          (symbol? x) x))
                                 (rest x))]
                    (cons :require (interpose :reload nss)))) ;; force reload
            x))
          ns))

(defn ns->files [dir ns]
  (let [extensions ["clj" "cljs" "cljc"]
        path (-> ns munge (str/replace "." java.io.File/separator))
        files (map #(io/file dir (str path "." %)) extensions)]
    (filter #(.exists ^java.io.File %) files)))

(def ^:dynamic *ctx* nil)
(def ^:dynamic *ns-path* nil)
(def debug true)

(defn process-source [file]
  (let [file-reader (io/reader (io/file file))
        source-reader (sci/reader file-reader)]
    (loop []
      (let [next-form (sci/parse-next *ctx* source-reader)]
        (when-not (= ::sci/eof next-form)
          (if (and (seq? next-form)
                   (= 'ns (first  next-form)))
            (let [ns (rewrite-ns next-form)]
              (sci/eval-form *ctx* ns))
            ;; look for more ns forms
            (recur)))))))

(defn -main [dir namespace]
  (let [namespace (symbol namespace)
        results (atom {namespace nil})
        ctx (sci/init {:load-fn (fn [{:keys [:namespace]}]
                                  ;; (prn namespace)
                                  (doseq [f (ns->files dir namespace)]
                                    ;; (prn f)
                                    (binding [*ns-path* (conj *ns-path* namespace)]
                                      (swap! results assoc-in *ns-path* {})
                                      (process-source f)))
                                  ;; always return empty source because we take care of "eval-ing" the file ourselves
                                  {:source ""})
                       :features #{:clj :cljs}})
        init-files (ns->files dir namespace)]
    ;; establish a thread-local bindings to allow set!
    (sci/with-bindings {sci/ns @sci/ns}
      (binding [*ctx* ctx
                *ns-path* [namespace]]
        (run! #(process-source %) init-files)))
    (pprint/pprint @results)))

;; run:
;; rlwrap clojure -A:examples -m sci.examples.ns-tree src sci.core
