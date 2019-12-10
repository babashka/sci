(ns sci.impl.classpath
  {:no-doc true}
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.jar JarFile JarFile$JarFileEntry]))

(set! *warn-on-reflection* true)

(defprotocol IResourceResolver
  (getResource [this path]))

(deftype DirectoryResolver [path]
  IResourceResolver
  (getResource [this resource-path]
    (let [f (io/file path resource-path)]
      (when (.exists f)
        (slurp f)))))

(deftype JarFileResolver [path]
  IResourceResolver
  (getResource [this path]))

(defn part->entry [part]
  (if (str/ends-with? part ".jar")
    (JarFileResolver. (JarFile. (io/file part)))
    (DirectoryResolver. (io/file part))))

(deftype Loader [entries]
  IResourceResolver
  (getResource [this resource-path]
    (some #(getResource % resource-path) entries)))

(defn classpath->entries [^String classpath]
  (let [parts (.split classpath (System/getProperty "path.separator"))
        entries (map part->entry parts)]
    (Loader. entries)))

(defn source-for-namespace [loader namespace]
  (let [ns-str (name namespace)
        path (.replace ns-str "." "/")
        paths (map #(str path %) [".clj" ".cljc"])]
    (some #(getResource loader %) paths)))

;;;; Scratch

(comment
  (def loader (classpath->entries ".:file:///Users/borkdude/.m2/repository/cheshire/cheshire/5.9.0/cheshire-5.9.0.jar"))
  (source-for-namespace loader 'src.sci.core)
  (source-for-namespace loader 'cheshire.core)
  )
