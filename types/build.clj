(ns build
  (:require [clojure.tools.build.api :as b]
            [insn.core :as insn]))

(set! *warn-on-reflection* false)

(def lib 'org.babashka/sci.impl.types)
(def version "0.0.2")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn interface-data []
  {:name 'sci.impl.types.IReified
   :version 1.8
   :flags #{:public :interface}
   :methods [{:flags #{:public :abstract}, :name :getMethods,    :desc [Object]}
             {:flags #{:public :abstract}, :name :getInterfaces, :desc [Object]}
             {:flags #{:public :abstract}, :name :getProtocols,  :desc [Object]}]})

(defn write-interface [_]
  (insn/write (insn/visit (interface-data)) "target/classes"))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (write-interface nil)
  #_(b/copy-dir {:src-dirs ["src" "resources"]
                 :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
    (merge {:installer :remote
                       :artifact jar-file
                       :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
                    opts))
  opts)
