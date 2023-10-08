(ns libsci-tasks
  (:require [babashka.deps :as deps]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests testing]]))

(defn- setup-clojure-lein-ps1-cmd
  "Return PowerShell command to invoke `lein.ps1` script installed as part
  of DeLaGuardo's setup-clojure GitHub action on MS-Windows."
  []
  (when-let [lein-home (System/getenv "LEIN_HOME")]
    (let [lein-ps1 (fs/path lein-home "bin" "lein.ps1")]
      (when (fs/exists? lein-ps1)
        (str "powershell -File " lein-ps1)))))

(defn compile-native
  "Compile libsci as native library in `libsci/target/`.
  It requires both leiningen and graalvm being installed.

  It expects to find the graalvm home path in the GRAALVM_HOME env
  var and lein in PATH."
  []
  (let [_   (println :lein (fs/which "lein"))
        graalvm-home (or (System/getenv "GRAALVM_HOME")
                         (throw (Exception. "Please set GRAALVM_HOME.")))
        java-home (str (fs/path graalvm-home "bin"))

        lein  (str (or (fs/which "lein")
                       ;; temporary workaround for
                       ;; https://github.com/DeLaGuardo/setup-clojure/issues/78
                       (setup-clojure-lein-ps1-cmd)
                       (throw (Exception. "Cannot find lein in PATH."))))
        sci-version (str/trim (slurp "resources/SCI_VERSION"))
        sci-jar (str "target/sci-" sci-version "-standalone.jar")
        svm-jar (str (or (first (fs/glob graalvm-home "**/svm.jar"))
                         (do (p/shell (str (fs/file graalvm-home "bin" "gu")) "install" "native-image")
                             (first (fs/glob graalvm-home "**/svm.jar")))
                         (throw (Exception. "Cannot find `svm.jar` in GRAALVM_HOME."))))]
    (prn :graalvm-home graalvm-home :java-home java-home :svm-jar svm-jar :lein lein)
    (p/shell lein "with-profiles" "+libsci,+native-image" "do" "clean," "uberjar")

    (let [javac (str (fs/file graalvm-home "bin" "javac"))]
      (p/shell javac
               "-cp" (str/join fs/path-separator [sci-jar svm-jar])
               "libsci/src/sci/impl/LibSci.java"))

    (let [native-image (str (fs/path graalvm-home "bin"
                                     (if (fs/windows?) "native-image.cmd" "native-image")))]
      (p/shell native-image
               "-jar" sci-jar
               "-cp" "libsci/src"
               "-H:Name=libsci"
               "--shared"   "-H:+ReportExceptionStackTraces"
               "-J-Dclojure.spec.skip-macros=true"
               "-J-Dclojure.compiler.direct-linking=true"
               "-H:IncludeResources=SCI_VERSION"
               "-H:ReflectionConfigurationFiles=reflection.json"
               "--initialize-at-build-time"
               "-H:Log=registerResource:"
               "--verbose"
               "--no-fallback"
               "--no-server"
               "--enable-preview"
               "-J-Xmx3g"))

    (p/shell lein "clean")
    (deps/clojure ["-Spath" "-Sdeps" "{:deps {borkdude/deps.clj {:mvn/version \"0.0.1\"}}}"])

    (fs/delete-tree "libsci/target")
    (fs/create-dirs "libsci/target")

    (let [lib (cond->> (System/mapLibraryName "sci")
                (fs/windows?)
                (str "lib"))]
      (doseq [path ["graal_isolate_dynamic.h" "libsci.h" "graal_isolate.h" lib "libsci_dynamic.h"]]
        (fs/move path "libsci/target"))
      (when (fs/windows?) (fs/move "libsci.lib" "libsci/target/sci.lib")))))

(comment
  run 'bb build-libsci-headers.clj' to generate the following files

  graal_isolate_dynamic.h 
  graal_isolate.h 
  libsci_dynamic.h
  libsci.dylib
  libsci.h)
  
(compile-native)