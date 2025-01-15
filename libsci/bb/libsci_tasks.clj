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

(defn compile-c
  "Compile example libsci C exec program as `libsci/target/from_c[.exe]`.

  Requires `gcc` to be in PATH."
  []
  (println "Compiling C!")
  (p/shell "gcc" "libsci/src/from_c.c"
           "-L" "libsci/target"
           "-I" "libsci/target"
           "-lsci" "-o" "libsci/target/from_c"
))
(defn compile-cpp
  "Compile example libsci C++ exec program as `libsci/target/from_cpp[.exe]`.

  Requires `g++` to be in PATH."
  []
  (println "Compiling C++!")
  (p/shell "g++" "libsci/src/from_cpp.cpp"
           "-L" "libsci/target"
           "-I" "libsci/target"
           "-lsci" "-o" "libsci/target/from_cpp"
           ))

(defn compile-rust
  "Compile example libsci C++ exec program as `libsci/target/from-rust[.exe]`.

  Requires `Rust`'s `cargo` to be in PATH, and LIBCLANG_PATH to point
  to the directory where `LLVM`s libclang is installed."
  []
  (println "Compiling Rust!")
  (let [libsci-path (str (fs/absolutize "libsci/target"))]
    (p/shell {:dir "libsci/from-rust"
              :extra-env {"LIBSCI_PATH" libsci-path}} "cargo build --release"))

  (fs/copy (cond-> "libsci/from-rust/target/release/from-rust" (fs/windows?) (str ".exe"))
           "libsci/target")

  (println "Copied the from-rust binary to libsci/target"))


(defn- library-env-var-update
  "Returns a map with the linker's relevant library path env var
  updated to include LIB-PATH at the end.

  The map contains a single entry where the key is the linker's env
  var and the value is the udpated env value."
  [lib-path]
  (let [os (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os "win") {"PATH" (str (System/getenv "PATH")
                                            ";" lib-path)}
      (str/includes? os "mac") {"DYLD_LIBRARY_PATH"
                                (str (System/getenv "DYLD_LIBRARY_PATH")
                                     ":" lib-path)}
      ;; assume *nix variant
      :else {"LD_LIBRARY_PATH" (str (System/getenv "LD_LIBRARY_PATH")
                                    ":" lib-path)})))

(deftest lang-programs
  (let [lib-path (str (fs/absolutize "libsci/target/"))
        shell-opts {:out :string
                    :extra-env (library-env-var-update lib-path)}]
    (testing "c program"
      (let [{:keys [out]} (p/shell shell-opts "libsci/target/from_c \"(+ 1 2)\"")]
        (is (= (str/trim out) "3"))))

    (testing "c++ program"
      (let [{:keys [out]} (p/shell shell-opts "libsci/target/from_cpp \"(+ 1 2)\"")]
        (is (= (str/trim out) "3"))))

    #_(testing "rust program"
      (let [{:keys [out]} (p/shell shell-opts "libsci/target/from-rust \"(+ 1 2)\"")]
        (is (= (str/trim out) "3"))))))

(defn test
  "Test example compiled libsci programs."
  []
  (let [{:keys [fail error]} (run-all-tests (re-pattern (str (namespace ::ns))))]
    (System/exit (if (pos? (+ fail error)) 1 0))))


