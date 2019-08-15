(defproject borkdude/sci
  #=(clojure.string/trim
     #=(slurp "resources/SCI_VERSION"))
  :description "Small Clojure Interpreter"
  :url "https://github.com/borkdude/sci"
  :scm {:name "git"
        :url "https://github.com/borkdude/sci"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :profiles {:clojure-1.9.0 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :clojure-1.10.1 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :test {:dependencies [[clj-commons/conch "0.9.2"]]}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :main sci.impl.native
                       :aot :all}}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])
;; force cache
