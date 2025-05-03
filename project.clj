(defproject org.babashka/sci
  #=(clojure.string/trim
     #=(slurp "resources/SCI_VERSION"))
  ;; :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
  :description "Small Clojure Interpreter"
  :url "https://github.com/babashka/SCI"
  :scm {:name "git"
        :url "https://github.com/babashka/SCI"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [borkdude/sci.impl.reflector "0.0.4"]
                 [borkdude/edamame "1.4.30"]
                 [org.babashka/sci.impl.types "0.0.2"]
                 [borkdude/graal.locking "0.0.2"]]
  :plugins [[lein-codox "0.10.7"]]
  :profiles {:clojure-1.9.0 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :clojure-1.10.3 {:depdencies [[org.clojure/clojure "1.10.3"]]}
             :clojure-1.11.1 {:dependencies [[org.clojure/clojure "1.11.1"]]}
             :native-image {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :dev {:dependencies [[thheller/shadow-cljs "2.8.64"]]}
             :test {:resource-paths ["test-resources"]
                    :jvm-opts ["-Djdk.attach.allowAttachSelf"]
                    :dependencies [[clj-commons/conch "0.9.2"]
                                   [criterium "0.4.5"]
                                   [com.clojure-goes-fast/clj-async-profiler "0.4.0"]]}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :aot [sci.impl.main]
                       :main sci.impl.main}
             :libsci {:dependencies [[cheshire "5.10.0"]]
                      :source-paths ["src" "libsci/src"]
                      :aot [sci.impl.libsci]}}
  ;; for testing only
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])

;; Notes
;; Generate a bundle size report with shadow-cljs:
;; npx shadow-cljs run shadow.cljs.build-report sci report.html
