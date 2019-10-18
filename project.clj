(defproject borkdude/sci
  #=(clojure.string/trim
     #=(slurp "resources/SCI_VERSION"))
  :description "Small Clojure Interpreter"
  :url "https://github.com/borkdude/sci"
  :scm {:name "git"
        :url "https://github.com/borkdude/sci"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :java-source-paths ["src-java"]
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [borkdude/edamame "0.0.7"]
                 [org.clojure/tools.reader "1.3.2"]]
  :aot [sci.impl.java]
  :profiles {:clojure-1.9.0 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :clojure-1.10.1 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :dev {:dependencies [[thheller/shadow-cljs "2.8.64"]]}
             :test {:jvm-opts ["-Djdk.attach.allowAttachSelf"]
                    :dependencies [[clj-commons/conch "0.9.2"]
                                   [criterium "0.4.5"]
                                   [com.clojure-goes-fast/clj-async-profiler "0.4.0"]]}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :aot :all}}
  ;; for testing only
  :main sci.impl.main
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])

;; Notes
;; Generate a bundle size report with shadow-cljs:
;; npx shadow-cljs run shadow.cljs.build-report sci report.html
