(defproject borkdude/sci
  #=(clojure.string/trim
     #=(slurp "resources/SCI_VERSION"))
  ;; :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
  :description "Small Clojure Interpreter"
  :url "https://github.com/borkdude/sci"
  :scm {:name "git"
        :url "https://github.com/borkdude/sci"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [borkdude/edamame "0.0.11-alpha.7"]
                 [borkdude/sci.impl.reflector "0.0.1"]
                 [org.clojure/tools.reader "1.3.2"]]
  :plugins [[lein-codox "0.10.7"]]
  :profiles {:clojure-1.9.0 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :clojure-1.10.2-alpha1 {:dependencies [[org.clojure/clojure "1.10.2-alpha1"]]}
             :dev {:dependencies [[thheller/shadow-cljs "2.8.64"]]}
             :test {:jvm-opts ["-Djdk.attach.allowAttachSelf"]
                    :dependencies [[clj-commons/conch "0.9.2"]
                                   [criterium "0.4.5"]
                                   [com.clojure-goes-fast/clj-async-profiler "0.4.0"]]}
             :compile-java {:java-source-paths ["src-java"]
                            :source-paths ["src" "src-java"]
                            :aot [sci.impl.java]}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :aot :all}
             :libsci {:dependencies [[cheshire "5.10.0"]]
                      :source-paths ["src" "libsci/src"]}}
  ;; for testing only
  :main sci.impl.main
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])

;; Notes
;; Generate a bundle size report with shadow-cljs:
;; npx shadow-cljs run shadow.cljs.build-report sci report.html
