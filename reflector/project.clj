(defproject borkdude/sci.impl.reflector "0.0.4"
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :description "JVM reflection support for SCI"
  :licence "MIT"
  :java-source-paths ["src"]
  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])
