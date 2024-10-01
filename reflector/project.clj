(defproject borkdude/sci.impl.reflector "0.0.2"
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :description "JVM reflection support for SCI"
  :licence "MIT"
  :java-source-paths ["src"]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])
