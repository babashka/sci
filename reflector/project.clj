(defproject borkdude/sci.impl.reflector "0.0.1-jdk11"
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :java-source-paths ["src-java"]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])
