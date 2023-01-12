(defproject org.clojars.existenzquantor/pddl-parse-and-ground "0.1.3"
  :description "A Parser and Grounder for PDDL files"
  :url "https://github.com/existenzquantor/pddl_parse_and_ground"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"] [org.clojure/data.json "2.4.0"] [org.clojure/math.combinatorics "0.1.6"]]
  :main ^:skip-aot pddl-parse-and-ground.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :sign-releases false}]]
  :min-lein-version "2.0.0")
