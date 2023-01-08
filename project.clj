(defproject pddl-parse-and-ground "0.1.0-SNAPSHOT"
  :description "A Parser and Grounder for PDDL files"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"] [org.clojure/data.json "2.4.0"] [org.clojure/math.combinatorics "0.1.6"]
]
  :main ^:skip-aot pddl-parse-and-ground.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})