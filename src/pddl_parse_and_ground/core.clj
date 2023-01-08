(ns pddl-parse-and-ground.core
  (:require [clojure.data.json :as json])
  (:require [pddl-parse-and-ground.parser :as parser])
  (:require [pddl-parse-and-ground.grounder :as grounder])
  (:gen-class))

(defn -main [& args]
  (let [parsed (parser/parse-domain-and-problem (first args) (second args))]
  (json/pprint (assoc-in parsed [:PDDLDomain :grounding] (grounder/ground-all-actions [parsed])))))