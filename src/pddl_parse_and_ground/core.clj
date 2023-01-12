(ns pddl-parse-and-ground.core
  (:require [clojure.data.json :as json])
  (:require [pddl-parse-and-ground.parser :as parser])
  (:require [pddl-parse-and-ground.grounder :as grounder])
  (:gen-class))



(defn -main [& args]
  (let [parsed (parser/parse-domain-and-problem (first args) (second args)) 
        with-grounded-actions (assoc-in parsed [:PDDLDomain :grounding :actions] (grounder/ground-all-actions [parsed]))
        with-grounded-predicates (assoc-in with-grounded-actions [:PDDLDomain :grounding :predicates] (grounder/ground-relevant-predicates with-grounded-actions))]
(json/pprint with-grounded-predicates)
  ))    
