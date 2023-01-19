(ns pddl-parse-and-ground.core
  (:require [clojure.data.json :as json])
  (:require [pddl-parse-and-ground.parser :as parser])
  (:require [pddl-parse-and-ground.grounder :as grounder])
  (:gen-class))


(defn parse-and-ground [domain problem]
  (let [parsed (parser/parse-domain-and-problem domain problem)
        with-grounded-actions (assoc-in parsed [:PDDLDomain :grounding :actions] (grounder/ground-all-actions parsed))
        with-grounded-predicates (assoc-in with-grounded-actions [:PDDLDomain :grounding :predicates] (grounder/ground-relevant-predicates with-grounded-actions))]
    with-grounded-predicates))


(defn -main [& args]
  (json/pprint (parse-and-ground (first args) (second args))))    
