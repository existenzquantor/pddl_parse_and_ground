(ns pddl-parse-and-ground.core
  (:require [clojure.data.json :as json])
  (:require [pddl-parse-and-ground.parser :as parser])
  (:require [pddl-parse-and-ground.grounder :as grounder])
  (:gen-class))

(defn add-grounded-actions [parsed]
  (assoc-in parsed [:PDDLDomain :grounding :actions] (grounder/ground-all-actions parsed)))

(defn add-relevant-predicates [parsed]
  (assoc-in parsed [:PDDLDomain :grounding :predicates] (grounder/ground-relevant-predicates parsed)))

(defn parse-and-ground [domain problem]
  (-> (parser/parse-domain-and-problem domain problem)
      (add-grounded-actions)
      (add-relevant-predicates)))

(defn -main [& args]
  (json/pprint (parse-and-ground (first args) (second args))))
