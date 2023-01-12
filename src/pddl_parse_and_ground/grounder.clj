(ns pddl-parse-and-ground.grounder
  (:require [clojure.math.combinatorics :as c])
  (:require [clojure.string]))

(defn get-arity [action]
  (count ((action :action) :parameters)))

(defn substitute-parameters [action pairs]
  (let [new-params (map second pairs)]
    (assoc-in action [:action :parameters] new-params)))

(defn substitute-match [pairs param]
  (let [pair (first pairs)]
    (cond
      (empty? pairs) nil
      (= (param :type) 'const) param
      (= ((first pair) :symbol) (param :variable)) (second pair)
      :else (substitute-match (rest pairs) param))))

(defn substitute-all-params [pairs params]
  (map (partial substitute-match pairs) params))

(defn substitute-formula [pairs formula-old]
  (cond
    (= 'and (formula-old :operator)) {:operator 'and :conjuncts (map (partial substitute-formula pairs) (formula-old :conjuncts))}
    (= 'not (formula-old :operator)) {:operator 'not :atom (substitute-formula pairs (formula-old :atom))}
    :else {:operator 'atom :name (formula-old :name) :params (substitute-all-params pairs (formula-old :params))}))


(defn substitute-precondition [action pairs]
  (let [formula-old ((action :action) :precondition)
        formula-new (substitute-formula pairs formula-old)]
    (assoc-in action [:action :precondition] formula-new)))

(defn substitute-effect [action pairs]
  (let [formula-old ((action :action) :effect)
        formula-new (substitute-formula pairs formula-old)]
    (assoc-in action [:action :effect] formula-new)))

(defn substitute [action pairs]
  (if (> (count ((action :action) :parameters)) 0)
    (substitute-effect (substitute-precondition (substitute-parameters action pairs) pairs) pairs)
  action))

(defn valid-pair? [pair]
  (= (get (first pair) :type) (get (second pair) :type)))

(defn action->grounding [action selection]
  (let [pairs (map list ((action :action) :parameters) selection)
        valid? (every? valid-pair? pairs)]
    (if valid? (substitute action pairs) nil)))

(defn inconsistent-set? [all-atoms negated-atoms]
  (if (empty? negated-atoms)
    false
    (let [negated-atom (first negated-atoms)]
      (if (some #(= (negated-atom :atom) %) all-atoms)
        true
        (inconsistent-set? all-atoms (rest negated-atoms))))))
      
(defn inconsistent? [action]
  (if (= 'and (((action :action) :effect) :operator))
    (inconsistent-set? (((action :action) :effect) :conjuncts) (filter #(= 'not (% :operator)) (((action :action) :effect) :conjuncts)))
    false
    ))

(defn ground-action [objects action]
  (let [arity (get-arity action)
        selections (c/selections objects arity)]
    (if (= (count selections) 0)
      (list action)
      (remove inconsistent? (remove nil? (map (partial action->grounding action) selections))))))

(defn ground-actions [objects actions]
  (map (partial ground-action objects) actions))

(defn ground-all-actions [domprob]
  (flatten (ground-actions
            ((domprob :PDDLProblem) :objects)
            ((domprob :PDDLDomain) :actions))))

(defn extract-atoms [formula]
  (cond
    (empty? formula) nil
    (= 'and (formula :operator)) (map extract-atoms (formula :conjuncts))
    (= 'not (formula :operator)) (formula ::atom)
    :else formula))

(defn extract-all-atoms [action]
  (concat (extract-atoms ((action :action) :precondition)) (extract-atoms ((action :action) :effect))))

(defn ground-relevant-predicates [domprob]
  (let [actions (((domprob :PDDLDomain) :grounding) :actions)
        atoms (map extract-all-atoms actions)]
    (set (remove nil? (flatten atoms)))))