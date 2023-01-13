(ns pddl-parse-and-ground.parser)

; Parse Domain Name
(defn get-domain-name [domain]
  (if (empty? domain)
    nil
    (if (and (list? (first domain)) (or (= 'domain (first (first domain))) (= ':domain (first (first domain)))))
      (second (first domain))
      (get-domain-name (rest domain)))))

(defn in? [coll elm]
  (some #(= elm %) coll))

(defn symbols->map [vars return] 
  (if (empty? vars)
    return
    (if (in? vars '-)
      (let [var (first vars)
            type (first (rest (rest vars)))]
        (symbols->map (rest (rest (rest vars))) (concat return (list {:symbol var :sort 'var :type type}))))
      (let [var (first vars)]
        (if (and (symbol? var) (not (= '\? (get (str var) 0))))
          (symbols->map (rest vars) (conj return {:symbol var :sort 'const :type nil}))
        (symbols->map (rest vars) (concat return (list {:symbol var :sort 'var :type nil}))))))))

; Parse Predicates
(defn make-predicate [pred]
  {:operator 'atom :name (first pred) :params (symbols->map (rest pred) '())})

(defn get-domain-predicates [domain]
  (if (empty? domain)
    nil
    (if (and (list? (first domain)) (= ':predicates (first (first domain))))
      (map make-predicate (rest (first domain)))
      (get-domain-predicates (rest domain)))))

; Parse Formula
(defn make-formula [form]
  (cond
    (= '() form) {}
    (= 'and (first form)) {:operator 'and :conjuncts (map make-formula (rest form))}
    (= 'not (first form)) {:operator 'not :atom (make-predicate (second form))}
    :else (make-predicate form)))

; Parse Precondition
(defn get-action-precondition [precs]
  (make-formula precs))

; Parse Effects
(defn get_action_effect [effs]
  (make-formula effs))

; Parse Actions
(defn get-domain-actions [domain action_list]
  (if (empty? domain)
    action_list
    (if (and (list? (first domain)) (= ':action (first (first domain))))
      (get-domain-actions (rest domain) (conj action_list {:action
                                                           {:name (second (first domain))
                                                            :parameters (symbols->map (nth (first domain) 3) '())
                                                            :precondition (get-action-precondition (nth (first domain) 5))
                                                            :effect (get_action_effect (nth (first domain) 7))}}))
      (get-domain-actions (rest domain) action_list))))

; Parse Domain
(defn get-domain-as-map [domain]
  {:name (get-domain-name domain)
   :predicates (get-domain-predicates domain)
   :actions (get-domain-actions domain '())})

(defn domain [path] (load-string (str "'" (slurp path))))

;; Problem
(defn problem [path] (load-string (str "'" (slurp path))))

; Parse Problem Name
(defn get-problem-name [problem]
  (if (empty? problem)
    nil
    (if (and (list? (first problem)) (or (= 'problem (first (first problem))) (= ':problem (first (first problem)))))
      (second (first problem))
      (get-problem-name (rest problem)))))

(defn lookup-type [object input]
  (if (empty? input)
    nil
    (if (= '- (first input))
      (first (rest input))
      (lookup-type object (rest input)))))

(defn get-problem-objects-recur [objects return]
  (if (empty? objects)
    return
    (if (= '- (first objects))
      (get-problem-objects-recur (rest (rest objects)) return)
      (get-problem-objects-recur (rest objects) (conj return {:symbol (first objects) :sort 'const :type (lookup-type (first objects) objects)})))))

(defn get-problem-objects [problem]
  (if (empty? problem)
    nil
    (if (and (list? (first problem)) (= ':objects (first (first problem))))
      (get-problem-objects-recur (rest (first problem)) '())
      (get-problem-objects (rest problem)))))

(defn get-problem-initial-state [problem]
  (if (empty? problem)
    nil
    (if (and (list? (first problem)) (= ':init (first (first problem))))
      (map make-predicate (rest (first problem)))
      (get-problem-initial-state (rest problem)))))

(defn make-goal [goal]
  (make-formula goal))

(defn get-problem-goal [problem]
  (if (empty? problem)
    nil
    (if (and (list? (first problem)) (= ':goal (first (first problem))))
      (make-goal (second (first problem)))
      (get-problem-goal (rest problem)))))

(defn get-problem-as-map [problem]
  {:name (get-problem-name problem)
   :domain (get-domain-name problem)
   :objects (get-problem-objects problem)
   :init (get-problem-initial-state problem)
   :goal (get-problem-goal problem)})

(defn set-param-type [objects param]
  (let [object (first objects)]
    (if (= (get-in object [:symbol]) (get-in param [:symbol]))
      (assoc-in param [:type] (get-in object [:type]))
      (set-param-type (rest objects) param))))

(defn set-type-in-list [objects atom]
  (assoc-in atom [:params] (map (partial set-param-type objects) (get-in atom [:params]))))

(defn set-type-in-formula [objects formula]
  (cond
    (= 'and (get-in formula [:operator])) (assoc-in formula [:conjuncts] (map (partial set-type-in-formula objects) (get-in formula [:conjuncts])))
    (= 'not (get-in formula [:operator])) (assoc-in formula [:atom] (set-type-in-formula objects (get-in formula [:atom])))
    (map? formula) (assoc-in formula [:params] (map (partial set-param-type objects) (get-in formula [:params])))
    :else (assoc-in formula [:params] (set-type-in-list objects (get-in formula [:params])))))

(defn infer-types-in-actions [objects action]
  (let [objects2 (concat objects (get-in action [:action :parameters]))
        act1 (assoc-in action [:action :precondition] (set-type-in-formula objects2 (get-in action [:action :precondition])))
        act2 (assoc-in act1 [:action :effect] (set-type-in-formula objects2 (get-in act1 [:action :effect])))]
    act2))

(defn infer-types [domprob]
  (let [objects (get-in domprob [:PDDLProblem :objects])
        problem (get-in domprob [:PDDLProblem])
        domain (get-in domprob [:PDDLDomain])]
    (println "(get-in problem [:init])): " (get-in problem [:init]))
    {:PDDLDomain {:name (get-in domain [:name])
                  :predicates (get-in domain [:predicates])
                  :actions (map (partial infer-types-in-actions objects) (get-in domain [:actions]))
                  :grounding nil}
     :PDDLProblem {:name (get-in problem [:name])
                   :domain (get-in problem [:domain])
                   :objects (get-in problem [:objects])
                   :init (flatten (map (partial set-type-in-list objects) (get-in problem [:init])))
                   :goal (set-type-in-formula objects (get-in problem [:goal]))}}))
    

(defn parse-domain-and-problem [dom prob]
  (let [domprob {:PDDLDomain (get-domain-as-map (domain dom))
                 :PDDLProblem (get-problem-as-map (problem prob))}]
    (infer-types domprob)))