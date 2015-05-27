(ns rule-engine.core
  (:require [clojure.set :refer [union]]))

;; Input example
(def rules '[{:patterns [(mother ?x ?y)]
              :assertions [(parent ?x ?y)]}

             {:patterns [(father ?x ?y)]
              :assertions [(parent ?x ?y)]}

             {:patterns [(father ?x ?y)
                         (parent ?y ?z)]
              :assertions [(grandfather ?x ?z)]}])

(def facts '[(mother jane paul)
             (father dan paul)
             (father michel jane)])

(def family-ties {:rules rules
                  :facts facts})



(defn variable? [x]
  "Is the given item a variable"
  (= \? (.charAt (str x) 0)))

(defn match-item [p f b]
  "Match a pattern item with a fact item, given the
  current bindings b. If they match return b enriched
  with a possible bound variable, etherwise return nil"
  (let [sym (if (contains? b p) (get b p) p)]
    (cond
      (variable? sym) (assoc b sym f)
      (= sym f) b
      :else nil)))

(match-item '?x 'hello {})                                  ;;{'?x 'hello}
(match-item '?x 'hello {'?x 'world})                        ;;nil
(match-item '?x 'hello {'?x 'hello})                        ;;{'?x 'hello}
(match-item '?x 'hello {'?y 'world})                        ;;{'?x 'hello '?y 'world}

(defn match [pat fact b]
  "Match a pattern against a fact, given the specified
  bindings. Returns the bindings enriched with new bound
  variables if the match succeedes, and nil otherwise"
  (if (not= (count pat) (count fact))
    nil
    (loop [pairs (map vector pat fact), b b]
      (if-let [[pat-item fact-item] (first pairs)]
        (when-let [new-b (match-item pat-item fact-item b)]
          (recur (rest pairs) new-b))
        b))))

(match '(father ?x b) '(father a b) {})                     ;;{'?x 'a}
(match '(father ?x b) '(father a b) {'?x 'c})               ;;nil
(match '(father ?x ?y) '(father a b) {'?y 'c})              ;;nil
(match '(father ?x ?y) '(father a b) {'?y 'b})              ;;{'?x 'a, '?y 'b}

(defn eval-item [item binds]
  (cond
    (and (variable? item) (contains? binds item)) (get binds item)
    (variable? item) (throw (Exception. (str "Unbound var: " item)))
    :else item))

(defn eval-pattern [pat binds]
  "Eval a pattern given a map of variables"
  (for [item pat]
    (eval-item item binds)))

(eval-pattern '(hello ?a) {'?a 'world})                     ;;'(hello world)

(declare solve)


(defmulti resolve-pattern
  "Multi-method to handle various types of patterns. It dispatches
  on the first symbol in the pattern"
  (fn [pat _ _] (first pat)))

(defmethod resolve-pattern 'if [pat _ b]
  (let [expr (second pat)
        simple-expr (eval-pattern expr b)
        truth (eval simple-expr)]
    (if truth (list b) '())))

(defmethod resolve-pattern 'not [pat db b]
  (let [negated-patterns (rest pat)
        solutions (solve negated-patterns db b)]
    (if (empty? solutions)
      (list b)
      '())))

(defmethod resolve-pattern :default [pat db b]
  (keep #(match pat % b) db))

(defn solve [patterns db b]
  "Takes a sequence of patterns, a db and a variable binding
  and tries to match each pattern sequentially, adding new
  variables in the binding as it iterates. It returns
  a sequence of enriched bindings corresponding to the valid
  matches"
  (if (empty? patterns)
    (list b)
    (let [pat (first patterns)
          bs (resolve-pattern pat db b)]
      (mapcat #(solve (rest patterns) db %) bs))))

(defn unifications [rule db]
  "Given a rule and a db of facts, return all possible bindings
  for the variables in the rule, that make the rule patterns
  match the facts of the db"
  (solve (:patterns rule) db {}))

(unifications (first rules) (set facts))

(def selection-strategy
  "Function which takes a range of possible applicable rules
  and returns one of them"
  rand-nth)

(defn select-rule [rules db]
  "If there are applicable rules with different variable bindings,
  select one of those rules with one binding. If no rules are applicable,
  return nil"
  (let [possibilities (for [r rules
                            b (unifications r db)]
                        [r b])]
    (selection-strategy possibilities)))


(defn apply-rule [rule binds db]
  "Apply a rule on a db, given a set of variable bindings
  for that rule"
  (let [new-facts (for [p (:assertions rule)]
                    (eval-pattern p binds))]
    (union new-facts db)))

(defn step [db rules]
  "Run a single step in the rule engine algorithm. Pick
  an applicable rule and run it on the database. If no rules
  are applicable, return nil"
  (when-let [[rule binds] (select-rule rules db)]
    (apply-rule rule binds db)))

(defn states [domain]
  "Takes a domain and returns the lazy stream
  of states that occur when running iteratively
  rules in the domain"
  (let [initial-db (set (:facts domain))]
    (iterate #(step % (:rules domain)) initial-db)))

(defn simulate [max-steps domain]
  "Iterate through the domain until no more
  rules are applicable or max-steps was reached"
  (->> (states domain)
       (take-while (complement nil?))
       (take max-steps)
       last))

(simulate 1000 family-ties)

;; Evaluates to:
;; #{(mother jane paul)
;;   (parent michel jane)
;;   (father michel jane)
;;   (grandfather michel paul)
;;   (parent jane paul)
;;   (parent dan paul)
;;   (father dan paul)}


(def max-problem
  ; Modelling the problem of finding the maximum from a collection of numbers
  ; The one rule is basically saying: if there is a number x, and there
  ; is no other number y such that y > x, than x must be the maximum
  '{:rules [{:patterns [(number ?x)
                        (not (number ?y)
                             (if (> ?y ?x)))]
             :assertions [(max ?x)]}]

    :facts [(number 1)
            (number 7)
            (number 2)
            (number 0)
            (number 3)
            (number 5)
            (number 8)
            (number 9)
            (number 6)
            (number 4)]})

(simulate 100 max-problem)

;; Evaluates to
;; #{(number 5)
;;   (number 6)
;;   (number 9)
;;   (number 2)
;;   (number 4)
;;   (number 0)
;;   (max 9)
;;   (number 8)
;;   (number 1)
;;   (number 3)
;;   (number 7)}


