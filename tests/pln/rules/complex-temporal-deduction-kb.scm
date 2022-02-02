;; Use temporal deduction (specialized for cognitive schematics) to
;; infer
;;
;; outside(house) ∧ exec(go_to_key) ↝ hold(key)
;; hold(key) ∧ exec(go_to_house) ↝ inside(house)
;; ⊢
;; (outside(house) ∧ exec(go_to_key)) ≺ exec(go_to_house) ↝ inside(house)

;; outside(house) ∧ exec(go_to_key) ↝ hold(key)
(BackPredictiveImplicationScopeLink (stv 1 0.00990099)
  (VariableSet)
  (SLink
    (ZLink))
  (AndLink (stv 0.16 0.0588235)
    (EvaluationLink (stv 0.6 0.0588235)
      (PredicateNode "outside")
      (ListLink
        (ConceptNode "self")
        (ConceptNode "house")))
    (ExecutionLink
      (SchemaNode "go_to_key")))
  (EvaluationLink (stv 0.26 0.0588235)
    (PredicateNode "holds")
    (ListLink
      (ConceptNode "self")
      (ConceptNode "key"))))

;; hold(key) ∧ exec(go_to_house) ↝ inside(house)
(BackPredictiveImplicationScopeLink (stv 1 0.00621118)
  (VariableSet)
  (SLink
    (ZLink))
  (AndLink (stv 0.1 0.0588235)
    (ExecutionLink
      (SchemaNode "go_to_house"))
    (EvaluationLink (stv 0.26 0.0588235)
      (PredicateNode "holds")
      (ListLink
        (ConceptNode "self")
        (ConceptNode "key"))))
  (EvaluationLink (stv 0.28 0.0588235)
    (PredicateNode "inside")
    (ListLink
      (ConceptNode "self")
      (ConceptNode "house"))))
