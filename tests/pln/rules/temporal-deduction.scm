 ;; Test temporal deduction
(BackPredictiveImplicationScopeLink (stv 0.345 0.91941)
    (VariableNode "$X")
    (SLink (ZLink))
  (EvaluationLink
    (PredicateNode "P")
    (VariableNode "$X"))
  (EvaluationLink
    (PredicateNode "Q")
    (VariableNode "$X")))

(BackPredictiveImplicationScopeLink (stv 0.5 0.91941)
  (VariableNode "$X")
  (SLink (SLink
    (ZLink)))
  (And
    (EvaluationLink
      (PredicateNode "Q")
      (VariableNode "$X"))
    (ExecutionLink 
    (SchemaNode "A2")))
  (EvaluationLink
    (PredicateNode "R")
    (VariableNode "$X")))

(BackPredictiveImplicationScopeLink (stv 0.345 0.91941)
    (VariableNode "$X")
    (SLink (ZLink))
    (And
      (EvaluationLink
        (PredicateNode "C")
        (VariableNode "$X"))
      (ExecutionLink
        (SchemaNode "A1")))
  (EvaluationLink
    (PredicateNode "Q")
    (VariableNode "$X")))

(define target
  (Quote (BackPredictiveImplicationScopeLink
    (Unquote (Variable "$v"))
    (Unquote (Variable "$l"))
    (Unquote (Variable "$a"))
    (EvaluationLink
        (PredicateNode "R")
        (Unquote (VariableNode "$X"))))))
