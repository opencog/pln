; Temporal conditional conjunction 
(BackPredictiveImplicationScopeLink (stv 1 0.0255786)
  (VariableSet
  ) 
  (SLink
    (ZLink
    ) 
  ) 
  (AndLink
    (EvaluationLink
      (PredicateNode "Pellet Position") 
      (ConceptNode "Right Square") 
    ) 
    (EvaluationLink
      (PredicateNode "Agent Position") 
      (ConceptNode "Left Square") 
    ) 
    (ExecutionLink
      (SchemaNode "Go Right") 
    ) 
  ) 
  (EvaluationLink
    (PredicateNode "Pellet Position") 
    (ConceptNode "Right Square") 
  ) 
)
(BackPredictiveImplicationScopeLink (stv 1 0.0255786)
  (VariableSet
  ) 
  (SLink
    (ZLink
    ) 
  ) 
  (AndLink
    (EvaluationLink
      (PredicateNode "Pellet Position") 
      (ConceptNode "Right Square") 
    ) 
    (EvaluationLink
      (PredicateNode "Agent Position") 
      (ConceptNode "Left Square") 
    ) 
    (ExecutionLink
      (SchemaNode "Go Right") 
    ) 
  ) 
  (EvaluationLink
    (PredicateNode "Agent Position") 
    (ConceptNode "Right Square") 
  ) 
) 
(BackPredictiveImplicationScopeLink (stv 1 0.0135635)
  (VariableSet
  ) 
  (SLink
    (SLink
      (ZLink)
    ) 
  ) 
  (AndLink
    (EvaluationLink
      (PredicateNode "Pellet Position") 
      (ConceptNode "Right Square") 
    ) 
    (ExecutionLink
      (SchemaNode "Eat") 
    ) 
    (EvaluationLink
      (PredicateNode "Agent Position") 
      (ConceptNode "Right Square") 
    ) 
  ) 
  (EvaluationLink
    (PredicateNode "Reward") 
    (NumberNode "1") 
  ) 
)

(define target
  (Quote (BackPredictiveImplicationScopeLink
    (Unquote (Variable "$v"))
    (Unquote (Variable "$l"))
    (Unquote (Variable "$a"))
    (Unquote (Variable "$b")))))
