;; Temporal deduction, for now specialized for action planning and
;; PredictiveImplicationScopeLink.
;;
;; PredictiveImplicationScope
;;   V
;;   T1
;;   P
;;   Q
;; PredictiveImplicationScope
;;   V
;;   T2
;;   And
;;     Q
;;     A
;;   R
;; |-
;; PredictiveImplicationScope
;;   V
;;   T2
;;   SequentialAnd
;;     T1
;;     P
;;     A
;;   R
;;
;; Written in a more compact form (ignoring the lags) it is
;;
;; P↝Q
;; (Q∧A)↝R
;; ⊢
;; (P≺A)↝R
;;
;; Typically the first premise P↝Q contains the first part of the
;; plan, such as
;;
;; (C∧A₁)≺A₂≺A₃↝Q
;;
;; meaning, if C, the initial context is true, and A₁ is taken (at the
;; same time C is true), then A₂ is taken followed by A₃, then Q will
;; be true.
;;
;; And the second premise contains the last part of the plan, such as
;;
;; Q∧A₄↝R
;;
;; meaning, if Q, the penultimate context is true, and A₄ is taken at
;; the same time, then R will be true. Which, in this example, allows
;; to infer the full plan
;;
;; (C∧A₁)≺A₂≺A₃≺A₄↝R
;;
;; Obviously these notations should be understood in Atomese terms as
;;
;; ∧: And
;; ≺: SequentialAnd
;; ↝: PredictiveImplication[Scope]
;;
;; It should be noted that such rule is not complete in the case too
;; much uncertainty is present and failure to reach the penultimate
;; context is probable (especially if executing the last action has a
;; detrimental effect on the goal). Such complete rule would look
;; like:
;;
;; P↝Q
;; Q∧A↝R
;; ¬Q∧A↝R
;; ⊢
;; P≺A↝R
;;
;; The regular deduction rule is also incomplete for the same reason,
;; thus for now we will make the same assumption which is, in this
;; context, that R is independent of A if executed while Q is not
;; true.
;;
;; It should be noted that if executing A while Q is not true is too
;; detrimental to acheiving R, then one needs more than a sequential
;; plan. One needs a plan tree, perhaps called behavior tree, -- see
;; https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)
;; and https://wiki.opencog.org/w/Behavior_tree_(2015_Archive) for an
;; early implementational attempt in OpenCog -- as to conditionalize
;; actions based on whether an intermediate state has been reached or
;; not. Using such behavior trees as plans would allow to obtain
;; cognitive schematics with much higher chances of success than with
;; linear plans (unless the second action is "to plan", in which case
;; it would in principle create a recursion exploring all branches
;; akin to how AIXI works).
;;
;; Other implicit premises are the probabilities of P and Q (like for
;; the regular deduction rule).

;; Rule
(define predictive-implication-scope-deduction-rule
  (let* ((V (Vardiable "$vardecl"))
	 (T1 (Variable "$lag-1"))
	 (T2 (Variable "$lag-2"))
	 (P (Variable "$P"))
	 (Q (Variable "$Q"))
	 (R (Variable "$R"))
	 (A (Variable "$A"))
	 (ExecutionT (Type 'ExecutionLink))
	 (NaturalT (Type 'NaturalLink))
	 (VariableT (Type 'VariableNode))
	 (VariableSetT (Type 'VariableSetLink))
	 (VariableListT (Type 'VariableListLink))
	 (TypedVariableT (Type 'TypedVariableLink))
	 (VardeclT (TypeChoice
		     VariableT
		     VariableSetT
		     VariableListT
		     TypedVariableT))
	 ;; Rule vardecl
	 (vardecl (VariableSet
		    (TypedVariable V VardeclT)
		    (TypedVariable T1 NaturalT)
		    (TypedVariable T2 NaturalT)
		    (TypedVariable A ExecutionT)
		    P
		    Q
		    R))
	 ;; Rule clauses
	 (PQ (PredictiveImplicationScope V T1 P Q))
	 (QA (And Q A))
	 (QAR (PredictiveImplicationScope V T2 QA R))
	 (present-clauses (Present PQ QAR))
	 (precondition-clauses (IsClosed PQ QAR))
	 ;; Rule rewriting term
	 (PA (AltSequentialAnd T1 P A))
	 (PAR (PredictiveImplicationScope V T2 PA R)))
    (Bind
      vardecl
      (And
        present-clauses
	precondition-clauses)
      (OutputExecution
        (GroundedSchema "scm: predictive-implication-scope-deduction")
	(List
	  ;; Conclusion
	  PAR
	  ;; Premises
	  PQ
	  QAR)))))

;; Formula
(define (predictive-implication-scope-deduction conclusion . premises)
  (ure-logger-fine "(predictive-implication-scope-deduction conclusion=~a . premises=~a)" conclusion premises)
  ;; NEXT
)
