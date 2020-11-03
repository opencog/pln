;; PredictiveImplicationScopeLink direct introduction rule
;;
;; T
;; P
;; Q
;; |-
;; PredictiveImplicationScope <TV>
;;   V
;;   T
;;   P
;;   Q
;;
;; where TV is calculated using direct evidence obtained from
;; timestamped instances of P and Q (thus extensional, not mixed).

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog pln))

;; Rule. For now it is fairly specialized, basically
;;
;; P1
;; P2
;; Q
;; There exists X and T such that
;;   Present
;;     AtTime
;;       Evaluation
;;         P1
;;         X
;;       T
;;     AtTime
;;       Evaluation
;;         P2
;;         X
;;       T
;;     AtTime
;;       Q
;;       T + 1
;; |-
;; PredictiveImplicationScope <TV>
;;   X
;;   1
;;   And
;;     Evaluation
;;       P1
;;       X
;;     Evaluation
;;       P2
;;       X
;;   Q
;;
;; Where TV is calculated according to direct evidence, that is AtTime
;; ...
(define predictive-implication-scope-direct-introduction-rule
  (let* ((X (Variable "$X"))
	 (P1 (Variable "$P1"))
	 (P2 (Variable "$P2"))
	 (Q (Variable "$Q"))
	 (VarT (Type 'Variable))
	 (PredT (Type 'Predicate)))
  (Bind
    (VariableSet
      (TypedVariable P1 PredT)
      (TypedVariable P2 PredT)
      Q)
    (And
      (Present P1 P2 Q)
      (Not (Equal P1 P2))
      ;; TODO: add Satisfaction to fulfill the existential condition
      )
    (ExecutionOutput
      (GroundedSchema "scm: predictive-implication-scope-direct-introduction")
      (List
        ;; Conclusion
        (PredictiveImplicationScope
	  X
	  (TimeNode "1")
	  (And
	    (Evaluation P1 X)
	    (Evaluation P2 X))
	  Q)
	;; Premises
	P1
	P2
	Q)))))

;; Formula
(define (predictive-implication-scope-direct-introduction conclusion . premises)
  ;; TODO
  (cog-merge-hi-conf-tv! conclusion (stv 1 0.99)))

;; Declaration
(define predictive-implication-scope-direct-introduction-rule-name
  (DefinedSchemaNode "predictive-implication-scope-direct-introduction-rule"))
(DefineLink predictive-implication-scope-direct-introduction-rule-name
  predictive-implication-scope-direct-introduction-rule)
