;; BackPredictiveImplicationScope conditional conjuntion introduction rule
;;
;; BackPredictiveImplicationScope <TV1>
;;   V
;;   T
;;   P
;;   Q
;; BackPredictiveImplicationScope <TV2>
;;   V
;;   T
;;   P
;;   R
;; |-
;; BackPredictiveImplicationScope <TV>
;;   V
;;   T
;;   P
;;   And 
;;      Q
;;      R
;;
;; where TV is calculated using TV1 and TV2.

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

(define back-predictive-implication-scope-conditional-conjunction-introduction-rule
  (let* ((V (Variable "$V"))
     (T (Variable "$T"))
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     (R (Variable "$R"))
     (NaturalT (TypeInh 'NaturalLink))
     (VardeclT (TypeChoice
        (TypeInh 'VariableNode)
        (Type 'VariableSet)
        (Type 'VariableList)
        (Type 'TypedVariableLink)))
     (PQ (Quote (BackPredictiveImplicationScope (Unquote V) (Unquote T) (Unquote P) (Unquote Q))))
     (PR (Quote (BackPredictiveImplicationScope (Unquote V) (Unquote T) (Unquote P) (Unquote R))))
     (QR (And Q R))
     (PQR (Quote (BackPredictiveImplicationScope (Unquote V) (Unquote T) (Unquote P) (Unquote QR)))))
  (Bind
    (VariableSet
      (TypedVariable V VardeclT)
      (TypedVariable T NaturalT)
      P
      Q
      R)
    (And
      (Present PQ PR)
      (Not (Identical Q R))
      (EvaluationLink
        (GroundedPredicate "scm: check_preconditions")
        (List 
        Q 
        R)
      )
    )
    (ExecutionOutput
      (GroundedSchema "scm: back-predictive-implication-scope-conditional-conjunction-introduction")
      (List
        ;; Conclusion
        PQR
        ;; Premises
        (Set
          PQ
          PR))))))

(define (check_preconditions Q R)
  (define (andlink? atom)
    (equal? (cog-type atom) 'AndLink))

  (if (or (and (andlink? Q) (member R (cog-outgoing-set Q)))
          (and (andlink? R) (member Q (cog-outgoing-set R))))
    (stv 0 1)
    (stv 1 1))
)

;; Formula
(define (back-predictive-implication-scope-conditional-conjunction-introduction conclusion . premises)
  (cog-logger-info "(back-predictive-implication-scope-conditional-conjunction-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((premises (car premises))
        (PQ (gar premises))
        (PR (gdr premises))
        (sPQ (cog-mean PQ))
        (cPQ (cog-confidence PQ))
        (sPR (cog-mean PR))
        (cPR (cog-confidence PR))
        (tv (stv (* sPQ sPR) (min cPQ cPR))))
        (cog-merge-hi-conf-tv! conclusion tv))))

;; Declaration
(define back-predictive-implication-scope-conditional-conjunction-introduction-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-conditional-conjunction-introduction-rule"))
(DefineLink back-predictive-implication-scope-conditional-conjunction-introduction-rule-name
  back-predictive-implication-scope-conditional-conjunction-introduction-rule)
