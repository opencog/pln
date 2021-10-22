;; BackPredictiveImplicationScopeLink direct introduction rule
;; (Look back variant of PredictiveImplicationScopeLink)
;;
;; T
;; P
;; Q
;; |-
;; BackPredictiveImplicationScope <TV>
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
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

;; Rule. For now it is fairly specialized, basically
;;
;; P1
;; P2
;; A
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
;;       Execution
;;         A
;;       T
;;     AtTime
;;       Q
;;       T + 1
;; |-
;; BackPredictiveImplicationScope <TV>
;;   X
;;   1
;;   And
;;     Evaluation
;;       P1
;;       X
;;     Evaluation
;;       P2
;;       X
;;     Execution
;;       A
;;   Q
;;
;; Where TV is calculated according to direct evidence, that is AtTime
;; ...
(define back-predictive-implication-scope-direct-introduction-rule
  (let* ((X (Variable "$X"))
     (P1 (Variable "$P1"))
     (P2 (Variable "$P2"))
     (A (Variable "$A"))
     (Q (Variable "$Q"))
     (VarT (Type 'VariableNode))
     (PredT (Type 'PredicateNode))
     (ScmT (Type 'SchemaNode))
     (EvalT (Type 'EvaluationLink)))
  (Bind
    (VariableSet
      (TypedVariable P1 PredT)
      (TypedVariable P2 PredT)
      (TypedVariable A ScmT)
      (TypedVariable Q EvalT))
    (And
      (Present P1 P2 A Q)
      (Not (Equal P1 P2))
      ;; TODO: add Satisfaction to fulfill the existential condition
      )
    (ExecutionOutput
      (GroundedSchema "scm: back-predictive-implication-scope-direct-introduction")
      (List
        ;; Conclusion
        (BackPredictiveImplicationScope
            X
            (TimeNode "1")
            (And
                (Evaluation P1 X)
                (Evaluation P2 X)
                (Execution A))
            Q)
        ;; Premises
        P1
        P2
        A
        Q)))))

;; Formula
(define (back-predictive-implication-scope-direct-introduction conclusion . premises)
  (cog-logger-fine "(back-predictive-implication-scope-direct-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 4)
      (let* ((P1 (list-ref premises 0))
         (P2 (list-ref premises 1))
         (A (list-ref premises 2))
         (Q (list-ref premises 3))
         ;; Fetch direct evidence for antecedent
         (T (Variable "$T"))
         (X (Variable "$X"))
         (TimeNT (Type 'TimeNode))
         (ante-body (And
              (Present
                (AtTime (Evaluation P1 X) T)
                (AtTime (Evaluation P2 X) T)
                (AtTime (Execution A) T))
              (absolutely-true-eval (AtTime (Evaluation P1 X) T))
              (absolutely-true-eval (AtTime (Evaluation P2 X) T))
              (absolutely-true-eval (AtTime (Execution A) T))))
         (ante-vardecl (VariableList (TypedVariable T TimeNT) X))
         (ante-query (Get ante-vardecl ante-body))
         (ante-res (cog-execute! ante-query))
         (ante-res-lst (cog-outgoing-set ante-res))
         (ante-size (length ante-res-lst)))
    (if (< 0 ante-size)
        (let* (;; For each evidence check if the succedent is true at T+1
           (get-time (lambda (p) (cog-outgoing-atom p 0)))
           (plus-1 (lambda (t) (TimeNode (number->string (+ (string->number (cog-name t)) 1)))))
           (QT1 (lambda (p) (cog-link 'AtTimeLink Q (plus-1 (get-time p)))))
           (true? (lambda (x) (and (not (null? x)) (tv->bool (cog-tv x)))))
           (succ-T1? (lambda (p) (true? (QT1 p))))
           (succ-lst (filter succ-T1? ante-res-lst))
           (succ-size (length succ-lst))
           ;; Calculate the TV of the predictive implication scope
           (strength (exact->inexact (/ succ-size ante-size)))
           (confidence (count->confidence ante-size))
           (tv (stv strength confidence)))
          (cog-merge-hi-conf-tv! conclusion tv))))))

;; Declaration
(define back-predictive-implication-scope-direct-introduction-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-direct-introduction-rule"))
(DefineLink back-predictive-implication-scope-direct-introduction-rule-name
  back-predictive-implication-scope-direct-introduction-rule)
