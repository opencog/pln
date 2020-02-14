;; =============================================================================
;; Attraction introduction rule, e.g. for Subset
;;
;; Subset <STV>
;;   A
;;   B
;; Subset <SNTV>
;;   Not
;;     A
;;   B
;; |-
;; Attraction <TV>
;;   A
;;   B
;;
;; where TV is defined as follows
;;
;; TV.s = pattern-of(B,A)
;; TV.c = min(STV.c, SNTV.c)
;;
;; pattern-of(B,A) = (P(B|A)-P(B|¬A))+
;;
;; where s(B) is the prior of B and x+ is the positive part of x. For
;; now the prior of B is 1.

;; Rule for Subset (TODO: replace by generator)
(define subset-attraction-rule
  (define A (Variable "$A"))
  (define B (Variable "$B"))
  (define CT (Type "ConceptNode"))
  (BindLink
    (VariableSet
      (TypedVariable A CT)
      (TypedVariable B CT))
    (Present
      (Subset A B)
      (Subset (Not A) B))
    (ExecutionOutputLink
      (GroundedSchemaNode "scm: attraction")
      (ListLink
        ;; Conclusion
        (Attraction A B)
        ;; Premises
        (Subset A B)
        (Subset (Not A) B)))))

;; Formula
(define (attraction conclusion . premises)
  (if (= (length premises) 2)
      (let* ((ATT conclusion)
             (SAB (car premises))
             (SNAB (cadr premises))
             (ATTs (max 0 (- (cog-mean SAB) (cog-mean SNAB))))
             (ATTc (min (cog-confidence SAB) (cog-confidence SNAB)))
             (ATTtv (stv ATTs ATTc)))
        (cog-merge-hi-conf-tv! ATT ATTtv))))

; Name the rule
(define subset-attraction-rule-name (DefinedSchemaNode "subset-attraction-rule"))
(DefineLink subset-attraction-rule-name subset-attraction-rule)
