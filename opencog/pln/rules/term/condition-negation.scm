;; Condition Negation rule, e.g. for Subset
;;
;; Subset <STV>
;;   A <ATV>
;;   B <BTV>
;; |-
;; Subset <TV>
;;   Not A
;;   B
;;
;; where TV is defined as follows
;;
;;   TV.s = (BTV.s - STV.s * ATV.s) / (1 - ATV.s)
;;   TV.c = ATV.c

;; Rule for Subset
;;
;; TODO: replace by generator.
(define subset-condition-negation-rule
  (define A (Variable "$A"))
  (define B (Variable "$B"))
  (define CT (Type "ConceptNode"))
  (Bind
    (VariableSet
      (TypedVariable A CT)
      (TypedVariable B CT))
    (Present
      (Subset A B))
    (ExecutionOutput
      (GroundedSchema "scm: subset-condition-negation")
      (List
        ;; Conclusion
        (Subset (Not A) B)
        ;; Premises
        (Subset A B)
        A
        B))))

;; Formula
(define (subset-condition-negation conclusion . premises)
  (if (= (length premises) 3)
      (let* ((NS conclusion)
             (S (car premises))
             (A (cadr premises))
             (B (caddr premises))
             (Ss (cog-mean S))
             (As (cog-mean A))
             (Ac (cog-confidence A))
             (Bs (cog-mean B))
             (NSs (if (< A-s 1)
                      (/ (- Bs (* Ss As)) (- 1 As))
                      1))
             (NSc (if (< A-s 1) Ac 0))
             (NStv (stv NSs NSc)))
        (cog-merge-hi-conf-tv! NS NStv))))

;; Name
(define subset-condition-negation-rule-name
  (DefinedSchemaNode "subset-condition-negation-rule"))
(DefineLink subset-condition-negation-rule-name subset-condition-negation-rule)
