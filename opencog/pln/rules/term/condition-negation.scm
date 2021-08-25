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
;; Strength:
;;
;;   TV.s = (BTV.s - STV.s * ATV.s) / (1 - ATV.s)
;;
;; which derives from
;;
;; P(B|¬A) = P(B∩¬A) / P(¬A)
;;         = (P(B) - P(B∩A)) / (1 - P(A))
;;         = (P(B) - P(B|A)*P(A)) / (1 - P(A))
;;
;; Confidence:
;;
;;   TV.c = min(count->confidence(confidence->count(ATV.c) * (1 - ATV.s)), STV.c)
;;
;; which is a heuristic deriving from
;;
;; 1) Estimating the positive count of (Not A), which should be the
;;    count of (Subset (Not A) B).
;;
;; 2) Combine with (via min) the count of (Subset A B) in case the
;;    estimation of 1) is too high.

;; Rule for Subset
;;
;; TODO: replace by generator.
(define subset-condition-negation-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (CT (Type "ConceptNode")))
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
          B)))))

;; Formula
(define (subset-condition-negation conclusion . premises)
  (if (= (length premises) 3)
      (let* ((NS conclusion)
             (S (car premises))
             (A (cadr premises))
             (B (caddr premises))
             (Ss (cog-mean S))
             (Sc (cog-confidence S))
             (As (cog-mean A))
             (Ac (cog-confidence A))
             (Bs (cog-mean B))
             (NAs (- 1 As))
             (NSs (if (< As 1)
                      (/ (- Bs (* Ss As)) NAs)
                      1))
             (NSc (if (< As 1)
                      (min (count->confidence (* (confidence->count Ac) NAs)) Sc)
                      0))
             (NStv (stv NSs NSc)))
        (cog-merge-hi-conf-tv! NS NStv))))

;; Name
(define subset-condition-negation-rule-name
  (DefinedSchemaNode "subset-condition-negation-rule"))
(DefineLink subset-condition-negation-rule-name subset-condition-negation-rule)
