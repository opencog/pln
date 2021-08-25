;; =============================================================================
;; Full Deduction Rule
;;
;; Like the deduction rule but does not make the following assumptions
;;
;; P(C|B,A) = P(C|B)
;; P(C|¬B,A) = P(C|¬B)
;;
;; This makes the rule a bit more complicated as the required premises
;; are a bit more complicated, but is more accurate when such
;; assumption is not valid.
;;
;; With that in mind let us derive P(C|A) to understand what premises
;; we need. We start with
;;
;; P(C|A) = P(C,A) / P(A)
;;
;; Let's partition P(C,A) according to B
;;
;; P(C|A) = (P(C,B,A) + P(C,¬B,A)) / P(A)
;;
;; which can be turned into
;;
;; P(C|A) = (P(C|B,A)×P(B,A) + P(C|¬B,A)×P(¬B,A)) / P(A)
;; P(C|A) = (P(C|B,A)×P(B|A)×P(A) + P(C|¬B,A)×P(¬B|A)×P(A)) / P(A)
;; P(C|A) = P(C|B,A)×P(B|A) + P(C|¬B,A)×P(¬B|A)
;;
;; Thus we need premises P(B|A), P(¬B|A), P(C|B,A), P(C|¬B,A).  The
;; rule should therefore be
;;
;; <implication> <TV1>
;;   A
;;   B
;; <implication> <TV2>
;;   A
;;   Not B
;; <implication> <TV1>
;;   And
;;     A
;;     B
;;   C
;; <implication> <TV2>
;;   And
;;     A
;;     Not B
;;   C
;; |-
;; <implication> <TV>
;;   A
;;   C
;;
;; -----------------------------------------------------------------------------

(use-modules (opencog logger))

(load "formulas.scm")

;; Generate the corresponding deduction rule given its link-type and
;; the type for each variable (the same for all 3).
(define (gen-full-deduction-rule link-type var-type)
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (C (Variable "$C"))
         (AB (link-type A B))
         (ANB (link-type A (Not B)))
         (ABC (link-type (And A B) C))
         (ANBC (link-type (And A (Not B) C))))
    (Bind
      (VariableList
        (TypedVariable A var-type)
        (TypedVariable B var-type)
        (TypedVariable C var-type))
      (And
        (Present
          AB
          ANB
          ABC
          ANBC)
        (Not (Identical A C)))
      (ExecutionOutput
        (GroundedSchema "scm: full-deduction")
        (List
          ;; Conclusion
          AC
          ;; Premises
          AB
          ANB
          ABC
          ANBC)))))

;; Full deduction formula
(define (full-deduction conclusion . premises)
  (if (= (length premises) 4)
    (let*
        ((AC conclusion)
         (AB (list-ref premises 0))
         (ANB (list-ref premises 1))
         (ABC (list-ref premises 2))
         (ANBC (list-ref premises 3))
         (ABs (cog-mean AB))
         (ABs (cog-confidence AB))
         (ANBs (cog-mean ANB))
         (ANBc (cog-confidence ANB))
         (ABCs (cog-mean ABC))
         (ABCc (cog-confidence ABC))
         (ANBCs (cog-mean ANBC))
         (ANBCc (cog-confidence ANBC))
         (alpha 0.9) ; how much confidence is lost at each deduction step
         (ACs (+ (* ABs ABCs) (* ANBs ANBCs)))
         (ABc (* alpha (min ABc ABCc ANBc ANBCc))))
      (cog-merge-hi-conf-tv! AC (stc ACs ACc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated naming, for backward compatibility only ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: should these names really deprecated?  It's nice the most
;; important part of the name appears first...

(define full-deduction-inheritance-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-full-deduction-rule InheritanceLink var-type)))

(define full-deduction-implication-rule
  (let ((var-type (TypeChoice
                    (TypeNode "PredicateNode")
                    (TypeNode "LambdaLink")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-full-deduction-rule ImplicationLink var-type)))

(define full-deduction-subset-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-full-deduction-rule SubsetLink var-type)))

;; Name the rules
(define full-deduction-inheritance-rule-name
  (DefinedSchemaNode "full-deduction-inheritance-rule"))
(DefineLink full-deduction-inheritance-rule-name
  full-deduction-inheritance-rule)

(define full-deduction-implication-rule-name
  (DefinedSchemaNode "full-deduction-implication-rule"))
(DefineLink full-deduction-implication-rule-name
  full-deduction-implication-rule)

(define full-deduction-subset-rule-name
  (DefinedSchemaNode "full-deduction-subset-rule"))
(DefineLink full-deduction-subset-rule-name
  full-deduction-subset-rule)

;;;;;;;;;;;;;;;;
;; New naming ;;
;;;;;;;;;;;;;;;;

(define inheritance-full-deduction-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-full-deduction-rule InheritanceLink var-type)))

(define implication-full-deduction-rule
  (let ((var-type (TypeChoice
                    (TypeNode "PredicateNode")
                    (TypeNode "LambdaLink")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-full-deduction-rule ImplicationLink var-type)))

(define subset-full-deduction-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-full-deduction-rule SubsetLink var-type)))

;; Name the rules
(define inheritance-full-deduction-rule-name
  (DefinedSchemaNode "inheritance-full-deduction-rule"))
(DefineLink inheritance-full-deduction-rule-name
  inheritance-full-deduction-rule)

(define implication-full-deduction-rule-name
  (DefinedSchemaNode "implication-full-deduction-rule"))
(DefineLink implication-full-deduction-rule-name
  implication-full-deduction-rule)

(define subset-full-deduction-rule-name
  (DefinedSchemaNode "subset-full-deduction-rule"))
(DefineLink subset-full-deduction-rule-name
  subset-full-deduction-rule)
