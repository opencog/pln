;; =============================================================================
;; Deduction Rule
;;
;; This deduction rule makes the assumptions
;;
;; P(C|B,A) = P(C|B)
;; P(C|¬B,A) = P(C|¬B)
;;
;; For the full deduction rule that does not make such assumption see
;; the full-deduction.scm file.
;;
;; A <TV1>
;; B <TV2>
;; C <TV3>
;; <implication> <TV4>
;;   A
;;   B
;; <implication> <TV5>
;;   B
;;   C
;; |-
;; <implication> <TV>
;;   A
;;   C
;;
;; where <implication> can be an ImplicationLink, SubsetLink or such.
;;
;; -----------------------------------------------------------------------------

(use-modules (opencog logger))

(load "formulas.scm")

;; Generate the corresponding deduction rule given its link-type and
;; the type for each variable (the same for all 3).
(define (gen-deduction-rule link-type var-type)
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (C (Variable "$C"))
         (AB (link-type A B))
         (BC (link-type B C))
         (AC (link-type A C)))
    (Bind
      (VariableList
        (TypedVariable A var-type)
        (TypedVariable B var-type)
        (TypedVariable C var-type))
      (And
        (Present
          AB
          BC)
        (Not (Identical A C)))
      (ExecutionOutput
        (GroundedSchema "scm: deduction")
        (List
          ;; Conclusion
          AC
          ;; Premises
          A
          B
          C
          AB
          BC)))))

;; Deduction formula
(define (deduction conclusion . premises)
  (if (= (length premises) 5)
    (let*
        ((AC conclusion)
         (A (list-ref premises 0))
         (B (list-ref premises 1))
         (C (list-ref premises 2))
         (AB (list-ref premises 3))
         (BC (list-ref premises 4))
         (sA (cog-mean A))
         (cA (cog-confidence A))
         (sB (cog-mean B))
         (cB (cog-confidence B))
         (sC (cog-mean C))
         (cC (cog-confidence C))
         (sAB (cog-mean AB))
         (cAB (cog-confidence AB))
         (sBC (cog-mean BC))
         (cBC (cog-confidence BC))
         (alpha 0.9) ; how much confidence is lost at each deduction step

         ;; Hacks to overcome the lack of distributional TV. If s=1
         ;; and c=0, then assign s to the mode value satisfying the
         ;; deduction consistency constraint (what a pain, let's use
         ;; 0.25 for now).
         (sA (if (and (< 0.99 sA) (<= cA 0)) 0.25 sA))
         (sB (if (and (< 0.99 sB) (<= cB 0)) 0.25 sB))
         (sC (if (and (< 0.99 sC) (<= cC 0)) 0.25 sC)))
      (if (and
           (or (= 0 cA) (= 0 cB) (= 0 cAB)
               (conditional-probability-consistency sA sB sAB))
           (or (= 0 cB) (= 0 cC) (= 0 cBC)
               (conditional-probability-consistency sB sC sBC)))
          (if (< 0.99 (* sB cB))
              ;; Hack to overcome for the lack of distributional
              ;; TV. This covers the case where B fully confidently
              ;; tends to 1. See formulas.scm Simple Deduction
              ;; Formula comment for more explanations. This
              ;; overlaps with the implication-introduction-rule.
              (let ((sAC sC)
                    (cAC (* alpha cA cC)))
                (if (and (< 1e-8 sAC) (< 1e-8 cAC)) ;; Don't create zero
                                              ;; knowledge. Note that
                                              ;; sAC == 0 is not zero
                                              ;; knowledge but it's
                                              ;; annoying in the
                                              ;; current hacky
                                              ;; situation.
                    (cog-merge-hi-conf-tv! AC (stv sAC cAC))))
              ;; Branch if sB * cB <= 0.99
              (let* ((sAC (if (or (< 0.99 (* sAB cAB)) (< 0.99 (* sBC cBC)))
                              ;; Hack to overcome for the lack of
                              ;; distributional TV. This covers the case
                              ;; where little is known about A and B
                              ;; (i.e. their strength is meaningless), yet
                              ;; we can confidently calculate sAC because
                              ;; sAB and sBC are so high anyway.
                              (* sAB sBC)
                              ;; Otherwise fall back on the naive formula
                              (simple-deduction-strength-formula sA sB sC sAB sBC)))
                     (cAC (min cAB cBC))
                     ;; Unless the 2 implication are fully confident
                     ;; decrease the confidence by some factor. I'm not
                     ;; sure how justify this for now, it's perhaps a
                     ;; bad hack.
                     (cAC (* (if (< cAC 0.99) alpha 1.0) cAC)))
                (if (and (< 1e-8 sAC) (< 1e-8 cAC)) ;; Don't create zero
                                              ;; knowledge. Note that
                                              ;; sAC == 0 is not zero
                                              ;; knowledge but it's
                                              ;; annoying in the
                                              ;; current hacky
                                              ;; situation.
                    (cog-merge-hi-conf-tv! AC (stv sAC cAC)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated naming, for backward compatibility only ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: should these names really deprecated?  It's nice the most
;; important part of the name appears first...

(define deduction-inheritance-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-deduction-rule InheritanceLink var-type)))

(define deduction-implication-rule
  (let ((var-type (TypeChoice
                    (TypeNode "PredicateNode")
                    (TypeNode "LambdaLink")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-deduction-rule ImplicationLink var-type)))

(define deduction-subset-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-deduction-rule SubsetLink var-type)))

;; Name the rules
(define deduction-inheritance-rule-name
  (DefinedSchemaNode "deduction-inheritance-rule"))
(DefineLink deduction-inheritance-rule-name
  deduction-inheritance-rule)

(define deduction-implication-rule-name
  (DefinedSchemaNode "deduction-implication-rule"))
(DefineLink deduction-implication-rule-name
  deduction-implication-rule)

(define deduction-subset-rule-name
  (DefinedSchemaNode "deduction-subset-rule"))
(DefineLink deduction-subset-rule-name
  deduction-subset-rule)

;;;;;;;;;;;;;;;;
;; New naming ;;
;;;;;;;;;;;;;;;;

(define inheritance-deduction-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-deduction-rule InheritanceLink var-type)))

(define implication-deduction-rule
  (let ((var-type (TypeChoice
                    (TypeNode "PredicateNode")
                    (TypeNode "LambdaLink")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-deduction-rule ImplicationLink var-type)))

(define subset-deduction-rule
  (let ((var-type (TypeChoice
                    (TypeNode "ConceptNode")
                    (TypeNode "AndLink")
                    (TypeNode "OrLink")
                    (TypeNode "NotLink"))))
    (gen-deduction-rule SubsetLink var-type)))

;; Name the rules
(define inheritance-deduction-rule-name
  (DefinedSchemaNode "inheritance-deduction-rule"))
(DefineLink inheritance-deduction-rule-name
  inheritance-deduction-rule)

(define implication-deduction-rule-name
  (DefinedSchemaNode "implication-deduction-rule"))
(DefineLink implication-deduction-rule-name
  implication-deduction-rule)

(define subset-deduction-rule-name
  (DefinedSchemaNode "subset-deduction-rule"))
(DefineLink subset-deduction-rule-name
  subset-deduction-rule)
