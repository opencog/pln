;; =============================================================================
;; Crisp deduction rules
;;
;; Inheritance
;;   A
;;   B
;; Inheritance
;;   B
;;   C
;; |-
;; Inheritance
;;   A
;;   C
;;
;; For now only the following rule is implemented
;;
;;       present-deduction-inheritance-rule
;;
;; No precondition exists on the TV, only the mere presence of the
;; Inheritance is enough. Likewise the conclusion requires no formula,
;; it only adds the presence of the conclusion to the atomspace.
;;
;; -----------------------------------------------------------------------------

(use-modules (opencog logger))

;; Generate the corresponding deduction rule given its link-type and
;; the type of each variable (the same for all 3).
(define (gen-present-deduction-rule link-type var-type)
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
      AC)))

(define present-deduction-inheritance-rule
  (gen-present-deduction-rule InheritanceLink (TypeNode "ConceptNode")))

;; Name the rules
(define present-deduction-inheritance-rule-name
  (DefinedSchemaNode "present-deduction-inheritance-rule"))
(DefineLink present-deduction-inheritance-rule-name
  present-deduction-inheritance-rule)
