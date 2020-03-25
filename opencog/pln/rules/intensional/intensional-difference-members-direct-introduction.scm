;; Rule for introducing
;;
;; MemberLink
;;   X
;;   IntensionalDifferenceLink
;;     A
;;     B
;;
;; IntensionalDifferenceLink
;;   A
;;   B
;; AttractionLink
;;   X
;;   A
;; AttractionLink
;;   X
;;   B
;; |-
;; MemberLink
;;   X
;;   IntensionalDifferenceLink
;;     A
;;     B

;; Rule
(define intensional-difference-members-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (X (Variable "$X"))
         (CT (Type "ConceptNode")))
    (Bind
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT)
        (TypedVariable X CT))
      (And
        (Present
          (IntensionalDifference A B)
          (Attraction X A)
          (Attraction X B)))
      (ExecutionOutput
        (GroundedSchema "scm: intensional-difference-members-direct-introduction")
        (List
          ;; Conclusion
          (Member X (IntensionalDifference A B))
          ;; Premise
          (IntensionalDifference A B))))))

; Name the rule
(define intensional-difference-members-direct-introduction-rule-name
  (DefinedSchemaNode "intensional-difference-members-direct-introduction-rule"))
(DefineLink intensional-difference-members-direct-introduction-rule-name
  intensional-difference-members-direct-introduction-rule)
