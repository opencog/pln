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

;; Formula
(define (intensional-difference-members-direct-introduction conclusion premise)
  ;; Given two concepts A and B, return the attraction link
  ;;
  ;; Attraction <TV>
  ;;   A
  ;;   B
  (define (get-attraction A B)
    (cog-link 'AttractionLink A B))

  ;; Given an attraction link, return the strength which is
  ;; the product of the mean and the confidence of the TV on
  ;; on the attraction link
  ;;
  ;; Attraction <TV>
  ;;   A
  ;;   B
  (define (get-pattern-strength at)
      (if (null? at) 0 (* (cog-mean at) (cog-confidence at))))

  ;; Given the attraction links of A and B, calculate the
  ;; strength of the TV expressed as
  ;;
  ;; min(attraction(X,A), 1 - attraction(X,B))
  ;;
  ;; where the attraction(X,A) is the strength of the TV of
  ;;
  ;; Attraction <TV>
  ;;   X
  ;;   A
  (define (get-strength A-at B-at)
    (min (cog-mean A-at) (- 1 (get-pattern-strength B-at))))

  ;; Given the attraction links of A and B, calculate the
  ;; confidence of the TV expressed as
  ;;
  ;; min(attraction(X,A), attraction(X,B))
  ;;
  ;; where the attraction(X,A) is the confidence of the TV of
  ;;
  ;; Attraction <TV>
  ;;   X
  ;;   A
  (define (get-confidence A-at B-at)
    (min (cog-confidence A-at) (cog-confidence B-at)))

  (let* ((MembLink conclusion)
         (A (gar premise))
         (B (gdr premise))
         (memb (gar MembLink))
         (A-at (get-attraction memb A))
         (B-at (get-attraction memb B))
         (TVs (get-strength A-at B-at))
         (TVc (get-confidence A-at B-at))
         (TV (stv TVs TVc)))
    (if (< 0 TVc) (cog-merge-hi-conf-tv! MembLink TV))))

; Name the rule
(define intensional-difference-members-direct-introduction-rule-name
  (DefinedSchemaNode "intensional-difference-members-direct-introduction-rule"))
(DefineLink intensional-difference-members-direct-introduction-rule-name
  intensional-difference-members-direct-introduction-rule)
