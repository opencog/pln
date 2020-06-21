;; TODO: maybe this should go to the term folder

;; Member deduction rule
;;
;; Subset <TV1> A B
;; Member <TV2> X A
;; |-
;; Member <TV3> X B

;; Rule
(define member-deduction-rule
  (let* ((X (Variable "$X"))
         (A (Variable "$A"))
         (B (Variable "$B"))
         (CptT (Type 'Concept)))
    (Bind
      (VariableSet
        X
        (TypedVariable A CptT)
        (TypedVariable B CptT))
      (Present
        (Subset A B)
        (Member X A))
      (ExecutionOutput
        (GroundedSchema "scm: member-deduction")
        (List
          ;; Conclusion
          (Member X B)
          ;; Premises
          (Subset A B)
          (Member X A))))))

;; Formula
(define (member-deduction conclusion . premises)
  (cog-logger-debug "(member-deduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 2)
      (let* ((SubAB (car premises))
             (MemXA (cadr premises))
             (tv-s (* (cog-mean SubAB) (cog-mean MemXA)))
             (tv-c (min (cog-confidence SubAB) (cog-confidence MemXA))))
        (cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c)))))

(define member-deduction-rule-name
  (DefinedSchemaNode "member-deduction-rule"))
(DefineLink member-deduction-rule-name
  member-deduction-rule)
