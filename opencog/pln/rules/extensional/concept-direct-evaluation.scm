;; Calculate the TV of a concept based on direct evidence
;;
;; Member X1 A
;; ...
;; Member Xn A
;; |-
;; A <TV>
;;
;; where TV is calculated using direct evidence obtained from member.

;; Rule
;;
;; TODO: support n-ary
(define concept-direct-evaluation-rule
  (let* ((X (Variable "$X"))
         (Y (Variable "$Y"))
         (A (Variable "$A"))
         (CptT (Type 'Concept)))
    (Bind
      (VariableSet
        X
        Y
        (TypedVariable A CptT))
      (And
        (Present
          (Member X A)
          (Member Y A)
          A)
        (Not (Equal X Y)))
      (ExecutionOutput
        (GroundedSchema "scm: concept-direct-evaluation")
        (List
          ;; Conclusion
          A
          ;; Premises
          (Set
            (Member X A)
            (Member Y A)))))))

;; Formula
(define (concept-direct-evaluation conclusion . premises)
  (cog-logger-debug "(concept-direct-evaluation conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((mbr-lnks (car premises))
             (all-nodes (cog-get-atoms 'Node #f))
             (usize (length all-nodes)) ; TODO: replace this
                                        ; horrible hack
             (tv-s (/ (cog-arity mbr-lnks) usize))
             (tv-c (count->confidence usize)))
        ;; Update conjunction TV
        (cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c)))))

(define concept-direct-evaluation-rule-name
  (DefinedSchemaNode "concept-direct-evaluation-rule"))
(DefineLink concept-direct-evaluation-rule-name
  concept-direct-evaluation-rule)
