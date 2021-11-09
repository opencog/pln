;; Calculate the TV of a concept based on direct evidence
;;
;; Member <TV1> X1 A
;; ...
;; Member <TVn> Xn A
;; |-
;; A <TV>
;;
;; where TV is calculated using direct evidence obtained from member. For now we assume that TV1 to TVn are absolutely true.

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
        (Evaluation
          (GroundedPredicate "scm: absolutely-true")
          (Member X A))
        (Evaluation
          (GroundedPredicate "scm: absolutely-true")
          (Member Y A))
        (Not (Identical X Y)))
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
  ;; (cog-logger-debug "(concept-direct-evaluation conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((mbr-lnks (cog-outgoing-set (car premises)))
             (mbr-lnk-A (car mbr-lnks))
             (mbr-lnk-B (cadr mbr-lnks))
             (all-nodes (cog-get-atoms 'Node #f))
             (usize (length all-nodes)) ; TODO: replace this
                                        ; horrible hack
             (vsize (if (not (equal? mbr-lnk-A mbr-lnk-B)) 2 1))
             (tv-s (/ vsize usize))
             (tv-c (count->confidence usize)))
        ;; Update conjunction TV
        (if (< 0 tv-c)
            (cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c))))))

(define concept-direct-evaluation-rule-name
  (DefinedSchemaNode "concept-direct-evaluation-rule"))
(DefineLink concept-direct-evaluation-rule-name
  concept-direct-evaluation-rule)
