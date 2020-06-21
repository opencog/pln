;; Conjunction direct introduction rule
;;
;; A
;; B
;; |-
;; And <TV>
;;   A
;;   B
;;
;; where TV is calculated using direct evidence obtained from member
;; or evaluation links.

(load "extensional-utils.scm")

;; Rule
;;
;; TODO: support n-ary
(define conjunction-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (CptT (Type 'Concept)))
    (Bind
      (VariableSet
        (TypedVariable A CptT)
        (TypedVariable B CptT))
      (Present
        A
        B)
      (ExecutionOutput
        (GroundedSchema "scm: conjunction-direct-introduction")
        (List
          ;; Conclusion
          (And A B)
          ;; Premises
          (Set
            A
            B))))))

;; Formula. Note that for now is also produces MemberLink while this
;; should probably go in a separate rule.
(define (conjunction-direct-introduction conclusion . premises)
  (cog-logger-debug "(conjunction-direct-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((operands (car premises))
             (A (gar operands))
             (B (gdr operands))
             ;; Fetch all members of A and B
             (A-mbrs (get-members-of A))
             (B-mbrs (get-members-of B))
             (AB-mbrs (lset-intersection equal? A-mbrs B-mbrs))
             (all-nodes (cog-get-atoms 'Node #f))
             (usize (length all-nodes)) ; TODO: replace this
                                        ; horrible hack
             (tv-s (/ (length AB-mbrs) usize))
             (tv-c (count->confidence usize)))
        ;; Create members over the conjunction
        ;; TODO: use min tvs instead of (stv 1 1)
        (map (lambda (x) (Member (stv 1 1) x conclusion)) AB-mbrs)

        ;; Update conjunction TV
        (if (< 0 tv-s)                  ; Hack
            (cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c))))))

(define conjunction-direct-introduction-rule-name
  (DefinedSchemaNode "conjunction-direct-introduction-rule"))
(DefineLink conjunction-direct-introduction-rule-name
  conjunction-direct-introduction-rule)
