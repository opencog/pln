;; Subset direct introduction rule
;;
;; A
;; B
;; |-
;; Subset <TV>
;;   A
;;   B
;;
;; where TV is calculated using direct evidence obtained from member
;; links (thus is extensional, nor mixed).

;; Rule
(define subset-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (CT (Type "ConceptNode")))
    (Bind
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT))
      (Present
        A
        B)
      (ExecutionOutput
        (GroundedSchema "scm: subset-direct-introduction")
        (List
          ;; Conclusion
          (Subset A B)
          ;; Premises
          A
          B)))))

;; Given a list of members of A and B calculate the TV of
;;
;; SubsetLink
;;   A
;;   B
(define (subset-evidence->tv A-mbrs B-mbrs)
  ;; (cog-logger-debug "(subset-evidence->tv A-mbrs=~a B-mbrs=~a)" A-mbrs B-mbrs)
  (let* ;; TODO consider TVs of the members
       ((A-size (length A-mbrs))
        (AB-mbrs (lset-intersection equal? A-mbrs B-mbrs))
        (AB-size (length AB-mbrs))
        (strength (if (< 0 A-size)
                      (exact->inexact (/ AB-size A-size))
                      1))
        (confidence (if (< 0 A-size)
                        (count->confidence A-size)
                        0)))
    (stv strength confidence)))

;; Formula
(define (subset-direct-introduction conclusion . premises)
  ;; (cog-logger-debug "(subset-direct-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 2)
      (let* ((Ss conclusion)
             (A (car premises))
             (B (cadr premises))
             ;; Fetch all members of A and B
             (A-mbrs (get-members A))
             (B-mbrs (get-members B))
             ;; Calculate the TV based on the members of A and B
             (tv (subset-evidence->tv A-mbrs B-mbrs)))
        (cog-merge-hi-conf-tv! Ss tv))))

(define subset-direct-introduction-rule-name
  (DefinedSchemaNode "subset-direct-introduction-rule"))
(DefineLink subset-direct-introduction-rule-name
  subset-direct-introduction-rule)
