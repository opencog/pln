;; Inheritance direct introduction rule
;;
;; A
;; B
;; |-
;; Inheritance <TV>
;;   A
;;   B
;;
;; where TV is calculated using direct evidence obtained from member
;; links.

;; Rule
(define inheritance-direct-introduction-rule
  (Bind
    (VariableSet
      (TypedVariable
        (Variable "$A")
        (Type "ConceptNode"))
      (TypedVariable
        (Variable "$B")
        (Type "ConceptNode")))
    (Present
      (Variable "$A")
      (Variable "$B"))
    (ExecutionOutput
      (GroundedSchema "scm: inheritance-direct-introduction")
      (List
        ;; Conclusion
        (Inheritance
          (Variable "$A")
          (Variable "$B"))
        ;; Premises
        (Variable "$A")
        (Variable "$B")))))

(define (variable? X)
  (or (equal? (cog-type X) 'VariableNode)
      (equal? (cog-type X) 'GlobNode)))

;; Helpers
(define (get-members C)
"
  Given a concept node C, return all its members (except variables)
"
  (let* ((member-links (cog-filter 'MemberLink (cog-incoming-set C)))
         (member-of-C? (lambda (x) (equal? C (gdr x))))
         (members (map gar (filter member-of-C? member-links))))
    (filter (lambda (x) (not (variable? x))) members)))

;; Given a list of members of A and B calculate the TV of
;;
;; Inheritance
;;   A
;;   B
(define (inheritance-evidence->tv A-mbrs B-mbrs)
  (cog-logger-debug "(inheritance-evidence->tv A-mbrs=~a B-mbrs=~a)" A-mbrs B-mbrs)
  (let* ;; TODO consider TVs of the members
      ((K 800) ; parameter to convert from count to confidence
       (A-size (length A-mbrs))
       (AB-mbrs (lset-intersection equal? A-mbrs B-mbrs))
       (AB-size (length AB-mbrs))
       (strength (if (< 0 A-size)
                     (exact->inexact (/ AB-size A-size))
                     1))
       (confidence (if (< 0 A-size)
                       (exact->inexact (/ A-size (+ A-size K)))
                       0)))
    (stv strength confidence)))

;; Formula
(define (inheritance-direct-introduction conclusion . premises)
  (cog-logger-debug "(inheritance-direct-introduction conclusion=~a . premises=~a)" conclusion premises)
  (cog-logger-debug "rule (cog-atomspace) = ~a" (cog-atomspace))
  (if (= (length premises) 2)
      (let* ((Inh conclusion)
             (dummy (cog-logger-debug "Inh=~a" Inh))
             (A (car premises))
             (dummy (cog-logger-debug "A=~a" A))
             (B (cadr premises))
             (dummy (cog-logger-debug "B=~a" B))
             ;; Fetch all members of A and B
             (A-mbrs (get-members A))
             (dummy (cog-logger-debug "A-mbrs=~a" A-mbrs))
             (B-mbrs (get-members B))
             (dummy (cog-logger-debug "B-mbrs=~a" B-mbrs))
             ;; Calculate the TV based on the members of A and B
             (tv (inheritance-evidence->tv A-mbrs B-mbrs))
             (dummy (cog-logger-debug "tv=~a" tv)))
        (cog-merge-hi-conf-tv! Inh tv))))

(define inheritance-direct-introduction-rule-name
  (DefinedSchemaNode "inheritance-direct-introduction-rule"))
(DefineLink inheritance-direct-introduction-rule-name
  inheritance-direct-introduction-rule)
