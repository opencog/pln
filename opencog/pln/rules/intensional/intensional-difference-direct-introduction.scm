;; Rule for introducing
;;
;; IntensionalDifferenceLink
;;   A
;;   B
;;
;; based on direct evidence of patterns of A and B, where a pattern of
;; A is a super set of A with a description shorter than A.
;;
;; A
;; B
;; precondition: there exists X, (Attraction A X) and (Attraction B X)
;; |-
;; IntensionalDifference <TV>
;;   A
;;   B
;;
;; where TV is
;;
;; ExtensionDifferenceLink <TV>
;;   patterns-of(A)
;;   patterns-of(B)
;;
;; patterns-of(A) is defined as the satifying set of pattern-of(X,A)
;; over X, where pattern-of(X,A) is calculated as follows
;;
;;   pattern-of(X,A) = s(X) × (P(X|A)-P(X|¬A))+
;;
;; where s(X) is the prior of X, reflecting it's simplicity. Thus the
;; simpler X is and the stronger the discriminating power of X over A
;; is, the more X is a pattern of A. Also, (x)+ is the posivitive part
;; of x, see
;; https://en.wikipedia.org/wiki/Positive_and_negative_parts.
;;
;; For the discriminating power of X over A, we also say that A is
;; attracted to pattern X, which can be represented with
;;
;; AttractionLink
;;   A
;;   X
;;
;; Although not present in the premises, this rule requires all
;; relevant attraction links to be present in the atomspace in order
;; to correctly calculate the TV.

;; TODO: in order to add the Attraction links in the premises maybe an
;; idea would be to introduce a has-closure predicate, such as
;;
;; Evaluation (stv 1 1)
;;   Predicate "has-closure"
;;   (Lambda X (Attraction A X))
;;
;; and
;;
;; Evaluation (stv 1 1)
;;   Predicate "has-closure"
;;   (Lambda X (Attraction B X))
;;
;; Or maybe even introduce a HasClosureLink, such as
;;
;; (HasClosureLink (stv 1 1)
;;   X
;;   (Attraction A X))

;; Rule
(define intensional-difference-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (X (Variable "$X"))
         (CT (Type "ConceptNode")))
    (Bind
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT))
      (And
        (Present
          A
          B)
        ;; There exists X such that
        ;;
        ;; (Attraction A X)
        ;; (Attraction B X)
        ;;
        ;; are present in the atomspace
        (Satisfaction
          (TypedVariable X CT)
          (Present
            (Attraction A X)
            (Attraction B X)))
        ;; A and B are different
        (Not (Equal A B)))
      (ExecutionOutput
        (GroundedSchema "scm: intensional-difference-direct-introduction")
        (List
          ;; Conclusion
          (IntensionalDifference A B)
          ;; Premises
          A
          B)))))

;; Formula
(define (intensional-difference-direct-introduction conclusion . premises)
  ;; Given a concept return all attraction link
  ;;
  ;; Attraction <TV>
  ;;   A
  ;;   X
  (define (get-attractions A)
    (let* ((at-links (cog-filter 'AttractionLink (cog-incoming-set A)))
           (A-at? (lambda (x) (equal? A (gar x)))))
      (filter A-at? at-links)))

  ;; The pattern strength is the product of the mean and the
  ;; confidence of the TV on the attraction link
  ;;
  ;; Attraction <TV>
  ;;   A
  ;;   X
  (define (get-pattern-strength A pat)
    (let* ((A-at (cog-link 'AttractionLink A pat)))      
      (if (null? A-at) 0 (* (cog-mean A-at) (cog-confidence A-at)))))

  ;; Given the attraction links of A and B calculate the fuzzy
  ;; difference between the patterns of A and B, expressed as
  ;;
  ;; Sum_x min(pattern-of(X,A), 1 - pattern-of(X,B))
  ;;
  ;; where pattern-of(X,A) is the strength of the TV of
  ;;
  ;; Attraction <TV>
  ;;   A
  ;;   X
  (define (numerator A-ats B)
    (define (fuzzy-difference A-at)
      (let* ((pat (gdr A-at)))
        (min (cog-mean A-at) (- 1 (get-pattern-strength B pat)))))
    (fold + 0 (map fuzzy-difference A-ats)))

  ;; (cog-logger-debug "(intensional-difference-direct-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 2)
      (let* ((IntInh conclusion)
             (A (car premises))
             (B (cadr premises))
             ;; Fetch all pattern attraction links and patterns
             (A-ats (get-attractions A))
             (B-ats (get-attractions B))
             (A-pats (map gdr A-ats))
             (all-cpts (cog-get-atoms 'ConceptNode #t))
             (usize (length all-cpts))  ; Universe size
             ;; Calculate denominator, numerator and TV
             (dnt usize)
             (TVs (if (< 0 dnt) (/ (numerator A-ats B) dnt) 1))
             (TVc (count->confidence dnt))
             (TV (stv TVs TVc)))
        (if (< 0 TVc) (cog-merge-hi-conf-tv! IntInh TV)))))

; Name the rule
(define intensional-difference-direct-introduction-rule-name
  (DefinedSchemaNode "intensional-difference-direct-introduction-rule"))
(DefineLink intensional-difference-direct-introduction-rule-name
  intensional-difference-direct-introduction-rule)
