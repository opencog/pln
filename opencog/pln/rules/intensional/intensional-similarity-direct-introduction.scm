;; Rule for introducing
;;
;; IntensionalSimilarityLink
;;   A
;;   B
;;
;; based on direct evidence of patterns between A and B, where a
;; pattern of A is a super set of A with a description shorter than A.
;;
;; A
;; B
;; precondition: there exists X, (Attraction A X) and (Attraction B X)
;; |-
;; IntensionalSimilarity <TV>
;;   A
;;   B
;;
;; where TV is
;;
;; ExtensionSimilarityLink <TV>
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
(define intensional-similarity-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (X (Variable "$X"))
         (CT (TypeInh "ConceptNode")))
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
        (Not (Identical A B)))
      (ExecutionOutput
        (GroundedSchema "scm: intensional-similarity-direct-introduction")
        (List
          ;; Conclusion
          (IntensionalSimilarity A B)
          ;; Premises (wrapped in Set because commutative)
          (Set
            A
            B))))))

;; Formula
(define (intensional-similarity-direct-introduction conclusion . premises)
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
  ;; intersection between the patterns of A and B, expressed as
  ;;
  ;; Sum_x min(pattern-of(X,A), pattern-of(X,B))
  ;;
  ;; where pattern-of(X,A) is the strength of the TV of
  ;;
  ;; Attraction <TV>
  ;;   A
  ;;   X
  (define (numerator A B-ats)
    (define (fuzzy-intersect B-at)
      (let* ((pat (gdr B-at)))
        (min (get-pattern-strength A pat) (cog-mean B-at))))
    (fold + 0 (map fuzzy-intersect B-ats)))

  ;; Given the attraction links of A and B calculate the fuzzy sum of
  ;; the union of patterns of A and B expressed as
  ;;
  ;; Sum_x max(pattern-of(X,A), pattern-of(X,B))
  (define (denominator A B pats)
    (define (fuzzy-union pat)
      (let ((A-pat-strength (get-pattern-strength A pat))
            (B-pat-strength (get-pattern-strength B pat)))
        (max A-pat-strength B-pat-strength)))
    (fold + 0 (map fuzzy-union pats)))

  ;; (cog-logger-debug "(intensional-similarity-direct-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((IntInh conclusion)
             (A (gar (car premises)))
             (B (gdr (car premises)))
             ;; Fetch all pattern attraction links and patterns
             (A-ats (get-attractions A))
             (B-ats (get-attractions B))
             (A-pats (map gdr A-ats))
             (B-pats (map gdr B-ats))
             (pats (lset-union equal? A-pats B-pats))
             ;; Calculate denominator, then
             (dnt (denominator A B pats))
             (TVs (if (< 0 dnt) (/ (numerator A B-ats) dnt) 1))
             (TVc (count->confidence (length B-ats)))
             (TV (stv TVs TVc)))
        (if (< 0 TVc) (cog-merge-hi-conf-tv! IntInh TV)))))

; Name the rule
(define intensional-similarity-direct-introduction-rule-name
  (DefinedSchemaNode "intensional-similarity-direct-introduction-rule"))
(DefineLink intensional-similarity-direct-introduction-rule-name
  intensional-similarity-direct-introduction-rule)
