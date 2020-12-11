;; PredictiveImplicationScopeLink direct evaluation rule
;;
;; PredictiveImplicationScope
;;   V
;;   T
;;   P
;;   Q
;; |-
;; PredictiveImplicationScope <TV>
;;   V
;;   T
;;   P
;;   Q
;;
;; where TV is calculated using direct evidence obtained from
;; timestamped instances of P and Q (thus extensional, not mixed).

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

;; Rule
(define predictive-implication-scope-direct-evaluation-rule
  (let* ((PIS (Variable "$PIS"))
	 (PISTy (Type 'PredictiveImplicationScopeLink)))
    (Bind
      (TypedVariable PIS PISTy)
      (And
        (Present PIS)
	(IsClosed PIS))
      (ExecutionOutput
        (GroundedSchema "scm: predictive-implication-scope-direct-evaluation")
	(List
          ;; Conclusion. No premises to avoid proof tree cycle.
          PIS)))))

;; Helpers
;; TODO: move to util file
(define (get-pis-antecedants PIS)
"
  Return the antecedants of a predictive implication scope. For instance given

  PredictiveImplicationScope
    <vardecl>
    <offset>
    And
      <P1>
      <P2>
    <Q>

  then return a scheme list with P1 and P2.
"
  (let* ((P (cog-outgoing-atom PIS 2)))
    (if (equal? (cog-type P) 'AndLink)
	(cog-outgoing-set P)
	(list P))))

;; TODO: move to util file
(define (get-pis-succedents PIS)
"
  Return the succedent of a predictive implication scope. For instance given

  PredictiveImplicationScope
    <vardecl>
    <offset>
    And
      <P1>
      <P2>
    <Q>

  then return a scheme list with Q.
"
  (let* ((P (cog-outgoing-atom PIS 3)))
    (if (equal? (cog-type P) 'AndLink)
	(cog-outgoing-set P)
	(list P))))

;; TODO: move to util file
(define (get-typed-vars vardecl)
"
  Take a variable declaration and output its scheme list of typed variables
"
  (if (or (equal? (cog-type vardecl) 'VariableSet)
	  (equal? (cog-type vardecl) 'VariableList))
      (cog-outgoing-set vardecl)
      (list vardecl)))

;; TODO: move to util file
(define (vardecl-append vardecl1 vardecl2)
"
  Take 2 variable declarations and append them. For now it is assumed
  that they do not have any variable in common. Also the resulting
  variable declaration will always be a list, regardless of whether the
  input variable declarations are sets.
"
  (let* ((lst (append (get-typed-vars vardecl1) (get-typed-vars vardecl2))))
    (if (< 1 (length lst))
	(VariableList lst)
	(car lst))))

;; TODO: move to util file
(define (get-vardecl PIS)
"
  Return the variable declaration of a PredicateImplicationScope
"
  (cog-outgoing-atom PIS 0))

;; TODO: move to util file
(define (get-pis-lag PIS)
"
  Return the lag of a PredictiveImplicationLink.

  That is given

  PredictiveImplicationLink
    <lag>
    P
    Q

  returns <lag>
"
  (cog-outgoing-atom PIS 1))

;; TODO: move to util file
(define (temporal-plus T1 T2)
"
  Calculate the addition of two time nodes (or here naturals for now).
"
  (if (equal? (cog-type T1) 'ZLink)
      T2
      (temporal-plus (cog-outgoing-atom T1 0) (S T2))))

;; Formula.  Assume crisps observations for now.
(define (predictive-implication-scope-direct-evaluation conclusion . premises)
  (ure-logger-fine "(predictive-implication-scope-direct-evaluation conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 0)
      (let* ((PIS conclusion)
	     (T (Variable "$T"))
	     (TimeT (TypeInh 'NaturalLink))
	     (ante-atime-events (get-pis-antecedants PIS))
	     (ante-timed-events (map (lambda (x) (AtTime x T)) ante-atime-events))
	     (ante-body (And
			  (Present ante-timed-events)
			  (IsClosed ante-timed-events)
			  (IsTrue ante-timed-events)))
	     (ante-vardecl (vardecl-append (TypedVariable T TimeT)
					   (get-vardecl PIS)))
	     (ante-query (Get ante-vardecl ante-body))
	     (ante-res (cog-execute! ante-query))
	     (ante-res-lst (cog-outgoing-set ante-res))
	     (ante-size (length ante-res-lst)))
	(if (< 0 ante-size)
	    (let* (;; For each evidence check if the succedent is true at T+LAG
		   (lag (get-pis-lag PIS))
		   (only-time (= 1 (length (get-typed-vars ante-vardecl))))
		   (get-time (lambda (p) (if only-time
					     p
					     (cog-outgoing-atom p 0))))
		   (plus-lag (lambda (t) (temporal-plus lag t)))
		   (succ-atime-events (get-pis-succedents PIS))
		   (Q (car succ-atime-events)) ; TODO: only one succedent assumed
		   (QT1 (lambda (p) (AtTime Q (plus-lag (get-time p)))))
		   (true? (lambda (x) (and (not (null? x)) (tv->bool (cog-tv x)))))
		   (succ-true? (lambda (p) (true? (QT1 p))))
		   (succ-lst (filter succ-true? ante-res-lst))
		   (succ-size (length succ-lst))
		   ;; Calculate the TV of the predictive implication scope
		   (strength (exact->inexact (/ succ-size ante-size)))
		   (confidence (count->confidence ante-size))
		   (tv (stv strength confidence)))
	      (cog-merge-hi-conf-tv! conclusion tv))))))

;; Declaration
(define predictive-implication-scope-direct-evaluation-rule-name
  (DefinedSchemaNode "predictive-implication-scope-direct-evaluation-rule"))
(DefineLink predictive-implication-scope-direct-evaluation-rule-name
  predictive-implication-scope-direct-evaluation-rule)
