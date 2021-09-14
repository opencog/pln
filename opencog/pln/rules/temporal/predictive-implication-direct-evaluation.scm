;; PredictiveImplicationLink direct evaluation rule
;;
;; PredictiveImplicationLink
;;   T
;;   P
;;   Q
;; |-
;; PredictiveImplicationLink <TV>
;;   T
;;   P
;;   Q
;;
;; where TV is calculated using direct evidence obtained from
;; timestamped instances of P and Q (thus extensional, not mixed).
;;
;; WARNING: Actually this is probably wrong and contrary to the
;; semantics of PredictiveImplicationLink which should probably be
;; defined as
;;
;; PredictiveImplicationLink
;;   T
;;   Lambda
;;     <vardecl-including-one-temporal-variable>
;;     <P-body-containing-variables-from-vardecl>
;;   Lambda
;;     <vardecl-including-one-temporal-variable>
;;     <Q-body-containing-variables-from-vardecl>
;;
;; is equivalent
;;
;; PredictiveImplicationScopeLink
;;   <vardecl-including-one-temporal-variable>
;;   T
;;   <P-body-containing-variables-from-vardecl>
;;   <Q-body-containing-variables-from-vardecl>
;;
;; that is PredictiveImplicationLink is simply between predicates that
;; happen to have a temporal component.

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

(load "utils.scm")

;; Rule
(define predictive-implication-direct-evaluation-rule
  (let* ((PI (Variable "$PI"))
	 (PITy (Type 'PredictiveImplicationLink)))
    (Bind
      (TypedVariable PI PITy)
      (And
        (Present PI)
	(IsClosed PI))
      (ExecutionOutput
        (GroundedSchema "scm: predictive-implication-direct-evaluation")
	(List
          ;; Conclusion. No premises to avoid proof tree cycle.
          PI)))))

;; Formula.  Assume crisps observations for now.
(define (predictive-implication-direct-evaluation conclusion . premises)
  (ure-logger-fine "(predictive-implication-direct-evaluation conclusion[~x]=~a . premises=~a)" (cog-handle conclusion) conclusion premises)
  (if (= (length premises) 0)
      (let* ((PI conclusion)
	     (T (Variable "$T"))
	     (TimeT (TypeInh 'NaturalLink))
	     (ante-atime-events (get-pi-antecedants PI))
	     (ante-timed-events (map (lambda (x) (AtTime x T)) ante-atime-events))
	     (ante-body (And
			  (Present ante-timed-events)
			  (IsClosed ante-timed-events)
			  (IsTrue ante-timed-events)))
	     (ante-vardecl (TypedVariable T TimeT))
	     (ante-query (Get ante-vardecl ante-body))
	     (ante-res (cog-execute! ante-query))
	     (ante-res-lst (cog-outgoing-set ante-res))
	     (ante-size (length ante-res-lst)))
	(if (< 0 ante-size)
	    (let* (;; For each evidence check if the succedent is true at T+LAG
		   (lag (get-pi-lag PI))
		   (_ (ure-logger-fine "lag = ~a" lag))
		   (plus-lag (lambda (t) (temporal-plus lag t)))
		   (succ-atime-events (get-pi-succedents PI))
		   (Q (car succ-atime-events)) ; TODO: only one succedent assumed
		   (_ (ure-logger-fine "Q = ~a" Q))
		   (QT1 (lambda (t) (AtTime Q (plus-lag t))))
		   (true? (lambda (x)
			    (and (not (null? x)) (tv->bool (cog-tv x)))))
		   (succ-true? (lambda (t)
				 (true? (QT1 t))))
		   (succ-lst (filter succ-true? ante-res-lst))
		   (succ-size (length succ-lst))
		   ;; Calculate the TV of the predictive implication scope
		   (strength (exact->inexact (/ succ-size ante-size)))
		   (confidence (count->confidence ante-size))
		   (tv (stv strength confidence)))
	      (ure-logger-fine "tv = ~a" tv)
	      (cog-merge-hi-conf-tv! conclusion tv))))))

;; Declaration
(define predictive-implication-direct-evaluation-rule-name
  (DefinedSchemaNode "predictive-implication-direct-evaluation-rule"))
(DefineLink predictive-implication-direct-evaluation-rule-name
  predictive-implication-direct-evaluation-rule)
