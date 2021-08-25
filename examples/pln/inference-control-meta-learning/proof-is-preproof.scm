;; 2. If and-BIT A is a proof of target T, then it is a preproof of
;;    target T as well.
;;
;; Implication <1 1>
;;   Predicate "URE:BC:proof-of"
;;   Predicate "URE:BC:preproof-of"
(define proof-is-preproof
  (Implication (stv 1 1)
    (Predicate "URE:BC:proof-of")
    (Predicate "URE:BC:preproof-of")))

;; Load conditional-total-instantiation-implication-meta-rule
(add-to-load-path "../../../opencog/pln/")
(load-from-path "meta-rules/predicate/conditional-total-instantiation.scm")

;; Turn proof-is-preproof into a rule
(define proof-is-preproof-rule
  (car (apply-rule conditional-total-instantiation-implication-meta-rule
                   proof-is-preproof)))
