;; Load PLN (with only the rule that matters)
(use-modules (opencog pln))
(pln-load 'empty)
(pln-load-rule 'present-deduction-inheritance)

;; Load KB
(load "kb.scm")

;; Build query to find all ancestors of A (according to inheritance)
(define A (Concept "A"))
(define X (Variable "$X"))
(define vardecl (TypedVariable X (Type "ConceptNode")))
(define AX (Inheritance A X))

;; Call forward chainer on the query
(define fcresults (pln-fc AX #:vardecl vardecl))

;; Get all ancestors
(define ancestors (cog-execute! (Get vardecl AX)))
