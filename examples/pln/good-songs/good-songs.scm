;; Load PLN
(use-modules (opencog pln))
(pln-load-rule 'fuzzy-conjunction-introduction-2ary)
(pln-load-rule 'conditional-full-instantiation-implication-scope-meta)

(use-modules (opencog logger))
(use-modules (opencog ure))
;; (cog-logger-set-level! "fine")
(ure-logger-set-level! "debug")

;; Load KB
(load "kb.scm")

;; What does Marry like?
(define what (Variable "$what"))
(define vardecl (TypedVariable what (Type "ConceptNode")))
(define sources (Set
                  song-3-composed-by-author-2
                  marry-like-song-3
                  listener-like-song-from-same-author))
(define target (Evaluation like (List marry what)))

;; Call forward chainer
;;
;; fc-results includes, among others,
;;
;; (EvaluationLink (stv 0.9 0.5625)
;;   (PredicateNode "like")
;;   (ListLink
;;     (ConceptNode "Marry")
;;     (ConceptNode "Dextrose is my bitch")))
;;
;; Because Marry likes a song from the same author
(define fc-results
  (pln-fc sources #:maximum-iterations 20 #:fc-retry-exhausted-sources #t))

;; Call backward chainer
;;
;; The following does not work due to the lack of full support of
;; meta-rule in backward chainer.
;;
;; (pln-bc target #:vardecl vardecl #:maximum-iterations 1000)
