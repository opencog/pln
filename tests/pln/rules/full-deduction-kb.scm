;; KB for testing the full deduction rule

;; Premises
(define P (Predicate "P"))
(define Q (Predicate "Q"))
(define R (Predicate "R"))
(define PQ-tv (stv 0.7 0.8))
(define PQR-tv (stv 0.8 0.6))
(define PNQR-tv (stv 0.9 0.4))
(define PQ (Implication PQ-tv P Q))
(define PQR (Implication PQR-tv (And P Q) R))
(define PNQR (Implication PNQR-tv (And P (Not Q)) R))

;; Conclusion
(define conclusion (Implication P R))
