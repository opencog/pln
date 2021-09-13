;; BackPredictiveImplicationScopeLink direct evaluation rule
;; (lookback variant of PredictiveImplicationScopeLink)
;;
;; BackPredictiveImplicationScope
;;   V
;;   T
;;   P
;;   Q
;; |-
;; BackPredictiveImplicationScope <TV>
;;   V
;;   T
;;   P
;;   Q
;;
;; where TV is calculated using direct evidence obtained from
;; timestamped instances of P and Q (thus extensional, not mixed).

(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

(load "utils.scm")

;; Rule
(define back-predictive-implication-scope-direct-evaluation-rule
  (let* ((BPIS (Variable "$BPIS"))
         (BPISTy (Type 'BackPredictiveImplicationScopeLink)))
    (Bind
      (TypedVariable BPIS BPISTy)
      (And
        (Present BPIS)
        (IsClosed BPIS))
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-direct-evaluation")
        (List
          ;; Conclusion. No premises to avoid proof tree cycle.
          BPIS)))))

;; Helpers
(define (Z? X)
  (equal? (cog-type X) 'ZLink))

(define (S? X)
  (equal? (cog-type X) 'SLink))

(define (variable? X)
  (equal? (cog-type X) 'VariableNode))

(define (and? X)
  (equal? (cog-type X) 'AndLink))

(define (back-sequential-and? X)
  (equal? (cog-type X) 'BackSequentialAndLink))

(define (get-time X)
"
  Given a timed atom return the time.  That is if X is

  (AtTime <A> <T>)

  return <T>
"
  (cog-outgoing-atom X 1))

(define (get-seq-lag SEQ)
"
  Return the lag of a BackSequentialAnd.

  That is given

  BackSequentialAnd
    <lag>
    <antecedent>
    <succedent>

  returns <lag>
"
  (cog-outgoing-atom SEQ 0))

(define (get-seq-antecedent SEQ)
"
  Return the antecedent of a BackSequentialAnd.

  That is given

  BackSequentialAnd
    <lag>
    <antecedent>
    <succedent>

  returns <antecedent>
"
(cog-outgoing-atom SEQ 1))

(define (get-seq-succedent SEQ)
"
  Return the succedent of a BackSequentialAnd.

  That is given

  BackSequentialAnd
    <lag>
    <antecedent>
    <succedent>

  returns <succedent>
"
(cog-outgoing-atom SEQ 2))

(define (get-pis-antecedent-timed-clauses BPIS T)
"
  Return the antecedent timed clauses of a back predictive implication scope.

  For instance given

  PredictiveImplicationScope
    <vardecl>
    <lag-2>
    SequentialAnd
      <lag-1>
      <P1>
      <P2>
    <Q>

  the return the following scheme list

  ((AtTime <P1> T) (AtTime <P2> (S T)))

  assuming <lag-1> is (S (Z)).
"
  (to-timed-clauses (get-pis-antecedent BPIS) T))

(define (get-max-time timed-clauses)
"
  Given a list of timed clauses, return the maximum lag w.r.t. T.
"
  (define head-time (get-time (car timed-clauses)))
  (if (< 1 (length timed-clauses))
      (lag-max head-time (get-max-time (cdr timed-clauses)))
      head-time))

(define (lag-max LAG1 LAG2)
"
  Return the max between two lags (including if they wrap variables).
"
  (cond [(or (Z? LAG1) (variable? LAG1)) LAG2]
    [(or (Z? LAG2) (variable? LAG2)) LAG1]
    [else (S (cog-outgoing-atom LAG1 0) (cog-outgoing-atom LAG2 0))]))

(define (get-pis-succedent-timed-clauses BPIS T)
"
  Return the succedent timed clauses of a predictive implication scope.

  For instance given

  PredictiveImplicationScope
    <vardecl>
    <lag-2>
    SequentialAnd
      <lag-1>
      <P1>
      <P2>
    <Q>

  the return the following scheme list

  ((AtTime <Q> (S (S T))))

  assuming <lag-1> and <lag-2> are both (S (Z)).
"
  (let* ((pis-lag (get-pis-lag BPIS))
     (max-time (get-max-time (get-pis-antecedent-timed-clauses BPIS T)))
     (suc-time (lag-add pis-lag max-time)))
    (to-timed-clauses (get-pis-succedent BPIS) suc-time)))

(define (to-timed-clauses LE T)
"
  Return a list of timed clauses given a structure of lagged events.

  For instance given

    SequentialAnd
      <lag>
      And
        <P>
        <Q>
      <R>

  return the following scheme list (assuming <lag> is 1)

  ((AtTime <P> T) (AtTime <Q> T) (AtTime <R> (S T)))
"
  (define (wrap-T x) (AtTime x T))
  (if (and? LE)
      (append-map (lambda (x) (to-timed-clauses x T)) (cog-outgoing-set LE))
      (if (back-sequential-and? LE)
          (let* ((lag (get-seq-lag LE))
                 (ante (get-seq-antecedent LE))
                 (succ (get-seq-succedent LE))
         (timed-ante (to-timed-clauses ante T))
         (lagged-T (lag-add lag T))
         (timed-succ (to-timed-clauses succ lagged-T)))
        (append timed-ante timed-succ))
          (list (wrap-T LE)))))

;; Formula.  Assume crisps observations for now.
(define (back-predictive-implication-scope-direct-evaluation conclusion . premises)
  (ure-logger-fine "(back-predictive-implication-scope-direct-evaluation conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 0)
      (let* ((BPIS conclusion)
         (T (Variable "$T"))
         (TimeT (TypeInh 'NaturalLink))
         (ante-timed-clauses (get-pis-antecedent-timed-clauses BPIS T))
         (ante-body (And
              (Present ante-timed-clauses)
              (IsClosed ante-timed-clauses)
              (IsTrue ante-timed-clauses)))
         (ante-vardecl (vardecl-append (TypedVariable T TimeT)
                       (get-vardecl BPIS)))
         (ante-query (Get ante-vardecl ante-body))
         (ante-res (cog-execute! ante-query))
         (ante-res-lst (cog-outgoing-set ante-res))
         (ante-size (length ante-res-lst)))
    (if (< 0 ante-size)
        (let* (;; For each evidence check if the succedent is true at T+LAG
           (lag (get-pis-lag BPIS))
           (only-time (= 1 (length (get-typed-vars ante-vardecl))))
           (get-p-time (lambda (p) (if only-time
                         p
                         (cog-outgoing-atom p 0))))
           (plus-lag (lambda (t) (lag-add lag t)))
           (succ-timed-clauses (get-pis-succedent-timed-clauses BPIS T))
           ;; TODO: only one succedent assumed for now
           (succ-timed-clause (car succ-timed-clauses))
           (succ-instantiate (lambda (p)
                       (cog-execute! (Put T succ-timed-clause p))))
           (true? (lambda (x)
                (and (not (null? x)) (tv->bool (cog-tv x)))))
           (succ-true? (lambda (p) (true? (succ-instantiate (get-p-time p)))))
           (succ-lst (filter succ-true? ante-res-lst))
           (succ-size (length succ-lst))
           ;; Calculate the TV of the back predictive implication scope
           (strength (exact->inexact (/ succ-size ante-size)))
           (confidence (count->confidence ante-size))
           (tv (stv strength confidence)))
          (cog-merge-hi-conf-tv! conclusion tv))))))

;; Declaration
(define back-predictive-implication-scope-direct-evaluation-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-direct-evaluation-rule"))
(DefineLink back-predictive-implication-scope-direct-evaluation-rule-name
  back-predictive-implication-scope-direct-evaluation-rule)
