;; Temporal deduction, for now specialized for cognitive scheme action planning and
;; BackPredictiveImplicationScopeLink.
;;
;; BackPredictiveImplicationScope
;;   V
;;   T1
;;   P
;;   Q
;; BackPredictiveImplicationScope
;;   V
;;   T2
;;   And
;;     Q
;;     A
;;   R
;; |-
;; BackPredictiveImplicationScope
;;   V
;;   T2
;;   BackSequentialAnd
;;     T1
;;     P
;;     A
;;   R
;;
;; Written in a more compact form (ignoring the lags) it is
;;
;; P↝ᵀ¹Q
;; (Q∧A)↝ᵀ²R
;; ⊢
;; (P≺ᵀ¹A)↝ᵀ²R
;;
;; Or simply (ignoring the lags)
;;
;; P↝Q
;; (Q∧A)↝R
;; ⊢
;; (P≺A)↝R
;;
;; Typically the first premise P↝Q contains the first part of the
;; plan, such as
;;
;; (C∧A₁)≺A₂≺A₃↝Q
;;
;; meaning, if C, the initial context is true, and A₁ is taken (at the
;; same time C is true), then A₂ is taken followed by A₃, then Q will
;; be true.
;;
;; And the second premise contains the last part of the plan, such as
;;
;; Q∧A₄↝R
;;
;; meaning, if Q, the penultimate context is true, and A₄ is taken at
;; the same time, then R will be true. Which, in this example, allows
;; to infer the full plan
;;
;; (C∧A₁)≺A₂≺A₃≺A₄↝R
;;
;; Obviously these notations should be understood in Atomese terms as
;;
;; ∧: And
;; ≺: SequentialAnd
;; ↝: PredictiveImplication[Scope]
;;
;; It should be noted that such rule is not complete in the case too
;; much uncertainty is present and failure to reach the penultimate
;; context is probable (especially if executing the last action has a
;; detrimental effect on the goal). Such complete rule would look
;; like:
;;
;; P↝Q
;; Q∧A↝R
;; ¬Q∧A↝R
;; ⊢
;; P≺A↝R
;;
;; The regular deduction rule is also incomplete for the same reason,
;; thus for now we will make the same assumption which is, in this
;; context, that R is independent of A if executed while Q is not
;; true.
;;
;; It should be noted that if executing A while Q is not true is too
;; detrimental to acheiving R, then one needs more than a sequential
;; plan. One needs a plan tree, perhaps called behavior tree, -- see
;; https:;;en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)
;; and https:;;wiki.opencog.org/w/Behavior_tree_(2015_Archive) for an
;; early implementational attempt in OpenCog -- as to conditionalize
;; actions based on whether an intermediate state has been reached or
;; not. Using such behavior trees as plans would allow to obtain
;; cognitive schematics with much higher chances of success than with
;; linear plans (unless the second action is "to plan", in which case
;; it would in principle create a recursion exploring all branches
;; akin to how AIXI works).
;;
;; Other implicit premises are the probabilities of P and Q (like for
;; the regular deduction rule).

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

;; Rule
(define back-predictive-implication-scope-deduction-cogscm-rule
  (let* ((V (Variable "$vardecl"))
     (T1 (Variable "$lag-1"))
     (T2 (Variable "$lag-2"))
     (P (Variable "$P"))
     (Q (Glob "$Q"))
     (semi-open (Interval (Number 0) (Number -1)))
     (R (Variable "$R"))
     (A (Variable "$A"))
     (ExecutionT (Type 'ExecutionLink))
     (NaturalT (TypeInh 'NaturalLink))
     (VariableT (TypeInh 'VariableNode))
     (VariableSetT (Type 'VariableSet))
     (VariableListT (Type 'VariableList))
     (TypedVariableT (Type 'TypedVariableLink))
     (VardeclT (TypeChoice
             VariableT
             VariableSetT
             VariableListT
             TypedVariableT))
     ;; Rule vardecl
     (vardecl (VariableSet
            (TypedVariable V VardeclT)
            (TypedVariable T1 NaturalT)
            (TypedVariable T2 NaturalT)
            (TypedVariable A ExecutionT)
            P
            (TypedVariable Q semi-open)
            R))
     ;; Rule clauses
     (PQ (Quote (BackPredictiveImplicationScope (Unquote V) (Unquote T1) (Unquote P) (Unquote (And Q)))))
     (QA (And Q A))
     (QAR (Quote (BackPredictiveImplicationScope (Unquote V) (Unquote T2) (Unquote QA) (Unquote R))))
     (present-clauses (Present PQ QAR))
     (precondition-clauses (IsClosed PQ QAR))
     ;; Rule rewriting term
     (PA (BackSequentialAnd T1 P A))
     (PAR (Quote (BackPredictiveImplicationScope (Unquote V) (Unquote T2) (Unquote PA) (Unquote R)))))
    (Bind
      vardecl
      (And
        present-clauses
        precondition-clauses)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-deduction-cogscm")
        (List
            ;; Conclusion
            PAR
            ;; Premises
            ;; Not closed premises are risky because inference tree would have free
            ;; variables which will result to ambiguty.
            ;; TODO: Use Lambda and Create a closed premise for P, Q and R.
            P
            (And Q)
            R
            PQ
            QAR)))))

;; The formula can be derived from the definition of
;; PredictiveImplicationLink
;; https://wiki.opencog.org/w/PredictiveImplicationLink#Semantics
(define (back-predictive-implication-scope-deduction-cogscm conclusion . premises)
  (ure-logger-fine "(back-predictive-implication-scope-deduction-cogscm conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 5)
     (let* ((PAR conclusion)
            (P (list-ref premises 0))
            (Q (list-ref premises 1))
            (R (list-ref premises 2))
            (PQ (list-ref premises 3))
            (QAR (list-ref premises 4))
            ;; From A lookback variant definition of PredictiveImplicationLink,
            ;; the ImplicationLink equivalence for PQ, QAR and PAR is as follows:
            ;; PQ <=>
            ;; (Implication
            ;;    (Lagged (Lambda (V T) (AtTime P T)) T1)
            ;;    (Lambda (V T) (AtTime Q T))
            ;; QAR <=>
            ;; (Implication
            ;;    (Lagged (Lambda (V T) (AtTime (And Q A) T)) T2)
            ;;    (Lambda (V T) (AtTime R T))
            ;;
            ;; use simple-deduction-strength-formula to get TV for the conclusion, PAR.
            ;; Hence, based on deduction rule:
            ;; A->B
            ;; B->C
            ;; A = (Lagged (Lambda (V T) (AtTime P T)) T1)
            ;; A <TV> is similar to P <TV>
            (sA (cog-mean P))
            (cA (cog-confidence P))
            ;; B = (Lagged (Lambda (V T) (AtTime (And Q A) T)) T2)
            ;; B <TV> is similar to Q <TV>
            (sB (cog-mean Q))
            (cB (cog-confidence Q))
            ;; C = (Lambda (V T) (AtTime R T))
            ;; C <TV> is similar to R <TV>
            (sC (cog-mean R))
            (cC (cog-confidence R))
            ;; A->B <=> PQ
            (sAB (cog-mean PQ))
            (cAB (cog-confidence PQ))
            ;; B->C <=> QAR
            (sBC (cog-mean QAR))
            (cBC (cog-confidence QAR)))
        (if (and
            (or (= 0 cA) (= 0 cB) (= 0 cAB)
                (conditional-probability-consistency sA sB sAB))
            (or (= 0 cB) (= 0 cC) (= 0 cBC)
                (conditional-probability-consistency sB sC sBC)))
            (let*
                ((sPAR (simple-deduction-strength-formula sA sB sC sAB sBC))
                (cPAR (min cAB cBC)))
                (cog-merge-hi-conf-tv! PAR (stv sPAR cPAR)))))))

;; Limit an number to be within a certain range
(define (limit x l u)
  (max l (min u x)))

(define (simple-deduction-strength-formula sA sB sC sAB sBC)
  (if
     (and
        (conditional-probability-consistency sA sB sAB)
        (conditional-probability-consistency sB sC sBC))
     ;; Preconditions are met
     (if (< 0.99 sB)
        ;; sB tends to 1
        sC
        ;; otherwise
        (+ (* sAB sBC) (/ (* (- 1 sAB) (- sC (* sB sBC))) (- 1 sB))))
     ;; Preconditions are not met
     0))

; Consistency Conditions
(define (smallest-intersection-probability sA sB)
  (limit (/ (+ sA sB -1) sA) 0 1))

(define (largest-intersection-probability sA sB)
  (limit (/ sB sA) 0 1))

(define (conditional-probability-consistency sA sB sAB)
  (and (< 0 sA)
       (<= (smallest-intersection-probability sA sB) sAB)
       (<= sAB (largest-intersection-probability sA sB))))

;; Declaration
(define back-predictive-implication-scope-deduction-cogscm-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-deduction-cogscm-rule"))
(DefineLink back-predictive-implication-scope-deduction-cogscm-rule-name
  back-predictive-implication-scope-deduction-cogscm-rule)
