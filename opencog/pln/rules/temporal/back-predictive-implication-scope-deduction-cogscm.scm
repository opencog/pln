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

;; Rule (assumes Q is a conjunction)
(define back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule
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
     (P↝Q (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T1)
              (Unquote P)
              (Unquote (And Q)))))
     (Q∧A (And Q A))
     (Q∧A↝R (Quote
              (BackPredictiveImplicationScope
                (Unquote V)
                (Unquote T2)
                (Unquote Q∧A)
                (Unquote R))))
     (present-clauses (Present P↝Q Q∧A↝R))
     (precondition-clauses (IsClosed P↝Q Q∧A↝R))
     ;; Rule rewriting term
     (P≺A (BackSequentialAnd T1 P A))
     (P≺A↝R (Quote
               (BackPredictiveImplicationScope
                 (Unquote V)
                 (Unquote T2)
                 (Unquote P≺A)
                 (Unquote R)))))
    (Bind
      vardecl
      (And
        present-clauses
        precondition-clauses)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-deduction-cogscm")
        (List
            ;; Conclusion
            P≺A↝R
            ;; Premises
            ;;
            ;; TODO: Use Lambda and create closed premises for P, Q
            ;; and R (because non closed premises are generally risky
            ;; as well as unnecessary).  Alternatively, we might be
            ;; able to avoid variables altogether by using (non-scope)
            ;; PredictiveImplicationLink coupled with appropriate
            ;; predicate constructors.
            P≺A
            Q∧A
            R
            P↝Q
            Q∧A↝R)))))

;; Rule (assumes Q is an EvaluationLink)
(define back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule
  (let* ((V (Variable "$vardecl"))
     (T1 (Variable "$lag-1"))
     (T2 (Variable "$lag-2"))
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     (R (Variable "$R"))
     (A (Variable "$A"))
     (ExecutionT (Type 'ExecutionLink))
     (NaturalT (TypeInh 'NaturalLink))
     (VariableT (TypeInh 'VariableNode))
     (VariableSetT (Type 'VariableSet))
     (VariableListT (Type 'VariableList))
     (TypedVariableT (Type 'TypedVariableLink))
     (EvaluationT (Type 'EvaluationLink))
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
            (TypedVariable Q EvaluationT)
            R))
     ;; Rule clauses
     (P↝Q (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T1)
              (Unquote P)
              (Unquote Q))))
     (Q∧A (And Q A))
     (Q∧A↝R (Quote
              (BackPredictiveImplicationScope
                (Unquote V)
                (Unquote T2)
                (Unquote Q∧A)
                (Unquote R))))
     (present-clauses (Present P↝Q Q∧A↝R))
     (precondition-clauses (IsClosed P↝Q Q∧A↝R))
     ;; Rule rewriting term
     (P≺A (BackSequentialAnd T1 P A))
     (P≺A↝R (Quote
               (BackPredictiveImplicationScope
                 (Unquote V)
                 (Unquote T2)
                 (Unquote P≺A)
                 (Unquote R)))))
    (Bind
      vardecl
      (And
        present-clauses
        precondition-clauses)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-deduction-cogscm")
        (List
            ;; Conclusion
            P≺A↝R
            ;; Premises
            ;;
            ;; TODO: Use Lambda and create closed premises for P, Q
            ;; and R (because non closed premises are generally risky
            ;; as well as unnecessary).  Alternatively, we might be
            ;; able to avoid variables altogether by using (non-scope)
            ;; PredictiveImplicationLink coupled with appropriate
            ;; predicate constructors.
            P≺A
            Q∧A
            R
            P↝Q
            Q∧A↝R)))))

;; The formula can be derived from the definition of
;; PredictiveImplicationLink
;; https://wiki.opencog.org/w/PredictiveImplicationLink#Semantics
(define (back-predictive-implication-scope-deduction-cogscm conclusion . premises)
  (ure-logger-fine "(back-predictive-implication-scope-deduction-cogscm conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 5)
     (let* ((P≺A↝R conclusion)
            (P≺A (list-ref premises 0))
            (Q∧A (list-ref premises 1))
            (R (list-ref premises 2))
            (P↝Q (list-ref premises 3))
            (Q∧A↝R (list-ref premises 4))

            ;; From a lookback variant definition of PredictiveImplicationLink,
            ;; the ImplicationLink equivalence for P↝Q, Q∧A↝R and P≺A↝R is as follows:
            ;;
            ;; P↝Q <=>
            ;; (Implication
            ;;    (Lagged (Lambda (V T) (AtTime P T)) T1)
            ;;    (Lambda (V T) (AtTime Q T))
            ;;
            ;; Q∧A↝R <=>
            ;; (Implication
            ;;    (Lagged (Lambda (V T) (AtTime (And Q A) T)) T2)
            ;;    (Lambda (V T) (AtTime R T))
            ;;
            ;; use simple-deduction-strength-formula to get TV for the conclusion, P≺A↝R.
            ;;
            ;; Hence, based on deduction rule (WARNING: do not confuse
            ;; A a concept or predicate, as defined in the regular
            ;; deduction rule, such as in A->B, and A an action, as
            ;; defined in this file) we obtain the following mapping
            ;; from temporal deduction to regular deduction:
            ;;
            ;; A = (And (Lagged (Lambda (V T) (AtTime P T)) T1) (Lambda T (AtTime A T)))
            ;; A <TV> is similar to P≺A <TV>
            ;;
            ;; B = (Lagged (Lambda (V T) (AtTime (And Q A) T)) T2)
            ;; B <TV> is similar to Q∧A <TV>
            ;;
            ;; C = (Lambda (V T) (AtTime R T))
            ;; C <TV> is similar to R <TV>
            ;;
            ;; AB <=> P↝Q
            ;;
            ;; BC <=> Q∧A↝R
            (A P≺A)
            (B Q∧A)
            (C R)
            (AB P↝Q)
            (BC Q∧A↝R)
            (AC P≺A↝R)

            ;; Get TVs of premises
            (A-tv (cog-tv A))
            (B-tv (cog-tv B))
            (C-tv (cog-tv C))
            (AB-tv (cog-tv AB))
            (BC-tv (cog-tv BC))

            ;; Calculate TV of conclusion
            (AC-tv (deduction-formula A-tv B-tv C-tv AB-tv BC-tv)))

        ;; (ure-logger-fine "A := P≺A = ~a" P≺A)
        ;; (ure-logger-fine "B := Q∧A = ~a" Q∧A)
        ;; (ure-logger-fine "C := R = ~a" R)
        ;; (ure-logger-fine "AB := P↝Q = ~a" P↝Q)
        ;; (ure-logger-fine "BC := Q∧A↝R = ~a" Q∧A↝R)
        ;; (ure-logger-fine "AC := P≺A↝R = ~a" P≺A↝R)
        ;; (ure-logger-fine "AC-tv = ~a" AC-tv)

        (if (< 0 (cog-tv-confidence AC-tv))
            (cog-merge-hi-conf-tv! AC AC-tv)))))

;; Limit a number to be within a certain range
(define (limit x l u)
  (max l (min u x)))

;; Calculate the TV of the conclusion of the deduction given the TVs
;; of its premises
(define (deduction-formula A-tv B-tv C-tv AB-tv BC-tv)
  (define sA (cog-tv-mean A-tv))
  (define cA (cog-tv-confidence A-tv))
  (define sB (cog-tv-mean B-tv))
  (define cB (cog-tv-confidence B-tv))
  (define sC (cog-tv-mean C-tv))
  (define cC (cog-tv-confidence C-tv))
  (define sAB (cog-tv-mean AB-tv))
  (define cAB (cog-tv-confidence AB-tv))
  (define sBC (cog-tv-mean BC-tv))
  (define cBC (cog-tv-confidence BC-tv))

  (if
     (and
      ;; Since sA is not used in the deduction formula, if its
      ;; confidence is null we don't do the conditional probability
      ;; consistency check for AB, only for BC.
      (or (= 0 cA) (conditional-probability-consistency sA sB sAB))
      (conditional-probability-consistency sB sC sBC))

     ;; Preconditions are met
     (stv (naive-deduction-strength-formula sA sB sC sAB sBC)
          (naive-deduction-confidence-formula cAB cBC))

     ;; Preconditions are not met
     (stv 1 0)))

;; Naive formula for calculating the strength of the conclusion given
;; the strengths of its premises
(define (naive-deduction-strength-formula sA sB sC sAB sBC)
  (+ (* sAB sBC) (/ (* (- 1 sAB) (- sC (* sB sBC))) (- 1 sB))))

;; Naive formula for calculating the confidence of the conclusion
;; given the confidences of its premises
(define (naive-deduction-confidence-formula cAB cBC)
  (define α 0.9) ; Degradation factor
  (* (min cAB cBC) α))

; Consistency Conditions
(define (smallest-intersection-probability sA sB)
  (limit (/ (+ sA sB -1) sA) 0 1))

(define (largest-intersection-probability sA sB)
  (limit (/ sB sA) 0 1))

(define (conditional-probability-consistency sA sB sAB)
  (and (< 0 sA)
       (<= (smallest-intersection-probability sA sB) sAB)
       (<= sAB (largest-intersection-probability sA sB))))

;; Declaration (assumes Q is a conjunction)
(define back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule"))
(DefineLink back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule-name
  back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule)

;; Declarations (assumes Q is an evaluation)
(define back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule"))
(DefineLink back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule-name
  back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule)
