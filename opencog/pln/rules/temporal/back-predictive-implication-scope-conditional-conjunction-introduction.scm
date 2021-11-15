;; BackPredictiveImplicationScope Conditional Conjuntion Introduction Rule
;;
;; This rule is similar to a conjunction introduction rule with an
;; extra condition (more specifically the antecedent of a predictive
;; implication).  Its compact notation is:
;;
;; P↝Q
;; P↝R
;; ⊢
;; P↝(Q∧R)
;;
;; Its Atomese notation is:
;;
;; BackPredictiveImplicationScope <TV1>
;;   V
;;   T
;;   P
;;   Q
;; BackPredictiveImplicationScope <TV2>
;;   V
;;   T
;;   P
;;   R
;; |-
;; BackPredictiveImplicationScope <TV>
;;   V
;;   T
;;   P
;;   And
;;      Q
;;      R
;;
;; where TV is calculated using TV1 and TV2 (their product assuming
;; P↝Q and P↝R are independent).

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

(define back-predictive-implication-scope-conditional-conjunction-introduction-rule
  (let* ((V (Variable "$V"))
     (T (Variable "$T"))
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     (R (Variable "$R"))
     (NaturalT (TypeInh 'NaturalLink))
     (VardeclT (TypeChoice
        (TypeInh 'VariableNode)
        (Type 'VariableSet)
        (Type 'VariableList)
        (Type 'TypedVariableLink)))
     (P↝Q (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T)
              (Unquote P)
              (Unquote Q))))
     (P↝R (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T)
              (Unquote P)
              (Unquote R))))
     (Q∧R (And Q R))
     (P↝Q∧R (Quote
              (BackPredictiveImplicationScope
                (Unquote V)
                (Unquote T)
                (Unquote P)
                (Unquote Q∧R)))))
  (Bind
    (VariableSet
      (TypedVariable V VardeclT)
      (TypedVariable T NaturalT)
      P
      Q
      R)
    (And
      (Present P↝Q P↝R)
      (Not (Identical Q R))
      (EvaluationLink
        (GroundedPredicate "scm: check_preconditions")
        (List
          Q
          R)
      )
    )
    (ExecutionOutput
      (GroundedSchema "scm: back-predictive-implication-scope-conditional-conjunction-introduction")
      (List
        ;; Conclusion
        P↝Q∧R
        ;; Premises
        (Set
          P↝Q
          P↝R))))))

(define (check_preconditions Q R)
  (define (andlink? atom)
    (equal? (cog-type atom) 'AndLink))

  (if (or (and (andlink? Q) (member R (cog-outgoing-set Q)))
          (and (andlink? R) (member Q (cog-outgoing-set R))))
    (stv 0 1)
    (stv 1 1)))

;; Formula
(define (back-predictive-implication-scope-conditional-conjunction-introduction conclusion . premises)
  (cog-logger-fine "(back-predictive-implication-scope-conditional-conjunction-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((premises (car premises))
        (P↝Q (gar premises))
        (P↝R (gdr premises))
        (sP↝Q (cog-mean P↝Q))
        (cP↝Q (cog-confidence P↝Q))
        (sP↝R (cog-mean P↝R))
        (cP↝R (cog-confidence P↝R))
        ;; This code:
        ;;
        ;;  (sP↝Q∧R (* sP↝Q sP↝R))
        ;;  (cP↝Q∧R (min cP↝Q cP↝R))
        ;;  (tv (stv sP↝Q∧R cP↝Q∧R)))
        ;; (if (< 0 cP↝Q∧R)
        ;;     (cog-merge-hi-conf-tv! conclusion tv)))))
        ;;
        ;; leads to the following warning:
        ;;
        ;;; WARNING: compilation of /home/nilg/Work/OpenCog/pln/opencog/pln/rules/temporal/back-predictive-implication-scope-conditional-conjunction-introduction.scm failed:
        ;;; Throw to key `decoding-error' with args `("scm_from_utf8_stringn" "input locale conversion error" 22 #vu8(157 81 226 136 167 82))'.
        ;;
        ;; Just to be cautious it has been ASCII-fied for now
        (sPQR (* sP↝Q sP↝R))
        (cPQR (min cP↝Q cP↝R))
        (tv (stv sPQR cPQR)))
       (if (< 0 cPQR)
           (cog-merge-hi-conf-tv! conclusion tv)))))

;; Declaration
(define back-predictive-implication-scope-conditional-conjunction-introduction-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-conditional-conjunction-introduction-rule"))
(DefineLink back-predictive-implication-scope-conditional-conjunction-introduction-rule-name
  back-predictive-implication-scope-conditional-conjunction-introduction-rule)
