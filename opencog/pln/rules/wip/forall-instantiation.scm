;; =======================================================================
;; ForAll Instantiation rule
;; (TODO add wiki page)
;;
;; ForAllLink
;;    V
;;    B
;; T
;; |-
;; B[V->T]
;;
;; where V is a variable, B is a body containing V, T is an atom to
;; substitute and B[V->T] is B where V has been substituted by T.
;;
;; As currently implemented T is not explicitely in the
;; premises. Instead it is queried directly by the rule's formula.
;;
;; Warning: the forall instantiation doesn't cleverly select the
;; instances that satisfy a precondition, if you have an
;; ImplicationLink inside your ForAllLink you probably want to use
;; Implication Instantiation on your ImplicationLink directly
;; (see implication-instantiation.scm).
;; -----------------------------------------------------------------------

(use-modules (srfi srfi-1))

(use-modules (opencog exec))
(use-modules (opencog logger))

(load-from-path "opencog/pln/rules/wip/instantiation.scm")

;;;;;;;;;;;;;;;;;;;;;;;
;; Helper definition ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define forall-total-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableList")))
     (VariableNode "$B")))

(define forall-instantiation-body
  (QuoteLink (ForAllLink
     (UnquoteLink (VariableNode "$TyVs"))
     (UnquoteLink (VariableNode "$B")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forall total instantiation rule ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define forall-total-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: forall-total-instantiation-formula")
     (ListLink
        (VariableNode "$TyVs")
        (VariableNode "$B"))))

;; Only try to match a ForAllLink with a type restricted variable in
;; the ForAllLink variable definition. The choice of the substitution
;; term is done randomly in forall-total-instantiation-formula. All
;; scoped variables are instantiated.
(define forall-total-instantiation-rule
  (BindLink
     forall-total-instantiation-variables
     forall-instantiation-body
     forall-total-instantiation-rewrite))

;; This function
;;
;; 1. randomly selects a substitution term (or a tuple of substitution
;;    terms, if the ForAll has multiple variables in scope),
;;
;; 2. performs the substitution,
;;
;; 3. calculates its TV (TODO: just <1 1> for now).
(define (forall-total-instantiation-formula SV B)
  (cog-set-tv!
   (let* (
          (SV-type (cog-type SV))
                                        ; Build a list of all type
                                        ; contrained variables
                                        ;
                                        ; There is only one type
                                        ; constrained variable
          (terms (cond ((cog-subtype? 'TypedVariableLink SV-type)
                        (select-substitution-term SV))
                                        ; there are multiple type
                                        ; constrained variables
                       ((cog-subtype? 'VariableList SV-type)
                        (select-substitution-terms SV))
                       (cog-logger-error
                        (string-append "Wrong type for ~a, "
                                       "should be a TypedVariableLink "
                                       "or a VariableList") SV))))
     
     ;; Substitute the variable by the term in the body
     (cog-execute! (PutLink (LambdaLink SV B) terms)))
   (stv 1 1)))

;; Name the rule
(define forall-total-instantiation-rule-name
  (DefinedSchemaNode "forall-total-instantiation-rule"))
(DefineLink forall-total-instantiation-rule-name
  forall-total-instantiation-rule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forall partial instantiation rule ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define forall-partial-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeNode "VariableList"))
     (VariableNode "$B")))

(define forall-partial-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: forall-partial-instantiation-formula")
     (ListLink
        (VariableNode "$TyVs")
        (VariableNode "$B"))))

;; Like forall-total-instantiation-rule but only instantiate one
;; variable amonst a variable list (if there is just one variable in
;; the forall scope, then this rule will not be invoked).
(define forall-partial-instantiation-rule
  (BindLink
     forall-partial-instantiation-variables
     forall-instantiation-body
     forall-partial-instantiation-rewrite))

;; This function
;;
;; 1. randomly selects a substitution term,
;;
;; 2. performs the substitution,
;;
;; 3. calculates its TV (TODO: just <1 1> for now).
(define (forall-partial-instantiation-formula TyVs B)
  (cog-set-tv!
   (let* (
          (TyV-and-remain (select-rm-rnd-outgoing TyVs))
          (TyV (car TyV-and-remain))
          (TyVs-remain (cadr TyV-and-remain))
          (TyVs-remain-len (length (cog-outgoing-set TyVs-remain)))
                                        ; Select the term to substitute
          (term (select-substitution-term TyV))
                                        ; Substitute the variable by
                                        ; the term in the body
          (B-inst (cog-execute! (PutLink (LambdaLink TyV B) term)))
                                        ; If there is only one
                                        ; variable left, discard the
                                        ; VariableLink
          (TyVs-remain (if (= TyVs-remain-len 1)
                           (gar TyVs-remain)
                           TyVs-remain)))
     (if (> TyVs-remain-len 0)
                                        ; If there are some variables
                                        ; left, add the ForAllLink
                                        ; back
         (ForAllLink TyVs-remain B-inst)
                                        ; Otherwise just return the
                                        ; body instance
         B-inst))
   ;; TODO: implement a probabilistic formula rather than <1 1>
   (stv 1 1)))

;; Name the rule
(define forall-partial-instantiation-rule-name
  (DefinedSchemaNode "forall-partial-instantiation-rule"))
(DefineLink forall-partial-instantiation-rule-name
  forall-partial-instantiation-rule)
