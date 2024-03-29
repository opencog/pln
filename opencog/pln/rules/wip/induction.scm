;; =============================================================================
;; InductionRule
;;
;; LinkType
;;   A
;;   B
;; LinkType
;;   A
;;   C
;; |-
;; LinkType
;;   B
;;   C
;;
;; Due to pattern matching issues, currently the file has been divided into 3
;; parts, each pertaining to different links. The rules are :-
;;       induction-inheritance-rule
;;       induction-implication-rule
;;       induction-subset-rule
;;
;; -----------------------------------------------------------------------------

(load "formulas.scm")

(define induction-inheritance-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (AndLink
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$C"))
            (NotLink
                (IdenticalLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: induction-formula")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))))
    
(define induction-implication-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (AndLink
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$C"))
            (NotLink
                (IdenticalLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: induction-formula")
            (ListLink
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (ImplicationLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))))

(define induction-subset-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (AndLink
            (SubsetLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (SubsetLink
                (VariableNode "$A")
                (VariableNode "$C"))
            (NotLink
                (IdenticalLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: induction-formula")
            (ListLink
                (SubsetLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (SubsetLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (SubsetLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))))

(define (induction-formula AB AC BC)
    (define A (gar AB))
    (define B (gdr AB))
    (define C (gdr AC))
    (let
        ((sA (cog-mean A))
         (cA (cog-confidence A))
         (sB (cog-mean B))
         (cB (cog-confidence B))
         (sC (cog-mean C))
         (cC (cog-confidence C))
         (sAB (cog-mean AB))
         (cAB (cog-confidence AB))
         (sAC (cog-mean AC))
         (cAC (cog-confidence AC)))
        (cog-set-tv!
            BC
            (stv 
                (simple-deduction-strength-formula sB sA sC (inversion-strength-formula sAB sA sB) sAC) 
                (min cAB cAC)))))
                
;; =============================================================================

;; Name the rules
(define induction-inheritance-rule-name (DefinedSchemaNode "induction-inheritance-rule"))
(DefineLink induction-inheritance-rule-name induction-inheritance-rule)

(define induction-implication-rule-name (DefinedSchemaNode "induction-implication-rule"))
(DefineLink induction-implication-rule-name induction-implication-rule)

(define induction-subset-rule-name (DefinedSchemaNode "induction-subset-rule"))
(DefineLink induction-subset-rule-name induction-subset-rule)
