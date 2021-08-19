(use-modules (opencog ure))


(define (make-rb rule_base)
    (MemberLink (DefinedSchemaNode "modus-ponens-inheritance-rule") rule_base)
    (MemberLink (DefinedSchemaNode "modus-ponens-implication-rule") rule_base)
    (MemberLink (DefinedSchemaNode "modus-ponens-subset-rule")  rule_base)
    (MemberLink (DefinedSchemaNode "contraposition-implication-rule") rule_base)
    (MemberLink (DefinedSchemaNode "contraposition-inheritance-rule") rule_base)
)

(cog-set-tv! (ConceptNode "apple") (stv 0.8 0.9))
(InheritanceLink (stv 0.8 0.4)
		 (ConceptNode "apple")
                 (ConceptNode "fruit"))
(define rbs (ConceptNode "pln"))
(define target (ConceptNode "fruit"))
(make-rb rbs)
;(cog-bc rbs target)
