;; Knowledge base for PLNUTest::test_modus_ponens()

(ConceptNode "apple" (stv 0.8 0.9))
(InheritanceLink (stv 0.8 0.4)
                 (ConceptNode "apple")
                 (ConceptNode "fruit"))

(define target (ConceptNode "fruit"))
