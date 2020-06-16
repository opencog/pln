import unittest
import torch

from opencog.ure import BackwardChainer
from opencog.atomspace import AtomSpace, types
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from pln import  TTruthValue
from pln import get_ttv, set_ttv
from pln.rules import propositional
from opencog.bindlink import execute_atom, evaluate_atom


def init_pln():
    rule_base = ConceptNode('PLN')
    propositional.add_members(rule_base)
    ExecutionLink(SchemaNode("URE:maximum-iterations"),
                  rule_base,
                  NumberNode('100'))
    return rule_base



class TestBasic(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)
        init_pln()

    def test_modus_ponens(self):
        rule_base = ConceptNode('PLN')
        set_ttv(ConceptNode("apple"), TTruthValue((0.8, 0.9)))
        inh = InheritanceLink(ConceptNode("apple"),
                ConceptNode("fruit"))
        set_ttv(inh, TTruthValue(0.8, 0.4))
        bc = BackwardChainer(self.atomspace, rule_base, ConceptNode("fruit"))
        bc.do_chain()
        result = get_ttv(bc.get_results().out[0])
        self.assertTrue(abs(0.68 - float(result.mean)) < 0.0001)

    def test_fuzzy_conjunction(self):
        rule_base = ConceptNode('PLN')
        apple = ConceptNode('apple')
        inh_green = InheritanceLink(PredicateNode("green"), ConceptNode("color"))
        set_ttv(inh_green, torch.tensor([0.96, .9]))
        ev_green = EvaluationLink(PredicateNode('green'), apple)
        set_ttv(ev_green, TTruthValue(0.85, 0.95))
        conj = AndLink(ev_green, inh_green)
        bc = BackwardChainer(self.atomspace, rule_base, conj)
        bc.do_chain()
        result = get_ttv(bc.get_results().out[0])
        self.assertTrue(abs(result.mean - 0.85) < 0.0001)
        self.assertTrue(abs(result.confidence - 0.9) < 0.0001)

    def test_cons_disj_elimination(self):
        rule_base = ConceptNode('PLN')
        A = PredicateNode('A')
        set_ttv(A, (1.0, 1.0))
        B = PredicateNode('B')
        set_ttv(B, (1.0, 1.0))
        C = PredicateNode('C')
        set_ttv(C, (1.0, 1.0))
        set_ttv(ImplicationLink(A, C), (0.55, 0.55))
        set_ttv(ImplicationLink(A,
                        OrLink(B, C)), (1.0, 1.0))

        impl = ImplicationLink(A, B)
        bc = BackwardChainer(self.atomspace, rule_base, impl)
        bc.do_chain()
        result = get_ttv(bc.get_results().out[0])
        self.assertTrue(result.mean == 1.0)
        alpha = 0.9
        self.assertTrue(result.confidence == alpha * 0.55)

    def test_contraposition(self):
        rule_base = ConceptNode('PLN')
        A = PredicateNode('A')
        set_ttv(A, (1.0, 1.0))
        B = PredicateNode('B')
        set_ttv(B, (1.0, 1.0))
        set_ttv(ImplicationLink(A, B), (1.0, 1.0))
        NBNA = ImplicationLink(NotLink(B), NotLink(A))
        bc = BackwardChainer(self.atomspace, rule_base, NBNA)
        bc.do_chain()
        result = bc.get_results().out[0]
        self.assertTrue(0 < get_ttv(result).confidence,
                'fails due to https://github.com/opencog/opencog/issues/3465')

    def tearDown(self):
        self.atomspace = None
        finalize_opencog()
