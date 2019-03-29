import unittest
import torch

from opencog.ure import BackwardChainer
from opencog.atomspace import AtomSpace, types
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from pln import  TTruthValue
from pln import get_tv, set_tv
from pln.rules import propositional


def init_pln():
    rule_base = ConceptNode('PLN')
    propositional.add_members(rule_base)
    ExecutionLink(SchemaNode("URE:maximum-iterations"),
                  rule_base,
                  NumberNode('75'))
    return rule_base



class TestBasic(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)
        init_pln()

    def test_modus_ponens(self):
        rule_base = ConceptNode('PLN')
        set_tv(ConceptNode("apple"), TTruthValue((0.8, 0.9)))
        inh = InheritanceLink(ConceptNode("apple"),
                ConceptNode("fruit"))
        set_tv(inh, TTruthValue(0.8, 0.4))
        bc = BackwardChainer(self.atomspace, rule_base, ConceptNode("fruit"))
        bc.do_chain()
        result = get_tv(bc.get_results().out[0])
        self.assertTrue(abs(0.68 - float(result.mean)) < 0.0001)

    def test_fuzzy_conjunction(self):
        rule_base = ConceptNode('PLN')
        apple = ConceptNode('apple')
        inh_green = InheritanceLink(PredicateNode("green"), ConceptNode("color"))
        set_tv(inh_green, torch.tensor([0.96, .9]))
        ev_green = EvaluationLink(PredicateNode('green'), apple)
        set_tv(ev_green, TTruthValue(0.85, 0.95))
        conj = AndLink(ev_green, inh_green)
        bc = BackwardChainer(self.atomspace, rule_base, conj)
        bc.do_chain()
        result = get_tv(bc.get_results().out[0])
        self.assertTrue(abs(result.mean - 0.85) < 0.0001)
        self.assertTrue(abs(result.confidence - 0.9) < 0.0001)

    def tearDown(self):
        self.atomspace = None
        self.model = None
        finalize_opencog()

