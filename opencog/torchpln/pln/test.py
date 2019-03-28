import unittest
import torch

from opencog.ure import BackwardChainer
from opencog.atomspace import AtomSpace, types
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from cognets import CogModule, CogModel, InputModule, InheritanceModule, get_value, TTruthValue
from cognets import get_tv, set_tv
from pln.rules import propositional

import __main__
__main__.CogModule = CogModule


RED = 0
GREEN = 1
BLUE = 2

def init_pln():
    rule_base = ConceptNode('PLN')
    propositional.add_members(rule_base)
    ExecutionLink(SchemaNode("URE:maximum-iterations"),
                  rule_base,
                  NumberNode('20'))
    return rule_base


class GreenPredicate(CogModule):

    def forward(self, x):
        """
        extract green channel and shift it above zero
        """
        mean = x.mean(dim=-1).mean(dim=-1)
        return mean[GREEN]


class TestBasic(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        self.model = CogModel(self.atomspace)
        initialize_opencog(self.atomspace)
        init_pln()

    def test_modus_ponens(self):
        rule_base = ConceptNode('PLN')
        set_tv(ConceptNode("apple"), TTruthValue((0.8, 0.9)))
        inh = InheritanceLink(ConceptNode("apple"),
                ConceptNode("fruit"))
        set_tv(inh, TTruthValue((0.8, 0.4)))
        bc = BackwardChainer(self.atomspace, rule_base, ConceptNode("fruit"))
        bc.do_chain()
        result = get_tv(bc.get_results().out[0])
        self.assertTrue(abs(0.68 - float(result.mean)) < 0.0001)

    def tearDown(self):
        self.atomspace = None
        self.model = None
        finalize_opencog()

