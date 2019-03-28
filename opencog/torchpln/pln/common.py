import torch
from opencog.atomspace import types, PtrValue, Atom
from opencog.scheme_wrapper import scheme_eval
from opencog.type_constructors import *


MEAN = 0
CONFIDENCE = 1

class TTruthValue(torch.Tensor):

    @property
    def mean(self):
        return self[MEAN]

    @property
    def confidence(self):
        return self[CONFIDENCE]


def get_tv(atom):
    key = atom.atomspace.add_node(types.PredicateNode, "cogNet-tv")
    value = atom.get_value(key)
    if value is None:
        default = TTruthValue([atom.tv.mean, atom.tv.confidence])
        set_tv(atom, default)
        value = default
    else:
        value = value.value()
    return value


def set_tv(atom, value, tv=False):
    key = atom.atomspace.add_node(types.PredicateNode, "cogNet-tv")
    assert isinstance(value, TTruthValue)
    atom.set_value(key, PtrValue(value))


def gt_zero_confidence(atom):
    tensor_tv = get_tv(atom)
    result = TruthValue(0 < tensor_tv.confidence, 1)
    return result


def cog_merge_hi_conf_tv(atom, tv):
    old_tv = get_tv(atom)
    if old_tv.confidence < tv.confidence:
        set_tv(atom, tv)
        atom.tv = TruthValue(float(tv.mean), float(tv.confidence))
