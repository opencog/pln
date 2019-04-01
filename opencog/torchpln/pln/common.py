import weakref
import torch
from opencog.atomspace import types, PtrValue, Atom
from opencog.scheme_wrapper import scheme_eval
from opencog.type_constructors import *


MEAN = 0
CONFIDENCE = 1

class TTruthValue(torch.Tensor):
    @staticmethod
    def __new__(cls, *args):
        if len(args) == 1:
            assert(len(args[0]) == 2)
            instance = super().__new__(cls, *args)
        elif len(args) == 2:
            instance = super().__new__(cls, args)
        else:
            raise RuntimeError("Expecting tuple of two number, \
                    tensor of len 2 or two numbers, got {0}".format(args))
        instance._stv = None
        return instance

    @property
    def mean(self):
        return self[MEAN]

    @property
    def confidence(self):
        return self[CONFIDENCE]

    @property
    def stv(self):
        raise NotImplementedError("working with stv is not implemented")

    @stv.setter
    def stv(self, stv):
        raise NotImplementedError("setting stv requires weak reference support\n \
                for simple truth value type")

    def __update_stv(self, *args):
        self._stv.mean = self.mean
        self._stv.confidence = self.confidence


def get_ttv(atom):
    key = atom.atomspace.add_node(types.PredicateNode, "cogNet-tv")
    value = atom.get_value(key)
    if value is None:
        default = TTruthValue([atom.tv.mean, atom.tv.confidence])
        set_ttv(atom, default)
        value = default
    else:
        value = value.value()
    return value


def set_ttv(atom, value):
    key = atom.atomspace.add_node(types.PredicateNode, "cogNet-tv")
    if not isinstance(value, TTruthValue):
        value = TTruthValue(value)
    atom.set_value(key, PtrValue(value))
    atom.tv = TruthValue(value.mean, value.confidence)


def gt_zero_confidence(atom):
    tensor_tv = get_ttv(atom)
    result = TruthValue(0 < tensor_tv.confidence, 1)
    return result


def cog_merge_hi_conf_tv(atom, tv):
    old_tv = get_ttv(atom)
    if old_tv.confidence < tv.confidence:
        set_ttv(atom, tv)
        atom.tv = TruthValue(float(tv.mean), float(tv.confidence))
