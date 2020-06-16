import weakref
import torch
from opencog.atomspace import types, PtrValue, Atom
from opencog.scheme_wrapper import scheme_eval
from opencog.type_constructors import *
from opencog.atomspace import TensorTruthValue as TTruthValue


def get_ttv(atom):
    value = atom.tv
    if not isinstance(value, TTruthValue):
        default = TTruthValue([atom.tv.mean, atom.tv.confidence])
        atom.tv = default
        value = default
    return value.torch()


def set_ttv(atom, value):
    if not isinstance(value, TTruthValue):
        value = TTruthValue(value)
    atom.tv = value
    return atom


def gt_zero_confidence(atom):
    tensor_tv = get_ttv(atom)
    result = TruthValue(1 if 0 < tensor_tv.confidence else 0, 1)
    return result


def cog_merge_hi_conf_tv(atom, tv):
    old_tv = get_ttv(atom)
    if old_tv.confidence < tv.confidence:
        set_ttv(atom, tv)


def gar(atom):
    if atom.out:
        return atom.out[0]
    return None


def gdr(atom):
    """
    Return second element of outgoing list
    """
    if len(atom.out) < 2:
        return None
    return atom.out[1]
