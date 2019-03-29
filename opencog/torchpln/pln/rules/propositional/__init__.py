from pln.common import *
import os


def fuzzy_conjunction_introduction_formula(conj, conj_set):
    atoms = conj_set.out
    tvs = list(get_tv(x) for x in conj_set.out)
    min_s = torch.min(torch.stack(tuple(x.mean for x in tvs)))
    min_c = torch.min(torch.stack(tuple(x.confidence for x in tvs)))
    result = TTruthValue(torch.stack([min_s, min_c]))
    cog_merge_hi_conf_tv(conj, result)
    return conj


def precise_modus_ponens_strength_formula(sA, sAB, snotAB):
    return sAB * sA + snotAB * (1 - sA)


def modus_ponens_formula(B, AB, A):
    sA = get_tv(A).mean
    cA = get_tv(A).confidence
    sAB = get_tv(AB).mean
    cAB = get_tv(AB).confidence
    snotAB = 0.2 # Huge hack
    cnotAB = 1
    new_tv = TTruthValue((precise_modus_ponens_strength_formula(sA, sAB, snotAB),
                min(cAB, cnotAB, cA)))
    cog_merge_hi_conf_tv(B, new_tv)
    return B


def consequent_disjunction_elimination_formula(conclusion, *premises):
    if len(premises) == 2:
        ABC = premises[0]
        AC = premises[1]
        sABC = get_tv(ABC).mean
        cABC = get_tv(ABC).confidence
        sAC = get_tv(AC).mean
        cAC = get_tv(AC).confidence
        alpha = 0.9 # Confidence-loss
                    # coefficient. TODO replace by
                    # something more meaningful
        AB = conclusion
        precondition = (sAC <= sABC) and (sAC < 1)
        sAB = ((sABC - sAC) / (1 - sAC)) if precondition else 1
        cAB = (alpha * min(cABC, cAC)) if precondition else 0
        if 0 < cAB:
            cog_merge_hi_conf_tv(AB, TTruthValue(sAB, cAB))
        return AB


def crisp_contraposition_scope_precondition(PQ):
    if (0.999 < get_tv(PQ).mean) < (0.999 < get_tv(PQ).confidence):
        return TruthValue(1.0, 1.0)
    return TruthValue(0.0, 1.0)


def contraposition_formula(conclusion, *premises):
    if len(premises) == 3:
        NBNA = conclusion
        AB = premises[0]
        A = premises[1]
        B = premises[2]
        sAB = get_tv(AB).mean
        cAB = get_tv(AB).confidence
        sA = get_tv(A).mean
        cA = get_tv(A).confidence
        sB = get_tv(B).mean
        cB = get_tv(B).confidence

    if (0.999 < sAB) and (0.999 < cAB):
        cog_merge_hi_conf_tv(NBNA, TTruthValue(sAB, cAB))
        if 1 > sB:
            sNBNA = sum(1, -sA, -sB, sAB * sA) / (1 - sB)
            cNBNA = min(cAB, cA, cB)
            cog_merge_hi_conf_tv(NBNA, TTruthValue(sNBNA, cNBNA))
    return conclusion

def crisp_contraposition_scope_formula(conclusion, *premises):
    if len(premises) == 1:
        NQNP = conclusion
        PQ = premises[0]
        sPQ = get_tv(PQ).mean
        cPQ = get_tv(PQ).confidence
        if (0.999 < sPQ) and (0.999 < cPQ):
            cog_merge_hi_conf_tv(NQNP, TTruthValue(sPQ, cPQ))
        return NQNP


def add_members(rule_base):
    atomspace = get_type_ctor_atomspace()
    # add current
    scheme_eval(atomspace, '(add-to-load-path "{0}")'.format(os.path.dirname(__file__)))

    # load files
    for f in ('consequent-disjunction-elimination.scm', 'fuzzy-disjunction-introduction.scm',
              'modus-ponens.scm', 'contraposition.scm', 'fuzzy-conjunction-introduction.scm'):
        scheme_eval(atomspace, '(load-from-path "' + f + '")')

    # modus-ponens
    MemberLink(DefinedSchemaNode("modus-ponens-inheritance-rule"), rule_base)
    MemberLink(DefinedSchemaNode("modus-ponens-implication-rule"), rule_base)
    MemberLink(DefinedSchemaNode("modus-ponens-subset-rule"), rule_base)
    # disjunction & fuzzy-conjunction
    for i in range(1, 6):
        MemberLink(DefinedSchemaNode("fuzzy-disjunction-introduction-{0}ary-rule".format(i)), rule_base)
        MemberLink(DefinedSchemaNode("fuzzy-conjunction-introduction-{0}ary-rule".format(i)), rule_base)
    # consequent elimination
    MemberLink(DefinedSchemaNode("consequent-disjunction-elimination-inheritance-rule"), rule_base)
    MemberLink(DefinedSchemaNode("consequent-disjunction-elimination-implication-rule"), rule_base)

    MemberLink(DefinedSchemaNode("crisp-contraposition-implication-scope-rule"), rule_base)
    MemberLink(DefinedSchemaNode("contraposition-implication-rule"), rule_base)
    MemberLink(DefinedSchemaNode("contraposition-inheritance-rule"), rule_base)
