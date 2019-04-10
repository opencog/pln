from pln.common import *
import os


def deduction_formula(conclusion, premises):
    print('vasya')
    import pdb;pdb.set_trace()
    if len(premises) == 2:
        AC = conclusion
        AB = premises[0]
        BC = premises[1]
        sA = gar(AB).tv.strength
        cA = gar(AB).tv.confidence
        sB = gar(BC).tv.strength
        cB = gar(BC).tv.confidence
        sC = gdr(BC).tv.strength
        cC = gdr(BC).tv.confidence
        sAB = AB.tv.strength
        cAB = AB.tv.confidence
        sBC = BC.tv.strength
        cBC = BC.tv.confidence
        alpha = 0.9 # how much confidence is lost at each deduction step

        # Hacks to overcome the lack of distributional TV. If s=1
        # and c=0, then assign s to the mode value satisfying the
        # deduction consistency constraint (what a pain, let's use
        # 0.25 for now).
        sA = 0.25 if (0.99 < sA) and (cA <= 0) else sA
        sB = 0.25 if (0.99 < sB) and (cB <= 0) else sB
        sC = 0.25 if (0.99 < sC) and (cC <= 0) else sC
        if all(
             any(0 == cA, 0 == cB, 0 == cAB,
                 conditional_probability_consistency(sA, sB, sAB)),
             any(0 == cB, 0 == cC, 0 == cBC,
                 conditional_probability_consistency(sB, sC, sBC))):
            if 0.99 < sB * cB:
                # Hack to overcome for the lack of distributional
                # TV. This covers the case where B fully confidently
                # tends to 1. See formulas.scm Simple Deduction
                # Formula comment for more explanations. This
                # overlaps with the implication-introduction-rule.
                sAC = sC
                cAC = alpha * cA * cC
                if all(1e-8 < sAC, 1e-8 < cAC):  # Don't create zero
                                               # knowledge. Note that
                                               # sAC == 0 is not zero
                                               # knowledge but it's
                                               # annoying in the
                                               # current hacky
                                               # situation.
                      cog_merge_hi_conf_tv(AC, TTruthValue(sAC, cAC))
                # Branch if sB * cB <= 0.99
                if any(0.99 < sAB * cAB, 0.99 < sBC * cBC):
                     # Hack to overcome for the lack of
                     # distributional TV. This covers the case
                     # where little is known about A and B
                     # (i.e. their strength is meaningless), yet
                     # we can confidently calculate sAC because
                     # sAB and sBC are so high anyway.
                     sAC = sAB * sBC
                     # Otherwise fall back on the naive formula
                else:
                     sAC = simple_deduction_strength_formula(sA, sB, sC, sAB,sBC)
                cAC = min(cAB, cBC)
                # Unless the 2 implication are fully confident
                # decrease the confidence by some factor. I'm not
                # sure how justify this for now, it's perhaps a
                # bad hack.
                cAC = (alpha if cAC < 0.99 else 1.0) * cAC
                if all(1e-8 < sAC, 1e-8 < cAC): # Don't create zero
                                                # knowledge. Note that
                                                # sAC == 0 is not zero
                                                # knowledge but it's
                                                # annoying in the
                                                # current hacky
                                                # situation.
                    cog_merge_hi_conf_tv(AC, TTruthValue(sAC, cAC))
    return conclusion


def add_members(rule_base):
    atomspace = get_type_ctor_atomspace()
    # add current
    scheme_eval(atomspace, '(add-to-load-path "{0}")'.format(os.path.dirname(__file__)))

    # load files
    for f in ('deduction.scm',):
        scheme_eval(atomspace, '(load-from-path "' + f + '")')
    # add to rule base
    MemberLink(DefinedSchemaNode("deduction-inheritance-rule"), rule_base)
    MemberLink(DefinedSchemaNode("deduction-implication-rule"), rule_base)
    MemberLink(DefinedSchemaNode("deduction-subset-rule"), rule_base)
