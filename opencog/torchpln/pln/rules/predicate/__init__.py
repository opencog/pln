from pln.common import *
import os


def fuzzy_conjunction_introduction_formula(conj, conj_set):
    atoms = conj_set.out
    tvs = list(get_ttv(x) for x in conj_set.out)
    min_s = torch.min(torch.stack(tuple(x.mean for x in tvs)))
    min_c = torch.min(torch.stack(tuple(x.confidence for x in tvs)))
    result = TTruthValue(torch.stack([min_s, min_c]))
    cog_merge_hi_conf_tv(conj, result)
    return conj


def precise_modus_ponens_strength_formula(sA, sAB, snotAB):
    return sAB * sA + snotAB * (1 - sA)


def modus_ponens_formula(B, AB, A):
    sA = get_ttv(A).mean
    cA = get_ttv(A).confidence
    sAB = get_ttv(AB).mean
    cAB = get_ttv(AB).confidence
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
        sABC = get_ttv(ABC).mean
        cABC = get_ttv(ABC).confidence
        sAC = get_ttv(AC).mean
        cAC = get_ttv(AC).confidence
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
    if (0.999 < get_ttv(PQ).mean) < (0.999 < get_ttv(PQ).confidence):
        return TruthValue(1.0, 1.0)
    return TruthValue(0.0, 1.0)


def contraposition_formula(conclusion, *premises):
    if len(premises) == 3:
        NBNA = conclusion
        AB = premises[0]
        A = premises[1]
        B = premises[2]
        sAB = get_ttv(AB).mean
        cAB = get_ttv(AB).confidence
        sA = get_ttv(A).mean
        cA = get_ttv(A).confidence
        sB = get_ttv(B).mean
        cB = get_ttv(B).confidence

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
        sPQ = get_ttv(PQ).mean
        cPQ = get_ttv(PQ).confidence
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


# Given List of values, wrap a Quote link around each element of that list
def quote_values(values):
    values_lst = values.out
    quoted-values-lst = [QuoteLink(x) for x in values_lst]
    return ListLink(quoted_values_lst)


# Given a list of values and a lambda link generate a list of terms
# as the results of beta reductions of values within the lambda. We
# can't just execute a put link because the result will be a set link
# and we need to preserve the order.
def map_beta_reduce(lambda_link, values):
    for v in values:
        execute_atom(PutLink(lambda_link, v))


def conditional_direct_evaluation_implication_scope_formula(I):
    out = I.out
    arity = len(out)
    vardecl = False if arity == 2 else out[0]
    antecedent = out[0 if arity == 2 else 1]
    consequent = out[1 if arity == 2 else  2]

    # Fetch all antecedent values
    antecedent_get = GetLink(vardecl, antecedent)
    antecedent_result = execute_atom(I.atomspace, antecedent_get)
    antecedent_values = antecedent_result.out

    # Possibly wrap the values with Quote, not sure that is right though
    antecedent_quoted_values = [quote_values(x) for x in antecedent_values]
    # Generate the antecedent and consequent terms
    antecedent_lambda = LambdaLink(vardecl, antecedent)
    consequent_lambda = LambdaLink(vardecl, consequent)
    antecedent_terms = map_beta_reduce(antecedent_lambda, antecedent_values)
    consequent_terms = map_beta_reduce(consequent_lambda, antecedent_values)

    # Calculate the TV based on the evidence
    tv = evidence_tv(antecedent_terms, consequent_terms)
    if 0.0 < tv.confidence:
        cog_merge_hi_conf_tv(I, tv)

# Given a list of antecedent and consequent terms calculate the TV of
# the implication
def evidence_tv(antecedent_terms, consequent_terms):
   # TODO replace by a distributional TV based calculation.
   K = 800  # parameter to convert from count to confidence
   def true_enough(A):
       TV = get_ttv(A)
       s = TV.mean
       c = TV.confidence
       return (s > 0.5) and (c > 0)

   both_true_enough = lambda pair: all([true-enough(x) for x in pair])
   true_enough_antecedent_terms =  [x for x in antecedent-terms if true-enough(x)]
   ant_con_pairs = antecedent_terms, consequent_terms
   true_enough_inter_terms = [x for x in ant_con_pairs if both_true_enough(x)]
   antecedent_length = len(true_enough_antecedent_terms)
   inter_length = len(true_enough_inter_terms)
   strength = (inter_length / antecedent_length) if 0 < antecedent_length else 0
   confidence = antecedent_length / K
   return TTruthValue(strength, confidence)

