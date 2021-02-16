# Temporal Reasoning with PLN

In this document we provide the foundation for doing Temporal
Reasoning with PLN.  It builds upon Chapter 14 of the PLN book, using
the same notations as well as explicitely defining a Probability Space
for it, based upon https://en.wikipedia.org/wiki/Stochastic_process.

## Notations

To simplify let's define the following to notations

### PredictiveImplication

```
PredictiveImplication
  T
  P
  Q
```

is denoted as

```
P↝ᵀQ
```

### SequentialAnd

```
SequentialAnd
  T
  P
  Q
```

is denoted as

```
P≺ᵀQ
```

## Semantics of PredictiveImplication

Let `{Xₜ}` be a stochastic process, indexed by time `t`, continuous or
discrete, with state space `S`, the space of atomspaces. Thus `{Xₜ}`
is a sequence of atomspaces, i.e. states (or subjective states) of an
agent across time.

In the context of expressing the semantics of predictive implications
using such notation it should be clear that such predictive
implications do not wholly represent the true distribution but rather
partial models attempting to approach it.  This may be important in
order to justify some assumptions made below.

Let's consider a generic predictive implication

```
PredictiveImplication <TV>
  T
  P
  Q
```

which informally means, that if `P(x)` is true (possibly to some
degree, if fuzzy), at some time `t`, then `Q(x)` will be true (to some
degree), at time `t + T` with probability described by `TV`.

Akin to how the time-homogenous property is used to define Markov
Processes [https://en.wikipedia.org/wiki/Markov_chain#Variations],
let's assume that such predictive implication is time and parameter
homogeneous (note that we ignore fuzzy membership, but it does not
result in lose of generality, in so far as fuzziness can be modeled
with probabilities), which gives us the following definition:

```
∀t,x Pr(Xᵤ ∈ S[Qᵤ(x)] | Xₜ ∈ S[Pₜ(x)]) = TV.s, with u=t+T
```

where `S[Pₜ(x)]` is the set of states (i.e. atomspaces) such that
`P(x)` is true at time `t`.

What this definition is saying is that, for all time `t` and all
parameter `x`, the probability of having stochastic process `{Xₜ}`
being in a state where `Q(x)` is true at time `t+T` given that `{Xₜ}`
is in a state where `P(x)` is true at time `t`, is `TV.s`.

We can formulate this in atomese by introducing a `SatisfyingPaths`
link, that represents all paths of `{Xₜ}` such that the arguments
describe state subspaces that the paths must satisfy at given times.
Of course such satisfying paths are infinite and uncountable and are
not meant to be enumerated (although abstractions thereof can be).
However it is useful as conceptual foundation to justify temporal rule
formulae, to us and ultimately to OpenCog itself.  Such atomese
definition is given below:

```
ForAll <s=1, c=1>
  t, x
  Subset <TV>
    SatisfyingPaths
      AtTime
        Evaluation
          P
          x
	  t
	SatisfyingPaths
	  AtTime
	    Evaluation
		  Q
		  x
	  t + T
```

The `ForAll` coming from the time and parameter homogeneity may seem
too constraining at first, indeed it seems difficult to discover such
homogeneous laws capturing the real stochastic process.  Remember
however that our predictive implications are not meant to wholly
describe reality, but framents of it.  Given this, such homogeneous
hypothesis might precisely be the semantics we need for deriving our
inference formulae, in fact it provides the iid assumption we need to
justify the averaging.  It also does not stop us from improving and
refining our models.  Indeed if evidence shows that such homogeneity
has become unlikely for a given predictive implication, such evidence
can be used to create better predictive implications and add them to
our model ensemble aimed at collectively improving the estimate of the
true stochastic process distribution.

### Loosening Homogeneity Assumptions

Let's exploring if we can loosen such homogeneity assumptions, by
replacing `ForAll` by averaging, i.e. replacing micro-homogeneity by
macro-homogeneity.  First let's try to loosen everything, time and
parameter.  Here's a candidate definition for that

```
Pr(⋃ᵤ,ₓ {Xᵤ∈S[Qᵤ(x)]} | ⋃ₜ,ₓ {Xₜ∈S[Pₜ(x)]}) = TV.s, with u=t+T
```

where `⋃ₜ,ₓ A(t,x)` is the countable union of `A(t,x)`, and `{e}` is
the set, here set of paths, satisfying expression `e`.

It's not difficult to see that such definition is problematic.  Indeed
if `Q` and `P` are uniformly random then it gives a TV strength of one
(as any path will eventually find itself in a valid state),
contradicting our wish/expectation of being 0.5.

Another option is to only replace only the `ForAll` over parameters by
averaging, but keep the universal quantification over time.  For
instance think of the following example where Marie systematically
blushes when being complimented, while John never blushes.  The
strength of the predictive implication

```
Compliment↝Blush
```

is expected to be 0.5, contracticting the parameter-homogeneity
assumption since it is 1 for Marie and 0 for John.  In order to
represent that we can use the following definition, keeping
homogeneity for time only:

```
∀t Pr(⋃ₓ {Xᵤ∈S[Qᵤ(x)]} | ⋃ₓ {Xₜ∈S[Pₜ(x)]}) = TV.s, with u=t+T
```

Or, equivalently

```
∀t Pr(Xᵤ ∈ ⋃ₓ S[Qᵤ(x)] | Xₜ ∈ ⋃ₓ S[Pₜ(x)]) = TV.s, with u=t+T
```

## Semantics of SequentialAnd

TODO

## Temporal Deduction

Here we derive the inference formula for doing temporal deduction,
that is given the inference rule

```
P↝ᵀ¹Q
Q↝ᵀ²R
⊢
P↝ᵀ¹⁺ᵀ²R
```

with respective truth values `TV1`, `TV2` and `TV3` for `P↝ᵀ¹Q`,
`Q↝ᵀ²R` and `P↝ᵀ¹⁺ᵀ²R`.

So what we want is to express the probability of `P↝ᵀ¹⁺ᵀ²R`

```
Pr(Xᵥ ∈ S[Rᵥ(x)] | Xₜ ∈ S[Pₜ(x)])
```

where `v=t+T1+T2`, given the probabilities of `P↝ᵀ¹Q` and `Q↝ᵀ²R`,
respectively

```
Pr(Xᵤ ∈ S[Qᵤ(x)] | Xₜ ∈ S[Pₜ(x)])
Pr(Xᵥ ∈ S[Rᵥ(x)] | Xᵤ ∈ S[Qᵤ(x)])
```

where `u=t+T1`.

We start with

```
Pr(Xᵥ ∈ S[Rᵥ(x)] | Xₜ ∈ S[Pₜ(x)]) = Pr(Xᵥ ∈ S[Rᵥ(x)], Xₜ ∈ S[Pₜ(x)]) / Pr(Xₜ ∈ S[Pₜ(x)])
```

For simplicity let's do the following renaming

```
A = Xₜ ∈ S[Pₜ(x)]
B = Xᵤ ∈ S[Qᵤ(x)]
C = Xᵥ ∈ S[Rᵥ(x)]
```

Thus `A` is the set of paths where `{Xₜ}` is in a state such `P(x)` is
true at time `t`, etc.

We start with

```
P(C|A) = P(C,A) / P(A)
```

Let's partition `P(C,A)` according to `B`

```
P(C|A) = (P(C,B,A) + P(C,¬B,A)) / P(A)
```

which can be turned into

```
P(C|A) = (P(C|B,A)×P(B,A) + P(C|¬B,A)×P(¬B,A)) / P(A)
P(C|A) = (P(C|B,A)×P(B|A)×P(A) + P(C|¬B,A)×P(¬B|A)×P(A)) / P(A)
P(C|A) = P(C|B,A)×P(B|A) + P(C|¬B,A)×P(¬B|A)
```

Now we can go two ways

1. No assumption about `P(C|B,A)` and `P(C|¬B,A)`.  That is we don't
   assume anything about how `B↝C` and `¬B↝C` behave in the context of
   `A`.  If we go this way we need to introduce premises so that these
   can be further inferred.

2. Assume `P(C|B,A) = P(C|B)` and `P(C|¬B,A) = P(C|¬B)`.  That is we
   assume that `B↝C` and `¬B↝C` are gonna behave identically in the
   universal context and the of `A`.

which way we go depends on what knowledge we have at our disposal.  If
we have no knowledge whatsoever about how `B↝C` and `¬B↝C` behave in
the context of `A`, we might assume (2.), then once we have gathered
evidence (direct or indirect) invalidating (2.), we can go with (1.).

As in the PLN book, let's go with (2.) for now, which allows to
simplify `P(C|A)` as follows

```
P(C|A) = P(C|B)×P(B|A) + P(C|¬B)×P(¬B|A)
```

Let's attempt to rewrite the right term of the sum to get rid of `¬B`,
to minimize the number of required premises.

```
P(C|¬B) = P(C,¬B) / P(¬B)
P(C|¬B) = (P(C)-P(C,B)) / P(¬B)
P(C|¬B) = (P(C)-P(C|B)×P(B)) / P(¬B)
P(C|¬B) = (P(C)-P(C|B)×P(B)) / (1-P(B))
P(C|¬B) = (P(C)-P(C|B)×P(B)) / (1-P(B))
```

```
P(¬B|A) = P(¬B,A) / P(PA)
P(¬B|A) = (P(A)-P(B,A)) / P(A)
P(¬B|A) = 1-P(B|A)
```

Given that let's get back to `P(C|A)`

```
P(C|A) = P(C|B)×P(B|A) + ((P(C)-P(C|B)×P(B)) / (1-P(B)))×(1-P(B|A))
P(C|A) = P(C|B)×P(B|A) + (P(C)-P(C|B)×P(B))×(1-P(B|A))/(1-P(B))
```

Which brings us back to the equation of the PLN book at the end of
Section 5.2.2.1 Heuristic Derivation of the
Independence-Assumption-Based Deduction Rule.

For now we can do that, but eventually maybe we'll want to go with
(1.) (no assumption) and let PLN develop these premises, `P(C|B,A)`
and `P(C|¬B,A)` and possibly introduce (2.) on its own, during these
extra steps, if no evidence to the contrary is yet available.  That
might not be the most efficient way to go about it, but it certainly
is the most flexible and elegant.  We'll have to try and see.

Also I want to emphasize, if that wasn't obvious, that assumption (2.)
does not always hold in practice, so we really need a way to deviate
from it.  Here's an example, let's say that we know that when John is
sad, telling him a joke makes him smile, `B↝C`.  But let's assume that
a tragedy happens, making John sad, `A↝B`.  In the context of that
tragedy, `A`, a joke will no longer have its intended effect,
i.e. `P(C|B,A) ≠ P(C|B)`.  Initially we might have no direct way to
anticipate that such assumption does not hold, because it might never
have been observed before.  That might be where decoupling such
assumption from the rule might be beneficial because `P(C|B,A)` could
still be indirectly inferred, for instance, by using indirect evidence
of how other agents behave in the context of a tragedy, and what this
would say about John's behavior.

So to conclude we can tell that temporal deduction behaves exactly as
regular deduction.  That is though because we assume the lags are
precisely known, it's clear however that if the timing is off by some
delta, `T1' = T1+Δ`, then we need to estimate how well `A↝B` holds for
this delta.  This requires adding assumptions which have not been
fleshed out yet.  We have the usual confidence decrease around the
known time, as well as taking time scales into account, etc, but
that's still unformal.  More research is needed in that regard.

## Distributional Lags

We have assumed above that `T1` and `T2` are constants, in general
we'll want to support distributional lags, which shouldn't be
conceptually harder, though likely more expensive as `T1+T2` then
becomes the convolution product of `T1` and `T2`.
