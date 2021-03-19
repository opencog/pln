# Temporal Reasoning with PLN

Document serving as complement of the Chapter 14 of the PLN book to
lay down the foundations of Temporal Reasoning with PLN.

Still WIP especially the part defining a Probability Space based upon
https://en.wikipedia.org/wiki/Stochastic_process where an atomspace is
the state of the system.

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

Or, equivalently (WRONG!!!!)

```
∀t Pr(Xᵤ ∈ ⋃ₓ S[Qᵤ(x)] | Xₜ ∈ ⋃ₓ S[Pₜ(x)]) = TV.s, with u=t+T
```

HYPOTHESES: We want predictive implication to be consistent with
implication if P and Q are constant over time.

TODO: the above is wrong (both)!!!! Because it allows to consider
paths such that Pₜ(a) holds for Xₜ and Qᵤ(b) holds for Xᵤ for a ≠ b.
Rather what we want is to relax the forall without allowing these
paths to contribute to the conditional probability of the predictive
implication.  We suggest the following definition:

```
TV.s = Pr(̱⋃ₓ ({Xᵤ∈S[Qᵤ(x)]} ⋂ {Xₜ∈S[Pₜ(x)]})) / Pr(⋃ₓ {Xₜ∈S[Pₜ(x)]}), with u=t+T
```

Let XPₜ(x) be the set of paths such that Pₜ(x) holds for x at t.

```
XPₜ(x) = {X | Xₜ ∈ S[Pₜ(x)]}
```

Let's explore the naive definition

```
TV.s = Pr(⋃ₓ XQᵤ(x) | ⋃ₓ XPₜ(x)), with u=t+T
```

Let's assume the set of paths XPₜ(a), that is the set of paths where P
holds at time t for some a, obvious such paths are in ⋃ₓ XPₜ(x).  Let
us now assume a subset of XPₜ(a) such that Q holds at time u for some
b such that a ≠ b, formally corresponding to XQᵤ(b) ⋂ XPₜ(a). It is
clear such a set will positively contribute to the probability above.
But that isn't what we want.  To remedy let's use such definition

```
TV.s = Pr(⋃ₓ (XQᵤ(x) ⋂ XPₜ(x)) | ⋃ₓ XPₜ(x)), with u=t+T
```

Indeed we can prove that the contribution XQᵤ(b) ⋂ XPₜ(a) is missing,
as intended.

Proof:

We want to prove that the following does not generally hold

```
XQᵤ(b) ⋂ XPₜ(a) ⊂ (⋃ₓ (XQᵤ(x) ⋂ XPₜ(x))) ⋂ (⋃ₓ XPₜ(x))
```

for a ≠ b and (XPₜ(a) ⋃ XPₜ(b)) ≠ ∅ to make sure the condition is met.

Indeed, let's first remove from the right term all subterms that do
not relate to a and b, as it is obvious that such subset relationship
will not generally hold, we're left with proving that

```
XQᵤ(b) ⋂ XPₜ(a) ⊂ ((XQᵤ(a) ⋂ XPₜ(a)) ⋃ (XQᵤ(b) ⋂ XPₜ(b))) ⋂ (XPₜ(a) ⋃ XPₜ(b))
```

does not generally hold.

We can rewrite the right term as a sum-of-products using

```
(A ⋃ B) ⋂ (C ⋃ D) = (A ⋂ C) ⋃ (A ⋂ D) ⋃ (B ⋂ C) ⋃ (B ⋂ D)
```

i.e.

```
((XQᵤ(a) ⋂ XPₜ(a)) ⋃ (XQᵤ(b) ⋂ XPₜ(b))) ⋂ (XPₜ(a) ⋃ XPₜ(b))
=
(XQᵤ(a) ⋂ XPₜ(a) ⋂ XPₜ(a)) ⋃
(XQᵤ(a) ⋂ XPₜ(a) ⋂ XPₜ(b)) ⋃
(XQᵤ(b) ⋂ XPₜ(b) ⋂ XPₜ(a)) ⋃
(XQᵤ(b) ⋂ XPₜ(b) ⋂ XPₜ(b))
=
(XQᵤ(a) ⋂ XPₜ(a)) ⋃
(XQᵤ(a) ⋂ XPₜ(a) ⋂ XPₜ(b)) ⋃
(XQᵤ(b) ⋂ XPₜ(b) ⋂ XPₜ(a)) ⋃
(XQᵤ(b) ⋂ XPₜ(b))
```

Let's assume that `XPₜ(b) ⋂ XPₜ(a)` are disjoin for a particular t, a
and b, thus `XPₜ(b) ⋂ XPₜ(a) = ∅`, in that case

```
(XQᵤ(a) ⋂ XPₜ(a) ⋂ XPₜ(b)) ⋃ (XQᵤ(b) ⋂ XPₜ(b) ⋂ XPₜ(a)) = ∅
```

thus the sum-of-product above can be simplified into

```
(XQᵤ(a) ⋂ XPₜ(a)) ⋃ (XQᵤ(b) ⋂ XPₜ(b))
```

Let's now assume that `XQᵤ(b) ⋂ XPₜ(a)` are not disjoin. Nothing
prevents us from assuming the particular cases where `XQᵤ(a) ⋂ XPₜ(a)`
and `XQᵤ(b) ⋂ XPₜ(b)` are disjoin, making

```
XQᵤ(b) ⋂ XPₜ(a) ⊂ (XQᵤ(a) ⋂ XPₜ(a)) ⋃ (XQᵤ(b) ⋂ XPₜ(b))
```

trivially not hold.

Thus, we have proven that

```
XQᵤ(b) ⋂ XPₜ(a) ⊂ (⋃ₓ (XQᵤ(x) ⋂ XPₜ(x))) ⋂ (⋃ₓ XPₜ(x))
```

does not generally hold since we can find a case where it doesn't.

In other words the following definition

```
TV.s = Pr(⋃ₓ (XQᵤ(x) ⋂ XPₜ(x)) | ⋃ₓ XPₜ(x)), with u=t+T
```

is consistent with what we want so far.

On last checkmark is we want PredictiveImplication to be equivalent to
Implication when P and Q are constant overtime.  In this case the
formula becomes

```
TV.s = Pr(⋃ₓ (XQ(x) ⋂ XP(x)) | ⋃ₓ XP(x))
```

where the temporal indices have been removed for more clarity, which
is...

TODO WARNING! Actually the state space might be instead be predicate
domains, that is the xs in P(x) and Q(x).  Thus {Xₜ} would become a
family of xs indexed by time, or should be say the evaluation links
would be indexed by time.  The solution seems to be adding an implicit
temporal parameter in each predicate, like P(x, t) and Q(x, t).  We're
back full circle.

## Semantics of SequentialAnd

### Existing Definition

In Chapter 14 of the PLN book, SequentialAnd is defined as follows

```
SequantialAnd A B <s, T>
```

iff

```
And
  And A B
  Initiation(B) – Initiation(A) lies in interval T
```

But it's not totally clear what it means.  First it is unclear what
`Initiation(A)` means, mostly it means a temporal predicate obtained
from `A` representing the degree of initiation of `A` over time
instead of its level of activity.  Then it's still unclear what

```
Initiation(B) – Initiation(A)
```

means.  Even if it is a predicate obtained from the higher level
operation of subtracting predicates, it would contradict the notion of
"lying in interval T", or least make it unclear.

Then it's not totally clear what

```
And A B
```

means too.  The best interpretation I can find is that `A` and `B` are
predicates (or concepts) incorporating a temporal variable, and thus
`And A B` means the intersection of `A` and `B` accross parameters and
time.  But that's not correct either because we are concerned about
the intersection of `A` and `B` according to some lag `T`.  Other
interpretations involving `A` and `B` being stripped of temporal
dimension don't make much sense to me either.

Let me thus offer an alternate definition that I believe will clarify
all that.

### Suggested Definition

Let's assume that `A` and `B` are temporal predicates, involving a non
temporal parameter denoted `p`, as well as a temporal parameter
denoted `t`.  Thus could be defined

```
A
:=
Lambda
  p, t
  A(p, t)
```

Given that, I offer the following definition for `SequentialAnd`.
There are two variations though, *lookback* or *lookahead*.  Let's
start with the lookahead variation

```
SequentialAnd <TV>
  T
  A
  B
:=
And <TV>
  A
  Lambda
    p, t
    B(p, t+T)
```

And then the lookback one

```
SequentialAnd <TV>
  T
  A
  B
:=
And <TV>
  Lambda
    p, t
    A(p, t-T)
  B
```

The difference concerns the meaning of the temporal parameter of
`SequentialAnd`.  In the lookahead variation, then for a particular
`p` and `t` we need to look into the future of `t` to determine the
truth of `SequentialAnd(T, A, B)(p, t)` since we need to evaluate
`B(p, t+T)`.  With the lookback variation we need to look into the
past of `t` to determine the truth of `SequentialAnd(T, A, B)(p, t)`
since we need to evaluate `A(p, t-T)`.

I think there are arguments both for the lookahead and lookback
variations.  One may favor of the lookback variation because if `t`
represents the current time, then lookback variations will be easier
to evaluate since we are more likely to have a record of the past of
their subcomponents than the future.  Whether we want to use lookahead
or lookback as default is ultimately a convention, either can be used
as long as temporal shifts are properly managed.  Will give an
argument supporting lookahead down below as well.

The other difference with the PLN book definition is there is no
`Initiation` predicate transformer or such involved.  I think it's
better to define sequential composition independently of those, it
offers more transparency and expressiveness.  Instead if one wants to
speak about initiation, termination or such, one would need to
explicitely mention it in the `SequentialAnd`.  So for instance if one
wants to express that `A` starts before `B` ends, one can write

```
SequentialAnd
  T
  Initiation(A)
  Termination(B)
```

where `Initiation(A)` is a temporal predicate transformer that takes a
temporal predicate `A` and returns a temporal predicate representing
the initiation of `A` over time instead of its activity, etc.  We
don't need to precisely define `Initiation` and such for now.  The
user should be able to define as many variations as wanted.  All that
said we'll probably want to converge to some standard/default
definitions of such temporal transformers.  As for maximizing
compactness we can always define shothands for recurring combinations
of temporal transformers such as `InitiationTerminationSequentialAnd`,
etc.

Last remark, using a `Lag` temporal transformer, defined as

```
Lag(T, A)
:=
Lambda
  p, t
  A(p, t+T)
```

one can simplify the definition of `SequentialAnd`.  For instance the
lookback variation of `SequentialAnd` would look like

```
SequentialAnd <TV>
  T
  A
  B
:=
And <TV>
  Lag(-T, A)
  B
```

## Semantics of PredictiveImplication

Now that the semantics of `SequentialAnd` is clearly defined, one can
define the semantics of `PredictiveImplication`.  The lookback
variation is

```
PredictiveImplication <TV>
  T
  A
  B
:=
Implication <TV>
  Lag(-T, A)
  SequentialAnd(T, A, B)
```

where `SequentialAnd` is also its lookback variation.  While the
lookahead variation is

```
PredictiveImplication <TV>
  T
  A
  B
:=
Implication <TV>
  A
  SequentialAnd(T, A, B)
```

where `SequentialAnd` is also its lookahead variation.  That latter is
more compact and looks identical to the definition of
`PredictiveImplication` in Chapter 14 of the PLN book.  These are
probably compelling reasons to prefer lookahead as default variation
over lookback.  But for now I prefer not to make any commitment.  Once
we get deeper into the implementation of temporal reasoning we'll have
a better idea of which variation is more elegant and we can settle to
a default accordingly.

## Temporizing Predicates

If the predicates involved are not explicitely temporal, i.e. do not
have a temporal dimension in their input arguments, but do however
evolve over time and have associated `AtTime` records, then we can
temporize them as follows.  For instance if `A` is a predicate with
argument `p` only, we can defined a temporized version
```
TA
:=
Lambda
  p, t
  AtTime
    A(p)
    t
```

where `TA` can be used instead of `A` as arguments of temporal
constructs such as `SequentialAnd`.  Maybe we could get away with the
notational abuse of using `A` directly, assuming it has implicitely
been temporized.  But for now we will not make such commitment, not
before further development has taken place regarding temporal
reasoning and its practice.

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

## Atomese Representation of ⋃ₓ {Xₜ ∈ S[Pₜ(x)]}

In the formula for temporal deduction we need to calculate P(B) and
P(C), which stand for

```
P(B) = P(⋃ₓ {Xₜ ∈ S[Qₜ(x)]})
```

which for a particular t should be well defined, however there is a
problem in assuming that such probability is the same of all t,
i.e. that such process is stationary.  Maybe the notion of time
homogeneity is wrong to begin with...

Let's take for instance the reward over time

```
AtTime
  Evaluation
    Predicate "Reward"
    Number 1
  t
```

is clearly not time homogeneous.  So then what would the probability
of the reward over time, represented by

```
Evaluation <TV>
  Predicate "Reward"
  Number 1
```

mean?

## Distributional Lags

We have assumed above that lags, `T`, `T1`, `T2`, etc, are constants.
In general we'll want to support distributional lags, which shouldn't
be conceptually harder, though likely more computationally expensive
as the sum of two distributional lags `T1+T2` then becomes the
convolution product of `T1` and `T2`.

In practice we'll probably want to introduced a Temporal Truth Value,
as hinted in the PLN book, that maps lags to TVs.  Maybe lags could
also be null and negative which would then allow to represent
simulatenous or retrospective implications with the same construct.
This however remains to be explored and for starter it is probably
fine to use constant lags.

## Frame Problem

See https://en.wikipedia.org/wiki/Frame_problem
