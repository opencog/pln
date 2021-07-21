# Probabilistic Logic Networks

PLN is a novel conceptual, mathematical and computational approach to uncertain
inference. In order to carry out effective reasoning in real-world
circumstances, AI software must robustly handle uncertainty. However, previous
approaches to uncertain inference do not have the breadth of scope required to
provide an integrated treatment of the disparate forms of cognitively critical
uncertainty as they manifest themselves within the various forms of pragmatic
inference. Going beyond prior probabilistic approaches to uncertain inference,
PLN is able to encompass within uncertain logic such ideas as induction,
abduction, analogy, fuzziness and speculation, and reasoning about time and
causality.

Further details can be found [here](http://wiki.opencog.org/wikihome/index.php/PLN).

## Implementation

The current implementation uses the
[URE](https://github.com/opencog/atomspace/tree/master/opencog/ure)
where PLN is one or a few specific rule bases, configured with scheme
function and scheme rules.

That folder contains

1. A set of scripts to easily configure the rule engine to utilize PLN
   rule bases.
2. A set of PLN rules, under the subfolder `rules`.
3. A set of PLN meta-rules, rules producing rules, under the subfolder
   `meta-rules`.
4. A set of higher level facts, often algebraic properties like
   symmetry, commutativity, etc, under the subfolder `facts`.

## Status

PLN is a work in progress. Rules are often crudely implemented,
usually for the sake of efficiency while allowing preliminary
experimentations. In particular

1. Many rules are only compatible with the forward chainer, not the
   backward chainer. That is because backward chaining requires that
   the conclusion pattern be statically defined, which makes some
   rules more difficult or less efficient to implement.
2. Confidence calculation is usually very crude, that is because it
   requires some forms of numerical integration to handle second order
   probabilities, which is costly and thus has been neglected so far.
3. Strength calculations are rather approximate too (though better
   than confidence) because ultimately their results depends not only
   on premises strengths but also their confidences.

## Usage

PLN has a scheme module containing various helpers to load rules and
run PLN inferences.

### Configure

First, the module must be loaded

```scheme
(use-modules (opencog pln))
```

Then a PLN rule-base can be loaded with

```scheme
(pln-load)
```

which loads the standard rule-base by default.  One can use this
function to load different rule-bases (though at the moment only two
rule bases are supported, `'standard` and `'empty`.  Alternatively one
can just use `pln-load-rule` to load a rule by providing the symbol of
that rule.  For example

```
(pln-load-rule 'subset-direct-introduction)
```

See `(help pln-load-rule)` for more information as well as
`(pln-supported-rules)` to get the symbols of all supported rules.

The rules are loaded in an auxilary atomspace in order not to pollute
the current atomspace.  That auxilary atomspace can be accessed via
the `pln-atomspace` variable.  If one wants to load a user defined
rule, one may use the helper

```scheme
(pln-load-from-path FILENAME)
```

In addition the `pln` modules offers helpers to display its content
without having to switch to it

```scheme
(pln-prt-atomspace)
```

To simply list its rule names and weights

```scheme
(pln-weighted-rules)
```

By default all rules have a default TV as weight, corresponding to a
flat second order distribution (as of today). One may change the
weights as follows

```scheme
(pln-set-rule-tv! rule-name tv)
```

For instance

```scheme
(pln-set-rule-tv! (DefinedSchemaNode "deduction-implication-rule") (stv 0.7 0.2))
```

sets the weight of the deduction implication rule to `(stv 0.7 0.2)`.

### Rule Naming Conventions

Below are the conventions we have adopted so far.

#### Evaluation

Rules containing `evaluation` are here to calculate the TV of an atom
without introducing it.  For instance

```
PredictiveImplicationLink
  T
  P
  Q
|-
PredictiveImplicationLink <TV>
  T
  P
  Q
```

is named `predictive-implication-direct-evaluation` because it only
calculates the TV of a predictive implication link if it is already
present.

#### Introduction

Rules containing `introduction` introduce a new atom given other
atoms, typically its outgoings.  For instance

```
A
B
|-
And <TV>
  A
  B
```

is named `conjunction-direct-introduction`.

#### Elimination

Rules that infer simpler atoms where a part has been removed contain
`elimination`.  For instance

```
Implication
  A
  Or
    B
    C
Implication
  A
  C
|-
Implication
  A
  B
```

is named `consequent-disjunction-elimination-implication`.

#### Conversion

Rules used to convert an equivalent form to another contain `to`.  For
instance

```
ImplicationScopeLink
   V
   P
   Q
|-
ImplicationLink
   LambdaLink
      V
      P
   LambdaLink
      V
      Q
```

is named `implication-scope-to-implication`.

#### Evidence-Based

Rules that use direct evidence to calculate the TV contain `direct`.
For instance

```
A
B
|-
And <TV>
  A
  B
```

is named `conjunction-direct-introduction` because the TV is
calculated based on the elements of `A` and `B`.  Note that the
presence or absence of these elements should ideally be represented as
premises but for technical reasons that is not yet the case.

#### Order

The order in which words are placed in a rule name is currently
somewhat arbitrary.  For instance in `inheritance-deduction`,
`inheritance`, the atom type which the rule is specialized to, is
placed before the rule type `deduction`, while in
`contraposition-implication` it is the opposite.  Also, in
`fuzzy-conjunction-introduction-1ary` the unary specialization is
placed after as well.

It's not clear what convention should be followed at this point, more
reflection needs is required.  One convention could be to order words
by significance.  For instance in a deduction rule specialized for
inheritance link, `deduction`, the most informative concept, would
appear first, to the left, followed by `inheritance`, which is less
informative.  Such convention is however not currently followed.

### Call Chainers

To call the forward chainer on a given source, simply type

```scheme
(pln-fc source)
```

Likewise to call the backward chainer on a given target, type

```scheme
(pln-bc target)
```

Numerous options can be used, for more information see

```scheme
(help pln-fc)
```

and

```scheme
(help pln-bc)
```

### Examples

PLN examples can be found under the [../../examples/pln](../../examples/pln)
directory. In particular the following examples use the `pln` module


* [../../examples/pln/ancestors](../../examples/pln/ancestors)
* [../../examples/pln/good-songs](../../examples/pln/good-songs)

The other examples can be informative but directly use the URE and
thus are less user friendly.
