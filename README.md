Probabilistic Logic Network
===========================

Probabilistic Logic Network, or PLN for short, is a logic invented by
Ben Goertzel et al [1] for common sense reasoning. It is particularily
well suited for uncertain reasoning, especially when knowledge is
based on limited observations from reality, but can also handle
abstract mathematical reasoning, and the relationship between the two.

To handle uncertainty PLN represent truth as a second order
distribution, i.e. a probabilistic distribution over probabilistic
distributions. Doing so allows to capture uncertainty while remaining
in the well known and proven framework of probability theory.

Usage
-----

This repository contains PLN rules for the
[Unified Rule Engine](https://github.com/opencog/ure),
or URE for short.  It allows forward or backward chaining, and
ultimately everything that Unified Rule Engine allows such asinference
control.

The rules and meta-rules (rules producing rules) are located under

```
opencog/pln/rules
opencog/pln/meta-rules
```

One can directly use these rules with the URE. Alternatively there is
a scheme PLN module containing predefined rule sets, see

[opencog/pln/README.md](opencog/pln/README.md)
