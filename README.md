# Probabilistic Logic Network

opencog | singnet
------- | -------
[![CircleCI](https://circleci.com/gh/opencog/pln.svg?style=svg)](https://circleci.com/gh/opencog/pln) | [![CircleCI](https://circleci.com/gh/singnet/pln.svg?style=svg)](https://circleci.com/gh/singnet/pln)

Probabilistic Logic Network, or PLN for short, is a logic invented by
Ben Goertzel et al [1] for common sense reasoning. It is particularily
well suited for uncertain reasoning, especially when knowledge is
based on limited observations from reality, but can also handle
abstract mathematical reasoning, and the relationship between the two.

To handle uncertainty PLN represents truth as a second order
distribution, i.e. a probabilistic distribution over probabilistic
distributions. Doing so allows to capture uncertainty while remaining
in the well known and proven framework of probability theory.

## Building and Installing

### Prequisite

* Build and install [Unified Rule Engine](https://github.com/opencog/ure)

### Building PLN

```bash
# Download PLN
git clone https://github.com/opencog/pln.git

# Move to its project folder
cd pln

# Build with CMake
mkdir build
cd build
cmake ..
make -j
```

### Installing PLN

After building, you must install PLN

```bash
sudo make install
```

Running `ldconfig` might required as well

```bash
ldconfig /usr/local/lib/opencog
```

Usage
-----

The easiest way to use PLN is via its scheme bindings. For that enter

```bash
guile
```

load the PLN module

```scheme
(use-modules (opencog pln))
```

then load PLN rules with functions `pln-load`, and run the forward and
backward chainers with `pln-fc` and `pln-bc`. More help can be found in
guile's online help `(help pln-load)`, `(help pln-fc)` and `(help pln-bc)`.

More usage information can be found in

[opencog/pln/README.md](opencog/pln/README.md)

Examples
--------

PLN examples can be found under the [examples/pln](examples/pln)
directory. In particular the following examples use the `pln` module


* [examples/pln/ancestors](examples/pln/ancestors)
* [examples/pln/good-songs](examples/pln/good-songs)

The other examples can be informative but directly use the URE and
thus are less user friendly.
