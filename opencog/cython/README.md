# Python bindings for PLN

## Requirements

* Python 2.7, Python 3 recommended.
* Cython 0.14 or later. http://www.cython.org/

The bindings are written mostly using Cython, which allows writing
code that looks pythonic but gets compiled to C.  It also makes it
trivial to access both Python objects and C objects without using a
bunch of extra Python library API calls.

## Package structure

Currently the package structure looks like this:

 opencog.pln
 opencog.pln.types
