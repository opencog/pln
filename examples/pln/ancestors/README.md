# Deduction

Simple PLN demo to show how to reason about the transitivity of
inheritance links.

## Usage

```bash
guile -l ancestors.scm
```

The ancestors should be stored in `ancestors` variable

```scheme
ancestors
```

which should contain the following

```scheme
(SetLink
   (ConceptNode "B")
   (ConceptNode "D")
   (ConceptNode "F")
   (ConceptNode "C")
)
```
