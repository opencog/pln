# Good Songs

The problem is simple, infer preferences about songs given that people
tend to like songs from the same authors.

# Usage

```bash
guile -l good-songs.scm
```

Results should be stored in

```scheme
fc-results
```

which should include, among others,

```scheme
(EvaluationLink (stv 0.9 0.5625)
  (PredicateNode "like")
  (ListLink
    (ConceptNode "Marry")
    (ConceptNode "Dextrose is my bitch")))
```

Because Marry likes a song from the same author.
