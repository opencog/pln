;; TODO: Load all rules at start up, and only add rule bases
;;
;; pln-add-rule-base
;; pln-list-rule-bases
;; pln-list-rule-base-rules

(use-modules (srfi srfi-1))

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (opencog ure))

; ----------------------------------------------------------------------------
(define-public (pln-load-rules RULE-TYPE)
"
  pln-load-rules RULE-TYPE

  Loads the different variations of the rules known by RULE-TYPE. A RULE-TYPE
  may include the categorization of the rule. For example, 'term/deduction'
  implies that the rule to be loaded is the term-logic deduction rule.
"
  ; NOTE:
  ; 1. If a rule needs formula defined in formulas.scm then the rule file
  ;    should load it.
  ; 2. Rule files are assumed to be named as "RULE-TYPE.scm"
  ; 3. load-from-path is used so as to be able to use build_dir/opencog/scm,
  ;    even when the module isn't installed.

  (load-from-path (string-append "opencog/pln/rules/" RULE-TYPE ".scm"))
)

(define-public (pln-load-meta-rules RULE-TYPE)
"
  pln-load-meta-rules RULE-TYPE

  Loads the different variations of the meta rules known by
  RULE-TYPE. A RULE-TYPE may include the categorization of the rule. For
  example, 'predicate/conditional-full-instantiation' implies that the
  rule to be loaded is the term-logic deduction rule.
"
  ; NOTE:
  ; 1. If a rule needs formula defined in formulas.scm then the rule file
  ;    should load it.
  ; 2. Rule files are assumed to be named as "RULE-TYPE.scm"
  ; 3. load-from-path is used so as to be able to use build_dir/opencog/scm,
  ;    even when the module isn't installed.

  (load-from-path (string-append "opencog/pln/meta-rules/" RULE-TYPE ".scm"))
)

(define-public pln-atomspace (cog-new-atomspace))

(define-public (pln-mk-rb)
"
  Create

  (Concept \"pln-rb\")
"
  (cog-new-node 'ConceptNode "pln-rb")
)

(define-public (pln-rb)
"
  Get

  (Concept \"pln-rb\")

  from pln-atomspace
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (define pln-atomspace-rb (pln-mk-rb))
  (cog-set-atomspace! current-as)
  pln-atomspace-rb)

(define* (pln-load #:key (rule-base ('standard)))
"
  Load and configure the PLN rule base.

  Usage: (pln-load #:rule-base rb)

  rb: [optional, default='standard] Rule base to load, with 2 rule
      bases supported so far

        1. 'empty
        2. 'standard

      The 'empty rule base contains no rule. In such case rules can be
      added using pln-add-rule-by-name or pln-add-rules-by-names.

      The 'standard rule base contains a few dozens of rules, such as
      deduction, modus ponens, contraposition, fuzzy conjunction and
      disjunction, as well as conditional instantiation, all in various
      declinations. To list all the rules one may use

        (pln-weighted-rules)

      Also, rules can be added or subtracted using the functions
      pln-add-rule-by-name, pln-add-rules-by-names,
      pln-rm-rule-by-name and pln-rm-rules-by-names.
"
  ;; Switch to PLN atomspace
  (define current-as (cog-set-atomspace! pln-atomspace))

  ;; Load rule files
  (pln-load-rules "term/deduction")
  (pln-load-rules "term/crisp-deduction")
  (pln-load-rules "term/inheritance-direct-introduction")
  (pln-load-rules "propositional/modus-ponens")
  (pln-load-rules "propositional/contraposition")
  (pln-load-rules "propositional/fuzzy-conjunction-introduction")
  (pln-load-rules "propositional/fuzzy-disjunction-introduction")

  ;; Load meta rule files
  (pln-load-meta-rules "predicate/conditional-full-instantiation")
  (pln-load-meta-rules "predicate/conditional-partial-instantiation")

  ;; Attach rules to PLN rule-base
  (let ((rlst (cond ((equal? rule-base 'empty)
                     (list))
                    ((equal? rule-base 'standard)
                     (list
                      ;; Deduction
                      "deduction-implication-rule"
                      "deduction-subset-rule"
                      "deduction-inheritance-rule"

                      ;; Modus Ponens
                      "modus-ponens-inheritance-rule"
                      "modus-ponens-implication-rule"
                      "modus-ponens-subset-rule"

                      ;; Contraposition
                      "crisp-contraposition-implication-scope-rule"
                      "contraposition-implication-rule"
                      "contraposition-inheritance-rule"

                      ;; Fuzzy Conjunction Introduction
                      "fuzzy-conjunction-introduction-1ary-rule"
                      "fuzzy-conjunction-introduction-2ary-rule"
                      "fuzzy-conjunction-introduction-3ary-rule"
                      "fuzzy-conjunction-introduction-4ary-rule"
                      "fuzzy-conjunction-introduction-5ary-rule"

                      ;; Fuzzy Disjunction Introduction
                      "fuzzy-disjunction-introduction-1ary-rule"
                      "fuzzy-disjunction-introduction-2ary-rule"
                      "fuzzy-disjunction-introduction-3ary-rule"
                      "fuzzy-disjunction-introduction-4ary-rule"
                      "fuzzy-disjunction-introduction-5ary-rule"

                      ;; Conditional Full Instantiation
                      "conditional-full-instantiation-implication-scope-meta-rule"
                      "conditional-full-instantiation-implication-meta-rule"
                      "conditional-full-instantiation-inheritance-meta-rule")))))

    (ure-add-rules-by-names (pln-mk-rb) rlst))

  ;; Switch back to previous space
  (cog-set-atomspace! current-as)

  ;; Avoid confusing the user with a return value
  *unspecified*)

(define-public (pln-prt-atomspace)
"
  Print all PLN rules loaded in pln-atomspace
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (cog-prt-atomspace)
  (cog-set-atomspace! current-as)

  ;; Avoid confusing the user with a return value
  *unspecified*)

(define-public (pln-weighted-rules)
"
  List all weighted rules in the PLN rule base.
"
  (ure-weighted-rules (pln-rb)))

(define-public (pln-set-rule-tv! rule-name tv)
"
  Set the weight TV of a given rule name, i.e. DefinedSchemaNode,
  associated to the PLN rule base. Under the hood this sets the TV
  of

  MemberLink
    rule-name
    (ConceptNode \"pln-rb\")
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (cog-set-tv! (MemberLink rule-name (pln-mk-rb)) tv)
  (cog-set-atomspace! current-as)

  *unspecified*)

(define-public (pln-add-rule-by-name rule-name)
"
  Call ure-add-rule-by-name on the PLN rule base. See

    (help ure-add-rule-by-name)

  for more info.
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (ure-add-rule-by-name (pln-mk-rb) rule-name)
  (cog-set-atomspace! current-as)

  *unspecified*)

(define-public (pln-add-rules-by-names rule-names)
"
  Call ure-add-rules-by-names on the PLN rule base. See

    (help ure-add-rules-by-names)

  for more info.
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (ure-rm-rules-by-names (pln-mk-rb) rule-names)
  (cog-set-atomspace! current-as)

  *unspecified*)

(define-public (pln-rm-rule-by-name rule-name)
"
  Call ure-rm-rule-by-name on the PLN rule base. See

    (help ure-rm-rule-by-name)

  for more info.
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (ure-rm-rule-by-name (pln-mk-rb) rule-name)
  (cog-set-atomspace! current-as)

  *unspecified*)

(define-public (pln-rm-rules-by-names rule-names)
"
  Call ure-rm-rules-by-names on the PLN rule base. See

    (help ure-rm-rules-by-names)

  for more info.
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (ure-rm-rules-by-names (pln-mk-rb) rule-names)
  (cog-set-atomspace! current-as)

  *unspecified*)

(define-public (pln-fc . args)
"
  Wrapper around cog-fc using (pln-rb) as rule base.

  See (help cog-fc) for more info.
"
  (apply cog-fc (cons (pln-rb) args)))

(define-public (pln-bc . args)
"
  Wrapper around cog-bc using (pln-rb) as rule base.

  See (help cog-bc) for more info.
"
  (apply cog-bc (cons (pln-rb) args)))

(export pln-load)
