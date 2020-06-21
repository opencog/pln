(use-modules (srfi srfi-1))

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (opencog ure))

;; Atomspace containing all PLN rules. All operations of loading
;; rules, etc, take place in this atomspace to not pollute the current
;; atomspace.
(define-public pln-atomspace (cog-new-atomspace))

(define-public (pln-load-from-path FILENAME)
"
  pln-load-from-path FILENAME

  Like load-from-path but load the content into pln-atomspace. Used to
  load PLN rules without polluting the current atomspace.
"
  ;; Switch to PLN atomspace
  (define current-as (cog-set-atomspace! pln-atomspace))

  (load-from-path FILENAME)

  ;; Switch back to previous space
  (cog-set-atomspace! current-as)

  ;; Avoid confusing the user with a return value
  *unspecified*)

(define-public (pln-rule-type->filename RULE-TYPE)
"
  pln-rule-type->filename RULE-TYPE

  Turn a rule type string turn it into a filename ready to be loaded
  using load-from-path. More precisely returns

  opencog/pln/rules/<RULE-TYPE>.scm
"
  (string-append "opencog/pln/rules/" RULE-TYPE ".scm"))

(define-public (pln-meta-rule-type->filename META-RULE-TYPE)
"
  pln-meta-rule-type->filename META-RULE-TYPE

  Turn a meta-rule type string turn it into a filename ready to be
  loaded using load-from-path. More precisely returns

  opencog/pln/meta-rules/<META-RULE-TYPE>.scm
"
  (string-append "opencog/pln/meta-rules/" META-RULE-TYPE ".scm"))

(define-public (pln-load-rules RULE-TYPE)
"
  pln-load-rules RULE-TYPE

  Loads the different variations of the rules known by RULE-TYPE in
  pln-atomspace. A RULE-TYPE may include the categorization of the
  rule. For example, 'term/deduction' implies that the rule to be loaded
  is the term-logic deduction rule.

  Notes:
    1. If a rule needs formula defined in formulas.scm then the rule file
       should load it.
    2. Rule files are assumed to be named as \"RULE-TYPE.scm\"
    3. load-from-path is used so as to be able to use build_dir/opencog/scm,
       even when the module isn't installed.
"
  (pln-load-from-path (pln-rule-type->filename RULE-TYPE)))

(define-public (pln-load-meta-rules META-RULE-TYPE)
"
  pln-load-meta-rules META-RULE-TYPE

  Loads the different variations of the meta rules known by
  META-RULE-TYPE in pln-atomspace. A META-RULE-TYPE may include the
  categorization of the rule. For example,
  'predicate/conditional-full-instantiation' implies that the rule to be
  loaded is the predicate-logic conditional instantiation rule.

  Note:
    1. If a rule needs formula defined in formulas.scm then the rule file
       should load it.
    2. Rule files are assumed to be named as \"META-RULE-TYPE.scm\"
    3. load-from-path is used so as to be able to use build_dir/opencog/scm,
       even when the module isn't installed.
"
  (pln-load-from-path (pln-meta-rule-type->filename META-RULE-TYPE)))

(define-public (pln-mk-rb)
"
  Create

  (Concept \"pln-rb\")
"
  (cog-new-node 'ConceptNode "pln-rb"))

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

(define-public (pln-load . rule-bases)
"
  Load and configure PLN rules. All or most PLN rules will be loaded
  in pln-atomspace, however depending on the choosen rule base only
  some might be used.

  Usage: (pln-load rb)

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
  (define rule-base (if (< 0 (length rule-bases)) (car rule-bases) 'standard))

  ;; Clear PLN atomspace
  (pln-clear)

  ;; Load rule files
  (pln-load-rules "term/deduction")
  (pln-load-rules "term/crisp-deduction")
  (pln-load-rules "term/condition-negation")
  (pln-load-rules "propositional/modus-ponens")
  (pln-load-rules "propositional/contraposition")
  (pln-load-rules "propositional/fuzzy-conjunction-introduction")
  (pln-load-rules "propositional/fuzzy-disjunction-introduction")
  (pln-load-rules "extensional/extensional-similarity-direct-introduction")
  (pln-load-rules "extensional/subset-direct-introduction")
  (pln-load-rules "extensional/conjunction-direct-introduction")
  (pln-load-rules "extensional/concept-direct-evaluation")
  (pln-load-rules "extensional/member-deduction")
  (pln-load-rules "intensional/attraction-introduction")
  (pln-load-rules "intensional/intensional-inheritance-direct-introduction")
  (pln-load-rules "intensional/intensional-similarity-direct-introduction")
  (pln-load-rules "intensional/intensional-difference-direct-introduction")

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
    (pln-add-rules-by-names rlst))

  ;; Avoid confusing the user with a return value
  *unspecified*)

(define-public (pln-prt-pln-atomspace)
"
  Print all PLN rules loaded in pln-atomspace
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (cog-prt-atomspace)
  (cog-set-atomspace! current-as)

  ;; Avoid confusing the user with a return value
  *unspecified*)

(define-public (pln-prt-atomspace)
"
  Like pln-prt-pln-atomspace.
"
  (pln-prt-pln-atomspace))

(define-public (pln-rules)
"
  List all rules in the PLN rule base.
"
  (ure-rules (pln-rb)))

(define-public (pln-weighted-rules)
"
  List all weighted rules in the PLN rule base.
"
  (ure-weighted-rules (pln-rb)))

(define-public (pln-set-rule-tv! rule-alias tv)
"
  Set the weight TV of a given rule alias, i.e. DefinedSchemaNode,
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

(define-public (pln-add-rule-by-name rule-name . tv)
"
  Call ure-add-rule-by-name on the PLN rule base. See

    (help ure-add-rule-by-name)

  for more info.
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (apply ure-add-rule-by-name (cons (pln-mk-rb) (cons rule-name tv)))
  (cog-set-atomspace! current-as)

  *unspecified*)

(define-public (pln-add-rules-by-names rule-names)
"
  Call ure-add-rules-by-names on the PLN rule base. See

    (help ure-add-rules-by-names)

  for more info.
"
  (define current-as (cog-set-atomspace! pln-atomspace))
  (ure-add-rules-by-names (pln-mk-rb) rule-names)
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

(define-public (pln-rm-all-rules)
"
  Remove all rules for the PLN rule base. See

    (help ure-rm-all-rules)

  for more info.
"
  (ure-rm-all-rules (pln-rb)))

(define (pln-set-attention-allocation value)
"
  Wrapper around ure-set-attention-allocation using (pln-rb) as rule base.

  See (help ure-set-attention-allocation) for more info.
"
  (ure-set-attention-allocation (pln-rb) value))

(define (pln-set-maximum-iterations rbs value)
"
  Wrapper around ure-set-maximum-iterations using (pln-rb) as rule base.

  See (help ure-set-maximum-iterations) for more info.
"
  (ure-set-maximum-iterations (pln-rb) value))

(define (pln-set-complexity-penalty rbs value)
"
  Wrapper around ure-set-complexity-penalty using (pln-rb) as rule base.

  See (help ure-set-complexity-penalty) for more info.
"
  (ure-set-complexity-penalty (pln-rb) value))

(define (pln-set-jobs rbs value)
"
  Wrapper around ure-set-jobs using (pln-rb) as rule base.

  See (help ure-set-jobs) for more info.
"
  (ure-set-jobs (pln-rb) value))

(define (pln-set-fc-retry-exhausted-sources rbs value)
"
  Wrapper around ure-set-fc-retry-exhausted-sources using (pln-rb) as rule base.

  See (help ure-set-fc-retry-exhausted-sources) for more info.
"
  (ure-set-fc-retry-exhausted-sources (pln-rb) value))

(define (pln-set-fc-full-rule-application rbs value)
"
  Wrapper around ure-set-fc-full-rule-application using (pln-rb) as rule base.

  See (help ure-set-fc-full-rule-application) for more info.
"
  (ure-set-fc-full-rule-application (pln-rb) value))

(define (pln-set-bc-maximum-bit-size rbs value)
"
  Wrapper around ure-set-bc-maximum-bit-size using (pln-rb) as rule base.

  See (help ure-set-bc-maximum-bit-size) for more info.
"
  (ure-set-bc-maximum-bit-size (pln-rb) value))

(define (pln-set-bc-mm-complexity-penalty rbs value)
"
  Wrapper around ure-set-bc-mm-complexity-penalty using (pln-rb) as rule base.

  See (help ure-set-bc-mm-complexity-penalty) for more info.
"
  (ure-set-bc-mm-complexity-penalty (pln-rb) value))

(define (pln-set-bc-mm-compressiveness rbs value)
"
  Wrapper around ure-set-bc-mm-compressiveness using (pln-rb) as rule base.

  See (help ure-set-bc-mm-compressiveness) for more info.
"
  (ure-set-bc-mm-compressiveness (pln-rb) value))

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

;; TODO: maybe move to ure as well
(define-public (pln-execute-rule-by-name rule-name)
"
  Execute a rule given its name, for instance

  (pln-execute-rule-by-name \"deduction-subset-rule\")
"
  ;; TODO
)

;; TODO: move to ure
(define-public (pln-add-rule rule-symbol)
  (let* ((rule-name (string-append (symbol->string rule-symbol) "-rule")))
    (pln-add-rule-by-name rule-name)))

;; TODO: move to ure
(define-public (pln-add-rules . rule-symbols)
  (for-each pln-add-rule rule-symbols))

;; TODO: move to ure
(define-public (pln-clear)
  ;; Switch to PLN atomspace and clear
  (define current-as (cog-set-atomspace! pln-atomspace))
  (clear)

  ;; Switch back to previous space
  (cog-set-atomspace! current-as)

  ;; Avoid confusing the user with a return value
  *unspecified*)
