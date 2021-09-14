;; A list of the helper functions necessary for temporal reasoning
;;

(define (get-pi-antecedants PI)
"
  Return the antecedants of a predictive implication. For instance given

  PredictiveImplicationLink
    <offset>
    And
      <P1>
      <P2>
    <Q>

  then return a scheme list with P1 and P2.
"
  (let* ((P (cog-outgoing-atom PI 1)))
    (if (equal? (cog-type P) 'AndLink)
	(cog-outgoing-set P)
	(list P))))

(define (get-pis-antecedent PIS)
"
  Return the antecedent of a predictive implication scope. That is given

  PredictiveImplicationScope
    <vardecl>
    <lag>
    <antecedent>
    <succedent>

  then return <antecedent>.
"
  (cog-outgoing-atom PIS 2))

(define (get-pi-succedents PI)
"
  Return the succedent of a predictive implication scope. For instance given

  BackPredictiveImplicationLink
    <offset>
    And
      <P1>
      <P2>
    <Q>

  then return a scheme list with Q.
"
  (let* ((P (cog-outgoing-atom PI 2)))
    (if (equal? (cog-type P) 'AndLink)
	(cog-outgoing-set P)
	(list P))))

(define (get-pis-succedent PIS)
"
  Return the succedent of a predictive implication scope. That is given

  PredictiveImplicationScope
    <vardecl>
    <lag>
    <antecedent>
    <succedent>

  then return <succedent>.
"
  (cog-outgoing-atom PIS 3))

(define (get-pi-lag PI)
"
  Return the lag of a BackPredictiveImplicationLink.

  That is given

  BackPredictiveImplicationLink
    <lag>
    P
    Q

  returns <lag>
"
  (cog-outgoing-atom PI 0))

(define (temporal-plus T1 T2)
"
  Calculate the addition of two time nodes (or here naturals for now).
"
  (if (equal? (cog-type T1) 'ZLink)
      T2
      (temporal-plus (cog-outgoing-atom T1 0) (S T2))))

;;
(define (get-typed-vars vardecl)
"
  Take a variable declaration and output its scheme list of typed variables
"
  (if (or (equal? (cog-type vardecl) 'VariableSet)
	  (equal? (cog-type vardecl) 'VariableList))
      (cog-outgoing-set vardecl)
      (list vardecl)))

(define (vardecl-append vardecl1 vardecl2)
"
  Take 2 variable declarations and append them. For now it is assumed
  that they do not have any variable in common. Also the resulting
  variable declaration will always be a list, regardless of whether the
  input variable declarations are sets.
"
  (let* ((lst (append (get-typed-vars vardecl1) (get-typed-vars vardecl2))))
    (if (< 1 (length lst))
	(VariableList lst)
	(car lst))))

(define (get-vardecl PIS)
"
  Return the variable declaration of a PredicateImplicationScope
"
  (cog-outgoing-atom PIS 0))

(define (get-pis-lag PIS)
"
  Return the lag of a PredictiveImplicationScopeLink.

  That is given

  PredictiveImplicationScopeLink
    <vardecl>
    <lag>
    <P>
    <Q>

  returns <lag>
"
  (cog-outgoing-atom PIS 1))

(define (lag-add LAG T)
"
  Add LAG to T. For example

    LAG = (S (S Z))
    T = (Variable \"$T\")

  return (S (S (Variable \"$T\")))
"
  (if (equal? (cog-type LAG) 'ZLink)
      T
      (lag-add (cog-outgoing-atom LAG 0) (S T))))
