;; Helpers for rules pertaining to extensional reasoning 

(define (variable? X)
"
  Return #t iff X is a variable or glob
"
  (or (equal? (cog-type X) 'VariableNode)
      (equal? (cog-type X) 'GlobNode)))

(define (get-member-links-of C)
"
  Given C, return all its member links,
  (except link with variable as member).
"
  (let* ((mbr-lnks (cog-filter 'MemberLink (cog-incoming-set C)))
         (nonvar-mbr-of-C? (lambda (x)
			     (and (equal? C (gdr x))
				  (not (variable? (gar x)))))))
    (filter nonvar-mbr-of-C? mbr-lnks)))

(define (get-members-of C)
"
  Given C, return all its members (except variables)
"
  (map gar (get-member-links-of C)))
