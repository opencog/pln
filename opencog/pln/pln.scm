(define-module (opencog pln)
  #:use-module (opencog)
  #:use-module (opencog pln-config)
)

; Load the C library that calls the classserver to load the types.
(load-extension
	(string-append opencog-ext-path-pln "libpln-types")
	"pln_types_init")

(load-from-path "opencog/pln/types/pln_types.scm")
(load "pln/pln-utils.scm")
