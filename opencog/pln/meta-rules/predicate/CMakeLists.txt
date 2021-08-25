# Predicate logic meta-rules
ADD_GUILE_MODULE(FILES
	conditional-total-instantiation.scm
	conditional-partial-instantiation.scm
	universal-total-instantiation.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/pln/meta-rules/predicate"
)
