# Extensional rules
ADD_GUILE_MODULE(FILES
	subset-direct-introduction.scm
	conjunction-direct-introduction.scm
	extensional-similarity-direct-introduction.scm
        concept-direct-evaluation.scm
	member-deduction.scm
	extensional-utils.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/pln/rules/extensional"
)
