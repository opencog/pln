# Term logic rules
ADD_GUILE_MODULE(FILES
	deduction.scm
	full-deduction.scm
	present-deduction.scm
	formulas.scm
	condition-negation.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/pln/rules/term"
)
