# Term logic rules
ADD_GUILE_MODULE(FILES
	deduction.scm
	crisp-deduction.scm
	formulas.scm
	condition-negation.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/pln/rules/term"
)
