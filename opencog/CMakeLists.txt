DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog pln-config" "PLN_TEST")

ADD_SUBDIRECTORY (atoms)
ADD_SUBDIRECTORY (pln)

IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)

WRITE_GUILE_CONFIG(
	${GUILE_BIN_DIR}/opencog/pln-config.scm
	SCM_CONFIG TRUE
)

WRITE_GUILE_CONFIG(
	${GUILE_BIN_DIR}/opencog/pln-config-installable.scm
	SCM_CONFIG FALSE
)

INSTALL(
	FILES ${GUILE_BIN_DIR}/opencog/pln-config-installable.scm
	DESTINATION ${GUILE_SITE_DIR}/opencog RENAME pln-config.scm
)
