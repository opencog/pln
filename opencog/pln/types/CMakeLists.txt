
OPENCOG_ADD_ATOM_TYPES(atom_types.script
	atom_types.h
	atom_types.definitions
	atom_types.inheritance
	pln_types.scm
	pln_types.pyx
)
ADD_CUSTOM_TARGET(pln_atom_types DEPENDS atom_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (pln-types SHARED
	atom_types_init
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(pln-types pln_atom_types)

TARGET_LINK_LIBRARIES(pln-types
	${ATOMSPACE_atomtypes_LIBRARY}
)

INSTALL (TARGETS pln-types
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

ADD_GUILE_EXTENSION(SCM_CONFIG pln-types "opencog-ext-path-pln")

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/atom_types.h
	DESTINATION "include/opencog/pln/types"
)

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/pln_types.scm
	DESTINATION "${GUILE_SITE_DIR}/opencog/pln/types"
)
