
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atompln
	ImplicationScopeLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atompln pln_atom_types)

TARGET_LINK_LIBRARIES(atompln
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atompln EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ImplicationScopeLink.h
	DESTINATION "include/opencog/atoms/pln"
)
