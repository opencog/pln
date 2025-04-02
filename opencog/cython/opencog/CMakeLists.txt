
# Need to use -fno-strict-aliasing when compiling cython code, in order
# to avoid nasty compiler warnings about aliasing.  Cython explicitly
# performs aliasing, in order to emulate python object inheritance.
# See, for example,
# https://groups.google.com/forum/#!topic/cython-users/JV1-KvIUeIg
#
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing")

INCLUDE_DIRECTORIES(
	${ATOMSPACE_INCLUDE_DIR}
	${Python3_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
)

SET(CYTHON_FLAGS "-3" "-f" "-Wextra" # "-Werror"
	"-I" "${ATOMSPACE_INCLUDE_DIR}/opencog/cython"
	"-I" "${ATOMSPACE_INCLUDE_DIR}/opencog/cython/opencog")

# Use this as a guide:
# https://github.com/OpenKinect/libfreenect/blob/master/wrappers/python/CMakeLists.txt

##################### PLN Types ##################

CYTHON_ADD_MODULE_PYX(pln
	"pln_types.pyx"
)

list(APPEND ADDITIONAL_MAKE_CLEAN_FILES "pln_types.cpp")

# opencog.pln_types Python bindings
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY(pln_cython SHARED
	pln.cpp
)

ADD_DEPENDENCIES(pln_cython pln-types)

TARGET_LINK_LIBRARIES(pln_cython
	${NO_AS_NEEDED}
	pln-types
	${ATOMSPACE_LIBRARIES}
	${Python3_LIBRARIES}
)

SET_TARGET_PROPERTIES(pln_cython PROPERTIES
	PREFIX ""
	OUTPUT_NAME pln)

INSTALL (TARGETS pln_cython
	DESTINATION "${PYTHON_DEST}")
