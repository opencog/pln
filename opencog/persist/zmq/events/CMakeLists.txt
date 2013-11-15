INCLUDE_DIRECTORIES (
        ${CMAKE_BINARY_DIR} # todo: check if necessary
#        ${PROJECT_SOURCE_DIR}/opencog/util
#        ${PROJECT_SOURCE_DIR}/opencog/atomspace
#        ${PROJECT_SOURCE_DIR}/opencog/server
#        ${EXPAT_INCLUDE_DIRS}
)

LINK_DIRECTORIES(
        ${PROJECT_BINARY_DIR}/opencog/atomspace
        ${PROJECT_BINARY_DIR}/opencog/server
)

ADD_LIBRARY (atomspacepublishermodule SHARED
    AtomSpacePublisherModule
)

LINK_LIBRARIES(
        server
        ${Boost_SIGNALS_LIBRARY}
)

TARGET_LINK_LIBRARIES(atomspacepublishermodule
    zmq
)
