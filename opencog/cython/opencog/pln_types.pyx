from opencog.atomspace import get_refreshed_types
from opencog.utilities import add_node, add_link

cdef extern :
    void pln_types_init()

pln_types_init()
types = get_refreshed_types()

include "opencog/pln/types/pln_types.pyx"
