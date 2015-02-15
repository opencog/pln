/*
 * opencog/persist/guile/PersistSCM.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include "PersistSCM.h"

using namespace opencog;

namespace opencog {

PersistSCM::PersistSCM(void)
{
	define_scheme_primitive("fetch-atom", &PersistSCM::fetch_atom, this);
	define_scheme_primitive("fetch-incoming-set", &PersistSCM::fetch_incoming_set, this);
	define_scheme_primitive("store-atom", &PersistSCM::store_atom, this);
	define_scheme_primitive("load-atoms-of-type", &PersistSCM::load_type, this);
	define_scheme_primitive("barrier", &PersistSCM::barrier, this);
}

// =====================================================================

Handle PersistSCM::fetch_atom(Handle h)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-atom");
	h = as->getImpl().fetchAtom(h);
	return h;
}

Handle PersistSCM::fetch_incoming_set(Handle h)
{
	// The "false" flag here means that the fetch is NOT recursive.
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-incoming-set");
	h = as->getImpl().fetchIncomingSet(h, false);
	return h;
}

/**
 * Store the single atom to the backing store hanging off the
  atom-space
 */
Handle PersistSCM::store_atom(Handle h)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("store-atom");
	as->getImpl().storeAtom(h);
	return h;
}

void PersistSCM::load_type(Type t)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("load-atoms-of-type");
	as->getImpl().loadType(t);
}

void PersistSCM::barrier(void)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("barrier");
	as->getImpl().barrier();
}

}
