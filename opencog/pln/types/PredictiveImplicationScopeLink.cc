/*
 * PredictiveImplicationScopeLink.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Kasim
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/pln/types/atom_types.h>
#include "PredictiveImplicationScopeLink.h"

using namespace opencog;

void PredictiveImplicationScopeLink::init(void)
{
	extract_variables(_outgoing);
}

PredictiveImplicationScopeLink::PredictiveImplicationScopeLink(const HandleSeq&& hseq, Type t)
		: ScopeLink(std::move(hseq), t)
{
	init();
}

void PredictiveImplicationScopeLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = _outgoing.size();

	if (sz < 3 or 4 < sz)
		throw InvalidParamException(TRACE_INFO,
		                            "Expecting an outgoing set of size 3 or 4, got %d", sz);

	ScopeLink::extract_variables(oset);

	if (3 == sz)
	{
		_time_interval = oset[1];
		_implicand = oset[2];
		return;
	}

	_time_interval = oset[2];
	_implicand = oset[3];
}

DEFINE_LINK_FACTORY(PredictiveImplicationScopeLink, PREDICTIVE_IMPLICATION_SCOPE_LINK)
