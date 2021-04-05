/*
 * PredictiveImplicationScopeLink.h
 *
 * Copyright (C) 2020 OpenCog Foundation
 * All Rights Reserved
 * Author: Kasim
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

#ifndef PLN_PREDICTIVEIMPLICATIONSCOPELINK_H
#define PLN_PREDICTIVEIMPLICATIONSCOPELINK_H

#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/pln/atom-types/atom_types.h>

namespace opencog
{
class PredictiveImplicationScopeLink : public ScopeLink
{
protected:
	void init(void);

	void extract_variables(const HandleSeq &oset);

	Handle _implicand;
	Handle _time_interval;
public:
	PredictiveImplicationScopeLink(const HandleSeq &&,
	                               Type= PREDICTIVE_IMPLICATION_SCOPE_LINK);

	PredictiveImplicationScopeLink(const PredictiveImplicationScopeLink &) = delete;

	PredictiveImplicationScopeLink &operator=(const PredictiveImplicationScopeLink &) = delete;

	static Handle factory(const Handle &);
};

typedef std::shared_ptr<PredictiveImplicationScopeLink> PredictiveImplicationScopeLinkPtr;
static inline PredictiveImplicationScopeLinkPtr PredictiveImplicationScopeLinkCast(const Handle& h)
{ return std::dynamic_pointer_cast<PredictiveImplicationScopeLink>(h); }
static inline PredictiveImplicationScopeLinkPtr PredictiveImplicationScopeLinkCast(AtomPtr a)
{ return std::dynamic_pointer_cast<PredictiveImplicationScopeLink>(a); }

#define createPredictiveImplicationScopeLink std::make_shared<PredictiveImplicationScopeLink>

}

#endif //PLN_PREDICTIVEIMPLICATIONSCOPELINK_H
