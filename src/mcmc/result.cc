/*
  Copyright (C) 2004-2010 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

#include "result.H"
#include "util/assert.hh"                           // for assert
#include <math.h>                                   // for log
#include <stdlib.h>                                 // for abs, abort
#include <algorithm>                                // for copy, max, fill
#include <boost/smart_ptr/intrusive_ptr.hpp>        // for intrusive_ptr
#include <iostream>                                 // for operator<<, ostream
#include <iterator>                                 // for valarray
#include <memory>                                   // for allocator_traits<...
#include <utility>                                  // for pair
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "computation/object.H"                     // for intrusive_ptr_rel...
#include "models/TreeInterface.H"                   // for TreeInterface
#include "models/model.H"                           // for Model, show_param...
#include "models/parameters.H"                      // for Parameters, accep...
#include "slice-sampling.H"                         // for integer_random_va...
#include "util/math/log-double.H"                   // for operator<<, log_d...
#include "util/myexception.H"                       // for myexception
#include "util/permute.H"                           // for randomize
#include "util/range.H"                             // for sum
#include "util/rng.H"                               // for poisson, uniform
#include "util/string/split.H"                      // for split

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MPI
#include <mpi.h>
#include <boost/mpi.hpp>
namespace mpi = boost::mpi;
#endif

extern int log_verbose;

namespace MCMC
{
    void Result::inc(const Result& R) {
	if (not counts.size()) {
	    counts.resize(R.size(),0);
	    totals.resize(R.size(),0);
	}

	counts += R.counts;
	totals += R.totals;
    }

    Result::Result(int l,int n)
	:counts(n,l),totals(0.0,l) 
    { }

    Result::Result(bool b)
	:counts(1,1),totals(0.0,1) 
    { 
	if (b) 
	    totals[0] = 1;
    }


    void MoveStats::inc(const std::string& name,const Result& R) {
	(*this)[name].inc(R);
    }

}

std::ostream& operator<<(std::ostream& o, const MCMC::MoveStats& Stats) 
{
    int prec = o.precision(4);
  
    for(std::map<std::string,MCMC::Result>::const_iterator entry = Stats.begin(); entry != Stats.end();entry++)
    {
	const MCMC::Result& R = entry->second;

	// print move name
	o<<entry->first<<":  ";

	// print move stats
	for(int i=0;i<R.size();i++) {
	    o<<"  X"<<i<<" = ";
	    if (R.counts[i])
		o<<R.totals[i]/R.counts[i];
	    else
		o<<"?";
	    o<<" ["<<R.counts[i]<<"]";
	}
	o<<std::endl;
    }
    if (Stats.empty())
	o<<"   Transition kernel average-based statistics: no data.\n";
    o.precision(prec);
    return o;
}
