/*
  Copyright (C) 2004-2011 Benjamin Redelings

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

///
/// \file   mcmc.H
/// \brief  Provides classes for constructing MCMC samplers.
///
/// This file provides classes for constructing MCMC samplers.  The
/// class Sampler is used to run the main loop of the sampler for
/// bali-phy.
///
/// \author Benjamin Redelings
/// 

#ifndef MCMC_H
#define MCMC_H

#include <functional>                                // for function
#include <iostream>                                  // for ostream
#include <map>                                       // for map
#include <string>                                    // for string
#include <valarray>                                  // for valarray
#include <vector>                                    // for vector
#include "proposals.H"                               // for Proposal
#include "util/bounds.H"                             // for bounds
#include "util/owned-ptr.H"                          // for owned_ptr
class Model;
struct slice_function;

// how to have different models, with different moves
// and possibly moves between models?

namespace MCMC {

    //---------------------- Move Stats ---------------------//
    /// \brief Stores counts, totals, and averages for an MCMC transition kernel
    /// 
    /// The averages for the i-th statistic are totals[i]/counts[i]. For 
    /// example, the success rate for an MH transition kernel would have a
    /// count (the number of moves run) and a total (the number of successes)
    /// resulting in an average (the fraction of successes).
    struct Result 
    {
	/// The number of trials for each statistic
	std::valarray<int> counts;

	/// The sum of values over the trials
	std::valarray<double> totals;

	/// The number of statistics we are calculating
	int size() const {return counts.size();}

	/// Increment counts and totals by the counts and totals given.
	void inc(const Result&);

	Result() {}
	Result(bool);
	Result(int,int=1);
    };

    class MoveStats: public std::map<std::string,Result>
    {
    public:
	void inc(const std::string&, const Result&);
    };
}

std::ostream& operator <<(std::ostream& o, const MCMC::MoveStats& Stats);

namespace MCMC
{
    using ::operator<<;
}
#endif
