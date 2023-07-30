/*
  Copyright (C) 2011 Benjamin Redelings

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
/// \file   logger.C
/// \brief  Provides classes for constructing MCMC samplers.
///
/// This file provides classes for constructing MCMC samplers.  The
/// class Sampler is used to run the main loop of the sampler for
/// bali-phy.
///
/// \author Benjamin Redelings
/// 

#include <iostream>

#include "logger.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "dp/2way.H"

namespace MCMC {

    int alignment_length(const data_partition& P)
    {
        if (not P.has_pairwise_alignments()) return 0;

	auto branches = P.t().all_branches_from_node(0);

	int total = P.seqlength(0);
	for(int b: branches)
	    total += P.get_pairwise_alignment(b).count_insert();

	return total;
    }

    int alignment_length(const Parameters& P)
    {
	int total = 0;
	for(int p=0;p<P.n_data_partitions();p++)
            total += alignment_length(P[p]);
	return total;
    }
}
