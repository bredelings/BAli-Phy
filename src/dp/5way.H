/*
   Copyright (C) 2004-2007,2010 Benjamin Redelings

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
/// \file 5way.H
///
/// \brief Defines the HMM for pairwise alignments on 5 branches in an NNI configuration.
///

#ifndef FIVEWAY_H
#define FIVEWAY_H

#include <vector>
#include "alignment/alignment.H"
#include "models/parameters.H"
#include "2way.H"
#include "hmm.H"


// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A5 {

  namespace states = A2::states;
  typedef HMM::bitmask_t bitmask_t;

  struct hmm_order
  {
    std::vector<int> nodes;
    int topology = -1;
    hmm_order() {}
    hmm_order(const std::vector<int>& n, int t):nodes(n),topology(t) {}
  };

  hmm_order get_nodes_random(const TreeInterface& t, int b);

  hmm_order get_nodes(const TreeInterface& t, int b);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  log_double_t correction(const data_partition& P, const hmm_order& order);

  log_double_t correction(const Parameters& P, const hmm_order& nodes);

  log_double_t acceptance_ratio(const Parameters& P1, const hmm_order&,
				const Parameters& P2, const hmm_order);

  HMM get_HMM(const data_partition& P, const hmm_order& nodes);

  std::vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const hmm_order& nodes);
}




#endif
