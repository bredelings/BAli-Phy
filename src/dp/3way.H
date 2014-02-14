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
/// \file 3way.H
///
/// \brief Defines the HMM for three pairwise alignments on adjacent branches of a tree.
///

#ifndef THREEWAY_H
#define THREEWAY_H

#include "mytypes.H"
#include "alignment/alignment.H"
#include "models/parameters.H"
#include "2way.H"
#include "hmm.H"

// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8

/// Namespace for the HMM for three pairwise alignments on adjacent branches of a tree.
namespace A3 {

  namespace states = A2::states;
  typedef HMM::bitmask_t bitmask_t;

  //  std::vector<int> get_nodes(const Tree& T,int n0);

  std::vector<int> get_nodes_random(const Tree& T,int n0);

  std::vector<int> get_nodes_branch(const Tree& T,int n0,int n1);

  std::vector<int> get_nodes_branch_random(const Tree& T,int n0,int n1);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  efloat_t correction(const data_partition& P,const std::vector<int>& nodes);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  efloat_t correction(const Parameters& P,const std::vector<int>& nodes);

  efloat_t acceptance_ratio(const Parameters& P1,const std::vector<int>&,
			    const Parameters& P2,const std::vector<int>&);

  std::vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const std::vector<int>& nodes);
}

#endif
