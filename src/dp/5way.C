/*
   Copyright (C) 2004-2007,2009-2010 Benjamin Redelings

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
/// \file 5way.C
///
/// \brief Defines the HMM for pairwise alignments on 5 branches in an NNI configuration.
///

#include <algorithm>
#include "5way.H"
#include "bits.H"
#include "math/logsum.H"
#include "rng.H"
#include "alignment/alignment-util.H"
#include "substitution/substitution-index.H"

using boost::dynamic_bitset;
using std::vector;
using std::pair;

/// Namespace for the HMM for pairwise alignments on 5 branches in an NNI configuration.
namespace A5 {

  /// Which 5 nodes are adjacent to this branch?
  vector<int> get_nodes(const Tree& T,int b) {
    assert(T.directed_branch(b).is_internal_branch());

    vector<const_branchview> branches;
    append(T.directed_branch(b).branches_before(),branches);
    append(T.directed_branch(b).branches_after(),branches);

    vector<int> nodes(6);
    
    // This must be an internal branch
    nodes[0] = branches[0].source();
    nodes[1] = branches[1].source();
    nodes[2] = branches[2].target();
    nodes[3] = branches[3].target();

    nodes[4] = T.directed_branch(b).source();
    nodes[5] = T.directed_branch(b).target();
    
    return nodes;
  }

  vector<int> get_nodes_random(const Tree& T, int b) {
    vector<int> nodes = get_nodes(T,b);
    if (uniform() < 0.5)
      std::swap(nodes[0],nodes[1]);
    if (uniform() < 0.5)
      std::swap(nodes[2],nodes[3]);
    if (uniform() < 0.5) {
      std::swap(nodes[0],nodes[2]);
      std::swap(nodes[1],nodes[3]);
      std::swap(nodes[4],nodes[5]);
    }
    return nodes;
  }


  // If we are just getting the order of the columns in the 3-way alignment
  // the this shouldn't affect anything else, should it??

  // The reason we must look at alignments is that +/- and -/+ ARE ordered
  // inside pairwise alignments.

  // What happens if we care about alignments that aren't part of the 3way?
  // Does this block stuff?  I think it did...

  efloat_t correction(const data_partition& P,const vector<int>& nodes) 
  {
    if (P.variable_alignment())
    {
      // get lengths of two internal nodes
      int length1 = P.A->seqlength(nodes[4]);
      int length2 = P.A->seqlength(nodes[5]);
      
      return pow( P.sequence_length_pr(length1) * P.sequence_length_pr(length2), 2);
    }
    else
      return 1;
  }


  efloat_t correction(const Parameters& P,const vector<int>& nodes) 
  {
    efloat_t C = 1.0;
    for(int i=0;i<P.n_data_partitions();i++)
      C *= correction(P[i],nodes);
    return C;
  }
    
  efloat_t acceptance_ratio(const Parameters& P1,const vector<int>& nodes1,
			      const Parameters& P2,const vector<int>& nodes2) 
  {
    return correction(P1,nodes1)/correction(P2,nodes2);
  }

  vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const vector<int>& nodes)
  {
    const Tree& T = P.T();

    int b1 = T.directed_branch(nodes[0],nodes[4]);
    int b2 = T.directed_branch(nodes[4],nodes[1]);
    int b3 = T.directed_branch(nodes[4],nodes[5]);
    int b4 = T.directed_branch(nodes[5],nodes[2]);
    int b5 = T.directed_branch(nodes[5],nodes[3]);
    
    vector<HMM::bitmask_t> a1 = convert_to_bits(P.get_pairwise_alignment(b1),0,4);
    vector<HMM::bitmask_t> a2 = convert_to_bits(P.get_pairwise_alignment(b2),4,1);
    vector<HMM::bitmask_t> a3 = convert_to_bits(P.get_pairwise_alignment(b3),4,5);
    vector<HMM::bitmask_t> a4 = convert_to_bits(P.get_pairwise_alignment(b4),5,2);
    vector<HMM::bitmask_t> a5 = convert_to_bits(P.get_pairwise_alignment(b5),5,3);

    vector<HMM::bitmask_t> a12345 = Glue_A(a1, Glue_A(a2, Glue_A(a3, Glue_A(a4, a5))));

    return a12345;
  }
}
