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
#include "rng.H"
#include "alignment/alignment-util.H"
#include "substitution/substitution-index.H"

using boost::dynamic_bitset;
using std::vector;
using std::pair;

/// Namespace for the HMM for pairwise alignments on 5 branches in an NNI configuration.
namespace A5 {

  /// Which 5 nodes are adjacent to this branch?
  hmm_order get_nodes(const Tree& T,int b) 
  {
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
    
    return {nodes,0};
  }

  hmm_order get_nodes_random(const Tree& T, int b) 
  {
    hmm_order order = get_nodes(T,b);
    if (uniform() < 0.5)
      std::swap(order.nodes[0], order.nodes[1]);
    if (uniform() < 0.5)
      std::swap(order.nodes[2], order.nodes[3]);
    if (uniform() < 0.5) {
      std::swap(order.nodes[0], order.nodes[2]);
      std::swap(order.nodes[1], order.nodes[3]);
      std::swap(order.nodes[4], order.nodes[5]);
    }
    return order;
  }


  // If we are just getting the order of the columns in the 3-way alignment
  // the this shouldn't affect anything else, should it??

  // The reason we must look at alignments is that +/- and -/+ ARE ordered
  // inside pairwise alignments.

  // What happens if we care about alignments that aren't part of the 3way?
  // Does this block stuff?  I think it did...

  log_double_t correction(const data_partition& P,const hmm_order& order) 
  {
    if (P.variable_alignment())
    {
      // get lengths of two internal nodes
      int length1 = P.A().seqlength(order.nodes[4]);
      int length2 = P.A().seqlength(order.nodes[5]);
      
      return pow( P.sequence_length_pr(length1) * P.sequence_length_pr(length2), 2);
    }
    else
      return 1;
  }


  log_double_t correction(const Parameters& P,const hmm_order& order) 
  {
    log_double_t C = 1.0;
    for(int i=0;i<P.n_data_partitions();i++)
      C *= correction(P[i], order);
    return C;
  }
    
  log_double_t acceptance_ratio(const Parameters& P1, const hmm_order& order1,
				const Parameters& P2, const hmm_order& order2) 
  {
    return correction(P1,order1)/correction(P2,order2);
  }

  HMM get_HMM(const data_partition& P, const hmm_order& order)
  {
    const Tree& T = P.T();

    const auto& nodes = order.nodes;

    int b1 = T.directed_branch(nodes[0],nodes[4]);
    int b2 = T.directed_branch(nodes[4],nodes[1]);
    int b3 = T.directed_branch(nodes[4],nodes[5]);
    int b4 = T.directed_branch(nodes[5],nodes[2]);
    int b5 = T.directed_branch(nodes[5],nodes[3]);
    
    HMM m1 = P.get_branch_HMM(b1);
    m1.remap_bits({0,4});
    HMM m2 = P.get_branch_HMM(b2);
    m2.remap_bits({4,1});
    HMM m3 = P.get_branch_HMM(b3);
    m3.remap_bits({4,5});
    HMM m4 = P.get_branch_HMM(b4);
    m4.remap_bits({5,2});
    HMM m5 = P.get_branch_HMM(b5);
    m5.remap_bits({5,3});

    HMM m12345;
    if (order.topology == 0)
      m12345 = Glue(m1,Glue(m2,Glue(m3,Glue(m4,m5))));
    else if (order.topology == 1)
      m12345 = Glue(m1,Glue(Glue(m3,Glue(m4,m5)),m2));
    else
      std::abort();

    m12345.hidden_bits.set(4);
    m12345.hidden_bits.set(5);
    m12345.B = P.get_beta();

    return m12345;
  }


  vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const hmm_order& order)
  {
    const Tree& T = P.T();

    const auto& nodes = order.nodes;

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

    if (order.topology == 0)
      return Glue_A(a1, Glue_A(a2, Glue_A(a3, Glue_A(a4, a5))));
    else if (order.topology == 1)
      return Glue_A(a1, Glue_A(Glue_A(a3, Glue_A(a4, a5)), a2));
    else
      std::abort();
  }
}
