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

#include "4way.H"
#include <assert.h>                                 // for assert
#include <math.h>                                   // for pow
#include <stdlib.h>                                 // for abort
#include <boost/dynamic_bitset/dynamic_bitset.hpp>  // for dynamic_bitset
#include <utility>                                  // for swap, pair
#include "dp/2way.H"                                // for convert_to_bits
#include "dp/hmm.H"                                 // for HMM, Glue, Glue_A
#include "models/parameters.H"                      // for data_partition
#include "util/rng.H"                               // for uniform

using boost::dynamic_bitset;
using std::vector;
using std::pair;

/// Namespace for the HMM for pairwise alignments on 5 branches in an NNI configuration.
namespace A4 {

  /*  Which 5 nodes are adjacent to this branch?
     0
      \
       3--4--2
      /
     1
  */
    
    hmm_order get_nodes(const TreeInterface& t, int b) 
    {
	assert(t.is_internal_branch(b));
	assert(t.degree(t.source(b)) == 2);
	assert(t.degree(t.target(b)) == 3);

	vector<int> branches;
	t.append_branches_after(b, branches);
	t.append_branches_after(t.reverse(b),branches);
	assert(branches.size() == 3);

	vector<int> nodes(5);

	// This must be an internal branch
	nodes[0] = t.target(branches[0]);
	nodes[1] = t.target(branches[1]);
	nodes[2] = t.target(branches[2]);

	nodes[3] = t.target(b);
	nodes[4] = t.source(b);
    
	return {nodes,0};
    }

    hmm_order get_nodes_random(const TreeInterface& t, int b) 
    {
	hmm_order order = get_nodes(t, b);
	if (uniform() < 0.5)
	    std::swap(order.nodes[0], order.nodes[1]);

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
	    return pow( P.sequence_length_pr(order.nodes[3]), 2 ) * P.sequence_length_pr(order.nodes[4]);
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

  /* This b? numbers on this diagram describe the order in which
     we emit things on the tree.

             N0
             |
	     b1
	     |
	     N3
	    / \
	   b2  b3
	  /     \
	 N1      N4
	          \
		   b4
		    \
		     N2
		    
     We consider an alternative in which 3 and 4 are emitted  before 2.
     
   */

    HMM get_HMM(const data_partition& P, const hmm_order& order)
    {
	auto t = P.t();

	const auto& nodes = order.nodes;

	int b1 = t.find_branch(nodes[0],nodes[3]);
	int b2 = t.find_branch(nodes[3],nodes[1]);
	int b3 = t.find_branch(nodes[3],nodes[4]);
	int b4 = t.find_branch(nodes[4],nodes[2]);
    
	HMM m1 = P.get_branch_HMM(b1);
	m1.remap_bits({0,3});
	HMM m2 = P.get_branch_HMM(b2);
	m2.remap_bits({3,1});
	HMM m3 = P.get_branch_HMM(b3);
	m3.remap_bits({3,4});
	HMM m4 = P.get_branch_HMM(b4);
	m4.remap_bits({4,2});

	HMM m1234;
	if (order.topology.value() == 0)
	    m1234 = Glue(m1,Glue(m2,Glue(m3,m4)));
	else if (order.topology.value() == 1)
	    m1234 = Glue(m1,Glue(Glue(m3,m4),m2));
	else
	    std::abort();

	m1234.hidden_bits.set(3);
	m1234.hidden_bits.set(4);
	m1234.B = P.get_beta();

	return m1234;
    }


    vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const hmm_order& order)
    {
	auto t = P.t();

	const auto& nodes = order.nodes;

	int b1 = t.find_branch(nodes[0],nodes[3]);
	int b2 = t.find_branch(nodes[3],nodes[1]);
	int b3 = t.find_branch(nodes[3],nodes[4]);
	int b4 = t.find_branch(nodes[4],nodes[2]);
    
	auto a1 = convert_to_bits(P.get_pairwise_alignment(b1),0,3);
	auto a2 = convert_to_bits(P.get_pairwise_alignment(b2),3,1);
	auto a3 = convert_to_bits(P.get_pairwise_alignment(b3),3,4);
	auto a4 = convert_to_bits(P.get_pairwise_alignment(b4),4,2);

	if (order.topology.value() == 0)
	    return Glue_A(a1, Glue_A(a2, Glue_A(a3, a4)));
	else if (order.topology.value() == 1)
	    return Glue_A(a1, Glue_A(Glue_A(a3, a4), a2));
	else
	    std::abort();
    }
}
