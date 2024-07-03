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
/// \file 3way.C
///
/// \brief Defines the HMM for three pairwise alignments on adjacent branches of a tree.
///

#include <algorithm>
#include "3way.H"
#include "2way.H"
#include "bits.H"
#include "util/permute.H"
#include "util/rng.H"
#include "models/parameters.H"

using boost::dynamic_bitset;
using std::vector;
using std::pair;

using namespace A3;

namespace A3 {

  vector<int> get_nodes(const TreeInterface& t,int n0)
  {
    assert(t.degree(n0) == 3);
    
    vector<int> nodes;
    nodes.push_back(n0);
    
    for(int node: t.neighbors(n0))
	nodes.push_back(node);
    
    return nodes;
  }
  
  vector<int> get_nodes_random(const TreeInterface& t,int n0)
  {
    vector<int> nodes;
    nodes.push_back(n0);
    
    auto neighbors = t.neighbors(n0);
    random_shuffle(neighbors);
    for(int node: neighbors)
	nodes.push_back(node);
    
    return nodes;
  }

  /// Setup node names, with nodes[0]=node1 and nodes[1]=node2
  vector<int> get_nodes_branch(const TreeInterface& t,int node1,int node2) {

    assert( t.is_connected(node1,node2) );

    vector<int> nodes = get_nodes(t,node1);

    for(int i=2;i<nodes.size();i++)
	if (nodes[i] == node2)
	    std::swap(nodes[i], nodes[1]);

    assert(nodes[0] == node1);
    assert(nodes[1] == node2);
    
    return nodes;
  }

  /// Setup node names, with nodes[0]=node1 and nodes[1]=node2
  vector<int> get_nodes_branch_random(const TreeInterface& t,int node1,int node2)
  {
    vector<int> nodes = get_nodes_random(t, node1);

    for(int i=2;i<nodes.size();i++)
	if (nodes[i] == node2)
	    std::swap(nodes[i], nodes[1]);

    assert(nodes[0] == node1);
    assert(nodes[1] == node2);

    return nodes;
  }
  
  log_double_t correction(const data_partition& P,const vector<int>& nodes) 
  {
    if (P.variable_alignment())
    {
      return pow(P.sequence_length_pr(nodes[0]), 2);
    }
    else
      return 1;
  }

  log_double_t correction(const Parameters& P,const vector<int>& nodes) 
  {
    log_double_t C = 1.0;
    for(int i=0;i<P.n_data_partitions();i++)
      C *= correction(P[i],nodes);
    return C;
  }
    
  log_double_t acceptance_ratio(const Parameters& P1,const vector<int>& nodes1,
			      const Parameters& P2,const vector<int>& nodes2) 
  {
    return correction(P1,nodes1)/correction(P2,nodes2);
  }

  vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const vector<int>& nodes)
  {
    auto t = P.t();

    assert(nodes.size() == 4);

    int b1 = t.find_branch(nodes[1],nodes[0]);
    int b2 = t.find_branch(nodes[0],nodes[2]);
    int b3 = t.find_branch(nodes[0],nodes[3]);

    vector<HMM::bitmask_t> a1 = convert_to_bits(P.get_pairwise_alignment(b1),0,3);
    vector<HMM::bitmask_t> a2 = convert_to_bits(P.get_pairwise_alignment(b2),3,1);
    vector<HMM::bitmask_t> a3 = convert_to_bits(P.get_pairwise_alignment(b3),3,2);

    vector<HMM::bitmask_t> a123 = Glue_A(a1, Glue_A(a2, a3));

    return a123;
  }
}
