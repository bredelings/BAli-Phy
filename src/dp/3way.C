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
#include "bits.H"
#include "util.H"
#include "util-random.H"
#include "rng.H"

using boost::dynamic_bitset;
using std::vector;
using std::pair;

using namespace A3;

namespace A3 {

  vector<int> get_nodes(const TreeInterface& t,int n0) {
    assert(t.is_internal_node(n0));
    
    vector<int> nodes(4);
    nodes[0] = n0;
    
    vector<int> neighbors = t.neighbors(n0);
    nodes[1] = neighbors[0];
    nodes[2] = neighbors[1];
    nodes[3] = neighbors[2];
    
    return nodes;
  }
  
  vector<int> get_nodes_random(const TreeInterface& t,int n0) {
    vector<int> nodes = get_nodes(t,n0);
    
    vector<int> nodes2;
    nodes2.insert(nodes2.end(),nodes.begin()+1,nodes.end());
    random_shuffle(nodes2);
    nodes[1] = nodes2[0];
    nodes[2] = nodes2[1];
    nodes[3] = nodes2[2];
    
    return nodes;
  }

  /// Setup node names, with nodes[0]=node1 and nodes[1]=node2
  vector<int> get_nodes_branch(const TreeInterface& t,int node1,int node2) {

    assert( t.is_connected(node1,node2) );

    vector<int> nodes = get_nodes(t,node1);
    
    // make sure nodes[1] == node2
    if (node2 == nodes[1])
      ; // good
    else if (node2 == nodes[2])
      std::swap(nodes[1],nodes[2]); // 
    else if (node2 == nodes[3])
      std::swap(nodes[1],nodes[3]);
    else
      std::abort();
    
    return nodes;
  }

  /// Setup node names, with nodes[0]=node1 and nodes[1]=node2
  vector<int> get_nodes_branch_random(const TreeInterface& t,int node1,int node2) {

    vector<int> nodes = get_nodes_branch(t, node1, node2);

    // randomize the order here
    if (myrandom(2) == 1)
      std::swap(nodes[2],nodes[3]);
    
    return nodes;
  }
  
  log_double_t correction(const data_partition& P,const vector<int>& nodes) 
  {
    if (P.variable_alignment())
    {
      // get the lengths of then internal node
      int length = P.A().seqlength(nodes[0]);

      return pow(P.sequence_length_pr(length), 2);
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
