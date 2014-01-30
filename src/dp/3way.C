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
#include "math/logsum.H"
#include "util.H"
#include "util-random.H"
#include "rng.H"
#include "substitution/substitution-index.H"

using boost::dynamic_bitset;
using std::vector;
using std::pair;

using namespace A3;

vector<int> get_path_3way(const alignment& A,const vector<int>& nodes) {
  int n0 = nodes[0];
  int n1 = nodes[1];
  int n2 = nodes[2];
  int n3 = nodes[3];
  return get_path_3way(A,n0,n1,n2,n3);
}


vector<int> get_path_3way(const alignment& A,int n0,int n1,int n2,int n3) {

  //----- Store whether or not characters are present -----//
  vector<bitmask_t> present;
  for(int column=0;column<A.length();column++) {
    bitmask_t bits;
    if (not A.gap(column,n0))
      bits.set(0);
    if (not A.gap(column,n1))
      bits.set(1);
    if (not A.gap(column,n2))
      bits.set(2);
    if (not A.gap(column,n3))
      bits.set(3);
    present.push_back(bits);
  }

  int A10 = states::M;
  int A20 = states::M;
  int A30 = states::M;

  vector<int> path;
  path.reserve(A.length()+1);
  for(int column=0;column<A.length();column++) {
    bitmask_t bits = present[column];

    if (bits.none()) 
      continue;

    int states = bits_to_states(bits);
    if (states & (1<<6))
      A10 = (states>>0)&3;
    if (states & (1<<7))
      A20 = (states>>2)&3;
    if (states & (1<<8))
      A30 = (states>>4)&3;

    states |= (A30<<4)|(A20<<2)|(A10<<0);
    states = (states<<4)|bits.to_ulong();
    int S = findstate(states);
    path.push_back(S);
  }
  
  path.push_back(endstate);
  return path;
}

namespace A3 {

  vector<int> states_list = construct_states();

  vector<int> get_nodes(const Tree& T,int n0) {
    assert(T.node(n0).is_internal_node());
    
    vector<int> nodes(4);
    nodes[0] = n0;
    
    vector<const_nodeview> neighbors;
    append(T.node(n0).neighbors(),neighbors);
    nodes[1] = neighbors[0];
    nodes[2] = neighbors[1];
    nodes[3] = neighbors[2];
    
    return nodes;
  }
  
  vector<int> get_nodes_random(const Tree& T,int n0) {
    vector<int> nodes = get_nodes(T,n0);
    
    vector<int> nodes2;
    nodes2.insert(nodes2.end(),nodes.begin()+1,nodes.end());
    random_shuffle(nodes2);
    nodes[1] = nodes2[0];
    nodes[2] = nodes2[1];
    nodes[3] = nodes2[2];
    
    return nodes;
  }

  /// Setup node names, with nodes[0]=node1 and nodes[1]=node2
  vector<int> get_nodes_branch(const Tree& T,int node1,int node2) {

    assert( T.is_connected(node1,node2) );

    vector<int> nodes = get_nodes(T,node1);
    
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
  vector<int> get_nodes_branch_random(const Tree& T,int node1,int node2) {

    vector<int> nodes = get_nodes_branch(T, node1, node2);

    // randomize the order here
    if (myrandom(2) == 1)
      std::swap(nodes[2],nodes[3]);
    
    return nodes;
  }
  

  vector<bitmask_t> get_state_emit() 
  {
    vector<bitmask_t> state_emit(nstates+1);

    for(int S2=0;S2<state_emit.size();S2++)
      state_emit[S2] = states_list[S2]&bitsmask;

    return state_emit;
  }

  vector<int> construct_states() {
    vector<int> states;

    for(int i=0;i<=nstates;i++)
      states.push_back(getstates(i));

    return states;
  }

  using indel::PairHMM;

  vector<double> get_start_P(const vector<indel::PairHMM>& P) {
    int count = 0;
    double sum = 0;

    vector<double> start_P(nstates,0.0);
    for(int S=0;S<start_P.size();S++) {
      int states = states_list[S];
      int s1 = (states>>4)&3;
      int s2 = (states>>6)&3;
      int s3 = (states>>8)&3;
      
      if (s1 == states::E or s2 == states::E or s3 == states::E)
	continue;
      
      // if we are using hidden states, only use one way
      if (s1 == states::G1) {
	if (not bitset(states,10)) continue;
      }
      else if (s2 == states::G1) {
	if (not bitset(states,11)) continue;
      }
      else if (s3 == states::G1) {
	if (not bitset(states,12)) continue;
      }
      
      start_P[S] = P[0].start_pi(s1) * P[1].start_pi(s2) * P[2].start_pi(s3);
      count++;
      sum += start_P[S];
    }    
    // check that the sum of Matrices[S](0,0) is 1, number of states examined is 27
#ifndef NDEBUG
    std::cerr<<"sum = "<<sum<<std::endl;
#endif
    assert(count==27);

    return start_P;
  }

  /// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
  int bits_to_states(bitmask_t bits) {
    int S = (1<<6)|(1<<7)|(1<<8);
    if (not bits.test(0)) {
      if (bits.test(1))
	S |= (states::G1<<0);
      else
	S = clearbit(S,6);

      if (bits.test(2))
	S |= (states::G1<<2);
      else
	S = clearbit(S,7);

      if (bits.test(3))
	S |= (states::G1<<4);
      else
	S = clearbit(S,8);
    }
    else {
      if (bits.test(1))
	S |= (states::M<<0);
      else
	S |= (states::G2<<0);
      
      if (bits.test(2))
	S |= (states::M<<2);
      else
	S |= (states::G2<<2);

      if (bits.test(3))
	S |= (states::M<<4);
      else
	S |= (states::G2<<4);
    }
    return S;
  }

  int getstates(int S) {
    assert(0 <= S and S<nstates+1);

    if (S==endstate)
      return (1<<12)|(1<<11)|(1<<10)|(states::E<<8)|(states::E<<6)|(states::E<<4);

    int bits=0;

    //--------- Set bits ---------
    if (S<8) {
      //internal node present
      bits |= (1<<0);

      // other nodes present according to S
      bits |= ((~S)&7)<<1;
    }
    else {
      S -=8;
      if (S/9 == 0) 
	bits |= (1<<1);
      else if (S/9 == 1) 
	bits |= (1<<2);
      else if (S/9 == 2) 
	bits |= (1<<3);
      S = S%9;
    }

    //-------- Get states --------
    int states = bits_to_states(bits);
    for(int i=0;i<3;i++)
      if (not bitset(states,6+i)) {
	int s = S%3;
	S /= 3;
	states |= (s<<(2*i));
      }

    //---------- Merge ----------
    return (states<<4)|bits;
  }

  inline int findstate(int states) {
    unsigned int mask = ~((~0)<<10);
    for(int S=0;S<=nstates;S++) {
      if ((states_list[S]&mask) == (states&mask))
	return S;
    }
    //couldn't find it?
    throw myexception()<<__PRETTY_FUNCTION__<<": couldn't find state";
  }

  using namespace A3;



  inline double getQ(int S1,int S2,const vector<indel::PairHMM>& P)
  {
    assert(0 <= S1 and S1 < nstates+1);
    assert(0 <= S2 and S2 < nstates+1);

    int states1 = states_list[S1];
    int states2 = states_list[S2];

    int ap1 = states1>>10;
    int ap2 = states2>>10;

    // If states are unordered, then force numerical order
    //  - this means that sequence 3 comes first
    // Note that the end state IS ordered - but this be handled here.
    if (not (ap1 & ap2) and (ap1>ap2))
      return 0.0;

    double Pr=1;
    for(int i=0;i<3;i++) {
      int s1 = (states1>>(2*i+4))&3;
      int s2 = (states2>>(2*i+4))&3;
      if (bitset(states2,10+i))     // this sub-alignment is present in this column
	Pr *= P[i](s1,s2);
      else if (s1 != s2)            // require state info from s1 hidden in s2
	return 0.0;
    }

    if (S1==endstate)
      assert(Pr==0.0);

    return Pr;
  }

  Matrix createQ(const vector<indel::PairHMM>& P) 
  {
    Matrix Q(nstates+1,nstates+1);

    for(int i=0;i<Q.size1();i++)
      for(int j=0;j<Q.size2();j++)
	Q(i,j) = getQ(i,j,P);

    return Q;
  }


  // Does this routine depend on order of unordered columns?
  //  - No: columns in subA1 but not in seq1 are ordered only in respect to columns in subA1
  //  - columns in seq1, seq2, and seq3 should remain in increasing order.

  alignment construct(const alignment& old, const vector<int>& path, 
		      int n0,int n1,int n2,int n3,const Tree& T,
		      const vector<int>& seq1,const vector<int>& seq2, const vector<int>& seq3) {

    vector< vector<int> > seq;
    seq.push_back(seq1); seq.push_back(seq2); seq.push_back(seq3);

    vector< dynamic_bitset<> > group;
    group.push_back( T.partition(n0,n1) );
    group.push_back( T.partition(n0,n2) );
    group.push_back( T.partition(n0,n3) );

    vector< vector<int> > subA(seq.size());

    for(int column=0;column<old.length();column++) {
      // We have no guarantee about the consistancy of the columns before in the 
      //  old alignment.  We will construct a consistant alignment from the path.
      for(int i=0;i<seq.size();i++)
	if (not all_gaps(old,column,group[i]))
	  subA[i].push_back(column);
    }

  
    // Account for silent end state with "-1"
    int newlength = path.size() - 1;
    // Add in columns not present in the 5way alignment
    for(int i=0;i<seq.size();i++)
      newlength += (subA[i].size() - seq[i].size());

    alignment A = blank_copy(old,newlength);
    assert(A.length() == newlength);

    int l=0;
    // position in sequence
    vector<int> cS(seq.size(),0);
    // position in sub-alignment
    vector<int> cA(seq.size(),0);
    for(int column=0;column<A.length();column++) {
      //      std::cerr<<column<<":  "<<cA[0]<<" "<<cS[0]<<"  "<<cA[1]<<" "<<cS[1]<<"   "<<cA[2]<<"  "<<cS[2]<<"  "<<l<<endl;

      //--------------- Check that we're putting in all the subA columns ---------------//
      for(int i=0;i<seq.size();i++) {
	assert(cA[i] >= cS[i]);
	assert(cA[i] <= subA[i].size());
      }
      //--------------- Do we have any columns subA[i] to insert? ---------------------//
      bool done=false;
      for(int i=0;i<seq.size();i++) {
	if (cA[i] < subA[i].size() and 
	    (cS[i] == seq[i].size() or 
	     (cS[i] < seq[i].size() and subA[i][cA[i]] != seq[i][cS[i]]))) {
	  for(int s=0;s<A.n_sequences();s++){
	    if (group[i][s])
	      A.set_value(column,s, old(subA[i][cA[i]],s) );
	    else
	      A.set_value(column,s, alphabet::gap );
	  }
	  cA[i]++;
	  done = true;
	  break;
	}
      }
      if (done) continue;
      
      //----------------- Insert a column corresponding to path[l] -------------------//
      bitmask_t bits = states_list[path[l]] & bitsmask;
      for(int s=0;s<A.n_sequences();s++) 
	A.set_value(column,s, alphabet::gap );
      
      for(int s=0;s<A.n_sequences();s++) {
	if (s == n0) {
	  if (dl(path[l]))
	    A.set_value(column,s, alphabet::not_gap);
	}
	else {
	  // which group is sequence 's' in?
	  int j = -1;
	  for(int i=0;i<group.size();i++)
	    if (group[i][s]) {
	      j = i;
	      break;
	    }
	  assert(j != -1);

	  // copy from the  correct column, based on the group 'j'
	  if (bits.test(1+j))
	    A.set_value(column,s, old(seq[j][cS[j]],s) );
	}
      }

      for(int i=0;i<seq.size();i++) {
	if (bits.test(1+i)) {
	  cS[i]++;
	  cA[i]++;
	}
      }
      l++;

      //    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl<<endl;
      assert(not all_gaps(A,column));
    }

    for(int i=0;i<seq.size();i++) {
      assert(cA[i] == subA[i].size());
      assert(cS[i] == seq[i].size());
    }
    assert(l == path.size()-1);

    for(int i=0;i<T.n_leaves();i++) 
      assert(A.seqlength(T.leaf_node(i)) == old.seqlength(T.leaf_node(i)));

    //  std::cerr<<"new = "<<A<<endl;  
    //  std::cerr<<"new(reordered) = "<<project(A,n0,n1,n2,n3)<<endl;
    assert(valid(A));

    return A;
  }

  // If we are just getting the order of the columns in the 3-way alignment
  // the this shouldn't affect anything else, should it??

  // The reason we must look at alignments is that +/- and -/+ ARE ordered
  // inside pairwise alignments.

  // What happens if we care about alignments that aren't part of the 3way?
  // Does this block stuff?  I think it did...
  vector<pair<int,int> > getorder2(const alignment& A,int n0,int n1,int n2,int n3) {

    vector<pair<int,int> > columns;
    vector<int> AP;                     // alignments present

    //----- Record which sub-alignments present per column ------//
    for(int column=0;column<A.length();column++) 
    {
      int bits=0;
      if (not A.gap(column,n0))
	bits |= (1<<0);
      if (not A.gap(column,n1))
	bits |= (1<<1);
      if (not A.gap(column,n2))
	bits |= (1<<2);
      if (not A.gap(column,n3))
	bits |= (1<<3);

      int states = bits_to_states(bits);
      int ap = states>>6;

      if (ap)
      {
	AP.push_back(ap);
	columns.push_back( pair<int,int>(column,-1) );
      }
    }

    //-------- Re-order unordered columns by AP order ---------//
    vector<int> order = iota((int)columns.size());

    for(int i=0;i+1<order.size();) 
    {
      int ap1 = AP[order[i  ]];
      int ap2 = AP[order[i+1]];
      if (not (ap1&ap2) and ap1 > ap2) {
	std::swap(order[i],order[i+1]);
	if (i>0) i--;
      }
      else
	i++;
    }

    //------- Place indices into sorted column list -------//
    for(int i=0;i<order.size();i++)
      columns[order[i]].second = i;
    for(int i=0;i<columns.size();i++)
      assert(columns[i].second >= 0);

    return columns;
  }

  // If we are just getting the order of the columns in the 3-way alignment
  // the this shouldn't affect anything else, should it??

  // The reason we must look at alignments is that +/- and -/+ ARE ordered
  // inside pairwise alignments.

  // What happens if we care about alignments that aren't part of the 3way?
  // Does this block stuff?  I think it did...
  vector<int> getorder(const alignment& A,int n0,int n1,int n2,int n3) {

    vector<int> columns;
    vector<int> AP;                     // alignments present

    //----- Record which sub-alignments present per column ------//
    for(int column=0;column<A.length();column++) {
      int bits=0;
      if (not A.gap(column,n0))
	bits |= (1<<0);
      if (not A.gap(column,n1))
	bits |= (1<<1);
      if (not A.gap(column,n2))
	bits |= (1<<2);
      if (not A.gap(column,n3))
	bits |= (1<<3);

      int states = bits_to_states(bits);
      int ap = states>>6;
      AP.push_back(ap);
      if (ap) {
	columns.push_back(column);
      }
    }

    //-------- Re-order unordered columns by AP order ---------//
    for(int i=0;i+1<columns.size();) {
      int ap1 = AP[columns[i  ]];
      int ap2 = AP[columns[i+1]];
      if (not (ap1&ap2) and ap1 > ap2) {
	std::swap(columns[i],columns[i+1]);
	if (i>0) i--;
      }
      else
	i++;
    }

    assert(columns == convert_to_column_list(getorder2(A,n0,n1,n2,n3)));
    return columns;
  }

  alignment project(const alignment& A,const vector<int>& nodes) {
    int n0 = nodes[0];
    int n1 = nodes[1];
    int n2 = nodes[2];
    int n3 = nodes[3];
    return project(A,n0,n1,n2,n3);
  }


  alignment project(const alignment& A1,int n0,int n1,int n2,int n3) {
    alignment A2(A1.get_alphabet());
    A2.add_sequence(A1.seq(n0));
    A2.add_sequence(A1.seq(n1));
    A2.add_sequence(A1.seq(n2));
    A2.add_sequence(A1.seq(n3));

    vector<int> columns = getorder(A1,n0,n1,n2,n3);
    A2.changelength(columns.size());
    for(int i=0;i<A2.length();i++) {
      A2.set_value(i,0, A1(columns[i],n0));
      A2.set_value(i,1, A1(columns[i],n1));
      A2.set_value(i,2, A1(columns[i],n2));
      A2.set_value(i,3, A1(columns[i],n3));
    }

    return A2;
  }

  efloat_t correction(const data_partition& P,const vector<int>& nodes) 
  {
    if (P.variable_alignment())
    {
      // get the lengths of then internal node
      int length = P.A->seqlength(nodes[0]);

      return pow(P.sequence_length_pr(length), 2);
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

}
