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

  vector<int> states_list = construct_states();

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
    if (myrandomf() < 0.5)
      std::swap(nodes[0],nodes[1]);
    if (myrandomf() < 0.5)
      std::swap(nodes[2],nodes[3]);
    if (myrandomf() < 0.5) {
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

  vector<pair<int,int> > getorder2(const alignment& A,const vector<int>& nodes) 
  {
    vector<pair<int,int> > columns;
    vector<int> AP;

    //----- Record which sub-alignments present per column ------//
    for(int column=0;column<A.length();column++) {
      int bits = 0;
      for(int i=0;i<nodes.size();i++)
	if (not A.gap(column,nodes[i]))
	  bits |= (1<<i);
    
      int states = bits_to_substates(bits);
      int ap = states>>(5*2);
      if (ap)
      {
	AP.push_back(ap);
	columns.push_back(pair<int,int>(column,-1));
      }
    }

    //-------- Re-order unordered columns by AP order ---------//
    vector<int> order = iota((int)columns.size());

    for(int i=0;i+1<order.size();) {
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

  vector<int> getorder(const alignment& A,const vector<int>& nodes) {
    vector<int> columns;
    vector<int> AP;

    //----- Record which sub-alignments present per column ------//
    for(int column=0;column<A.length();column++) {
      int bits = 0;
      for(int i=0;i<nodes.size();i++)
	if (not A.gap(column,nodes[i]))
	  bits |= (1<<i);
    
      int states = bits_to_substates(bits);
      int ap = states>>(5*2);
      AP.push_back(ap);
      if (ap)
	columns.push_back(column);
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

    assert(columns == convert_to_column_list(getorder2(A,nodes)));
    return columns;
  }

  /// Get the 5way path for A at nodes
  vector<int> get_path(const alignment& A,const vector<int>& nodes,const vector<int>& states) {
    const int endstate = states.size()-1;

    //----- Store whether or not characters are present -----//
    vector<int> present;
    for(int column=0;column<A.length();column++) {
      int bits=0;
      for(int i=0;i<nodes.size();i++)
	if (not A.gap(column,nodes[i]))
	  bits |= (1<<i);
      present.push_back(bits);
    }

    int A04 = states::M;
    int A14 = states::M;
    int A25 = states::M;
    int A35 = states::M;
    int A45 = states::M;

    vector<int> path;
    path.reserve(A.length()+1);
    for(int column=0;column<A.length();column++) {
      int bits = present[column];

      if (not bits) 
	continue;

      int substates = bits_to_substates(bits);
      if (substates & (1<<10))
	A04 = (substates>>0)&3;
      if (substates & (1<<11))
	A14 = (substates>>2)&3;
      if (substates & (1<<12))
	A25 = (substates>>4)&3;
      if (substates & (1<<13))
	A35 = (substates>>6)&3;
      if (substates & (1<<14))
	A45 = (substates>>8)&3;

      substates |= (A45<<8)|(A35<<6)|(A25<<4)|(A14<<2)|(A04<<0);
      int state = (substates<<6)|bits;
      int S = findstate(state,states);
      path.push_back(S);
    }
  
    path.push_back(endstate);
    return path;
  }

  int findstate(int state,const vector<int>& states) {
    for(int i=0;i<states.size();i++)
      if (states[i] == state)
	return i;
    throw myexception()<<__PRETTY_FUNCTION__<<": couldn't find state";
  }

  inline int bits_to_state(int bits,int b1,int b2) {
    if (not bitset(bits,b1)) {
      if (bitset(bits,b2))
	return states::G1;
      else
	return 3;
    }
    else {
      if (bitset(bits,b2))
	return states::M;
      else
	return states::G2;
    }
  }

  // Takes a bitmask of character-presence in sequences 0,1,2,3,4,5
  // Returns the state for each alignment, 
  // with the validity of sub-alignments 0,1,2,3,4 marked in bits 10,11,12,13,14
  inline int bits_to_substates(int bits) {
    int S=0;
    S |= bits_to_state(bits,4,0)<<0;
    S |= bits_to_state(bits,4,1)<<2;
    S |= bits_to_state(bits,5,2)<<4;
    S |= bits_to_state(bits,5,3)<<6;
    S |= bits_to_state(bits,4,5)<<8;
    
    for(int i=0;i<5;i++) {
      if (bits_present(S>>(2*i),3))   // If alignment i is missing,
	S &= ~(3<<(2*i));             //  clear its state
      else                            // Otherwise 
	S = setbit(S,10+i);           //  mark it as present in this column.
    }
    return S;
  }
	
  bool bits_match_substates(int bits,int substates) {
    // What states for the subalignments are required by the bits?
    int substates2 = bits_to_substates(bits);
    
    // Check each of the subalignments for a valid substate
    bool valid = true;
    for(int i=0;i<5 and valid;i++) {
	  
      // If the state is E then its not valid
      int ss1 = (substates>>(2*i))&3;
      if (ss1 == 3) 
	valid = false;
	  
      // If the subalignment is present in this column
      // but doesn't match what the bits say, then its not valid
      if (not bitset(substates2,10+i)) continue;
      int ss2 = (substates2>>(2*i))&3;
      if (ss1 != ss2)
	valid = false;
    }
	
    return valid;
  }


  /// Are all the bits connected?
  bool legal(int bits) {
    int nl = 0;
    int nr = 0;
    if (bitset(bits,0)) nl++;
    if (bitset(bits,1)) nl++;

    if (bitset(bits,2)) nr++;
    if (bitset(bits,3)) nr++;
  
    // Look at all paths connecting leaf nodes
    if (nl == 2 and not bitset(bits,4)) return false;
    if (nr == 2 and not bitset(bits,5)) return false;
    if (nl and nr and not (bitset(bits,4) and bitset(bits,5))) return false;

    // Look at all paths connecting leaf and internal nodes
    if (nl and bitset(bits,5) and not bitset(bits,4)) return false;
    if (nr and bitset(bits,4) and not bitset(bits,5)) return false;

    return true;
  }

  /// Construct a list of states (this also assigns them names)
  vector<int> construct_states() {
    vector<int> states;

    // Consider all bit patterns except "no bits anywhere"
    for(int bits=1;bits<(1<<6);bits++)
      if (legal(bits))
	for(int substates=0;substates<(1<<10);substates++)
	  if (bits_match_substates(bits,substates)) {
	    int ss = bits_to_substates(bits);
	    ss |= substates;
	    states.push_back(bits | (ss<<6));
	  }

    
    // Now add the E state - empty, but all the subA's are present
    int estate = 0;
    for(int i=0;i<5;i++) {
      estate |= (states::E<<(2*i));
      estate |= (1<<(10+i));
    }
    estate <<= 6;
    states.push_back(estate);

    return states;
  }


  /// Get the vector of start probabilities
  vector<double> get_start_P(const vector<indel::PairHMM>& P)
  {
    int count = 0;
    double sum = 0.0;

    vector<double> start_P(states_list.size()-1,0.0);
    int allmatch = (bits_to_substates(63)<<6)|63;
    allmatch = findstate(allmatch,states_list);

    start_P[allmatch] = 1.0;
    return start_P;

    //FIXME - the general case is difficult - won't do it now
    for(int i=0;i<3*3*3*3*3;i++) {
      int S=i;
      int s1 = S%3; S/= 3;
      int s2 = S%3; S/= 3;
      int s3 = S%3; S/= 3;
      int s4 = S%3; S/= 3;
      int s5 = S%3; S/= 3;

      int substates = (s5<<8)|(s4<<6)|(s3<<4)|(s2<<2)|(s1<<0);
      int bits = -1;
      for(int i=0;i<(1<<6);i++) {
	if (bits_match_substates(bits,substates)) {
	  bits = i;
	  break;
	}
      }
      assert(bits != -1);
      int state = findstate(bits|(substates<<6),states_list);
      start_P[state] = P[0].start_pi(s1) * P[1].start_pi(s2) * P[2].start_pi(s3) 
	* P[3].start_pi(s4) * P[4].start_pi(s5);

      count++;
      sum += start_P[S];
    }
    // check that the sum of Matrices[S](0,0) is 1, number of states examined is 27
#ifndef NDEBUG
    std::cerr<<"sum = "<<sum<<std::endl;
#endif
    assert(count==243);

    return start_P;
  }

  /// Compute the probability of moving from state #S1 to state #S2
  double getQ(int S1,int S2,const vector<indel::PairHMM>& P,const vector<int>& states) 
  {
    int endstate = states.size()-1;

    assert(0 <= S1 and S1 < states.size());
    assert(0 <= S2 and S2 < states.size());

    // Get the substate info for the states
    int states1 = states[S1]>>6;
    int states2 = states[S2]>>6;

    // And note which alignments are present
    int ap1 = states1>>10;
    int ap2 = states2>>10;

    // If states are unordered, then force numerical order
    //  - this means that sequence 3 comes first
    // Note that the end state IS ordered - but this be handled here.
    if (not (ap1 & ap2) and (ap1>ap2))
      return 0.0;

    double Pr=1.0;
    for(int i=0;i<5;i++) {
      int s1 = (states1>>(2*i))&3;
      int s2 = (states2>>(2*i))&3;
      if (bitset(ap2,i))            // this sub-alignment is present in this column
	Pr *= P[i](s1,s2);
      else if (s1 != s2)            // require state info from s1 hidden in s2
	return 0.0;
    }

    if (S1==endstate)
      assert(Pr==0.0);

    return Pr;
  }

  /// Create the full transition matrix
  void updateQ(Matrix& Q,const vector<indel::PairHMM>& P,const vector<int>& states) 
  {
    for(int i=0;i<Q.size1();i++)
      for(int j=0;j<Q.size2();j++)
	if (Q(i,j) > 0.0)
	  Q(i,j) = getQ(i,j,P,states);
  }

  /// Create the full transition matrix
  void fillQ(Matrix& Q,const vector<indel::PairHMM>& P,const vector<int>& states) 
  {
    for(int i=0;i<Q.size1();i++)
      for(int j=0;j<Q.size2();j++)
	Q(i,j) = getQ(i,j,P,states);
  }

  /// Create the full transition matrix
  Matrix createQ(const vector<indel::PairHMM>& P,const vector<int>& states) 
  {
    Matrix Q(states.size(),states.size());

    fillQ(Q,P,states);

    return Q;
  }


  // Does this routine depend on order of unordered columns?
  //  - No: columns in subA1 but not in seq1 are ordered only in respect to columns in subA1
  //  - columns in seq1, seq2, and seq3 should remain in increasing order.

  alignment construct(const alignment& old, const vector<int>& path, 
		      const vector<int>& nodes,const Tree& T,const vector< vector<int> >& seq,const  vector<int>& states_list) {

    // Construct the list of nodes present in the 4 sub-trees
    vector< dynamic_bitset<> > group;
    group.push_back(T.partition(nodes[4],nodes[0]));
    group.push_back(T.partition(nodes[4],nodes[1]));
    group.push_back(T.partition(nodes[5],nodes[2]));
    group.push_back(T.partition(nodes[5],nodes[3]));

    // Construct the list of columns present in the 4 sub-alignments
    vector< vector<int> > subA(seq.size());
    for(int i=0;i<subA.size();i++)
      subA[i].reserve(old.length());
    for(int column=0;column<old.length();column++) {
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

    int l=0;                            // position in path
    vector<int> cS(seq.size(),0);       // position in sequence
    vector<int> cA(seq.size(),0);       // position in sub-alignment

    for(int column=0;column<A.length();column++) {
      //    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl;

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
      int bits = states_list[path[l]] & bitsmask;

      for(int s=0;s<A.n_sequences();s++) 
	A.set_value(column,s, alphabet::gap );

      for(int s=0;s<A.n_sequences();s++) {
	if (s == nodes[4]) {
	  if (bitset(bits,4))
	    A.set_value(column,s, alphabet::not_gap );
	}
	else if (s == nodes[5]) {
	  if (bitset(bits,5))
	    A.set_value(column,s, alphabet::not_gap );
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
	  if (bitset(bits,j))
	    A.set_value(column,s, old(seq[j][cS[j]],s) );
	}
      }

      for(int i=0;i<seq.size();i++) {
	if (bitset(bits,i)) {
	  cS[i]++;
	  cA[i]++;
	}
      }
      l++;

      assert(not all_gaps(A,column));

      //    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl<<endl;
      assert(not all_gaps(A,column));
    }

    for(int i=0;i<seq.size();i++) {
      assert(cA[i] == subA[i].size());
      assert(cS[i] == seq[i].size());
    }
    assert(l == path.size()-1);

    for(int s=0;s<T.n_leaves();s++) 
      assert(A.seqlength(T.leaf_node(s)) == old.seqlength(T.leaf_node(s)));

    //  std::cerr<<"new = "<<A<<endl;  
    //  std::cerr<<"new(reordered) = "<<project(A,n0,n1,n2,n3)<<endl;
    //    std::cerr<<"A5::construct - ";
    //    for(int i=0;i<nodes.size();i++)
    //      std::cerr<<"n"<<i<<" = "<<nodes[i]<<"  ";
    //    std::cerr<<"\n";
    assert(valid(A));

    return A;
  }

  alignment project(const alignment& A1,const vector<int>& nodes) {
    alignment A2(A1.get_alphabet());

    for(int i=0;i<nodes.size();i++)
      A2.add_sequence(A1.seq(nodes[i]));

    vector<int> columns = getorder(A1,nodes);
    A2.changelength(columns.size());
    for(int column=0;column<A2.length();column++) {
      for(int i=0;i<nodes.size();i++) 
	A2.set_value(column,i, A1(columns[column],nodes[i]) );
    }

    return A2;
  }

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
}
