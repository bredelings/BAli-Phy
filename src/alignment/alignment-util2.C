/*
  Copyright (C) 2004-2009 Benjamin Redelings

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
/// \file alignment-util.C
///
/// \brief This file implements alignment utility functions.
///

#include <boost/optional.hpp>
#include "alignment-util.H"
#include "alignment-util2.H"
#include "util.H"
#include "setup.H"
#include "io.H"

using std::string;
using std::vector;
using std::valarray;
using std::cout;
using std::cerr;
using std::endl;
using std::istream;

using boost::dynamic_bitset;
using boost::optional;

using boost::program_options::variables_map;

/// Check that any two present nodes are connected by a path of present nodes
bool all_characters_connected(const TreeInterface& t, const vector<dynamic_bitset<>>& partitions,
			      dynamic_bitset<> present,const vector<int>& _ignore)
{
    assert(present.size() == t.n_nodes());

    //--------- set the ignored nodes to 'not present' -----------//
    dynamic_bitset<> ignore(present.size());
    for(int i=0;i<_ignore.size();i++) {
	int n = _ignore[i];
	present[n] = false;
	ignore[n] = true;
    }

    //---------- for each internal node... -------------//
    for(int n1=t.n_leaves(); n1<t.n_nodes(); n1++) {

	if (present[n1] or ignore[n1]) continue;
      
	//------- if it is '-' and not ignored ... -------//
	vector<int> neighbors = t.neighbors(n1);
	assert(neighbors.size() == 3);

	//---- check the three attatched subtrees ... ----//
	int total=0;
	for(int i=0;i<neighbors.size();i++) {
	    dynamic_bitset<> group = partitions[t.find_branch(n1,neighbors[i])];
	    if (present.intersects(group))
		total++;
	}

	//----- nodes should be present in only one. -----//
	if (total > 1)
	    return false;
    }
    return true;
}


void connect_all_characters(const TreeInterface& t, const vector<dynamic_bitset<>>& partitions, dynamic_bitset<>& present)
{
    assert(present.size() == t.n_nodes());
  
    //---------- for each internal node... -------------//
    for(int n1=t.n_leaves(); n1<t.n_nodes(); n1++) 
    {
	if (present[n1]) continue;

	//------- if it is '-' and not ignored ... -------//
	vector<int> neighbors = t.neighbors(n1);
	assert(neighbors.size() == 3);

	//---- check the three attatched subtrees ... ----//
	int total=0;
	for(int i=0;i<neighbors.size();i++)
	{
	    dynamic_bitset<> group = partitions[t.find_branch(n1,neighbors[i])];
	    if (present.intersects(group))
		total++;
	}

	if (total > 1)
	    present[n1] = true;
    }
    assert(all_characters_connected(t,partitions,present,vector<int>()));
}

/// \brief Check if internal node characters are only present between leaf charaters.
///
/// \param A The alignment
/// \param T The tree
bool check_leaf_characters_minimally_connected(const alignment& A,const TreeInterface& t)
{
    assert(A.n_sequences() == t.n_nodes());

    auto partitions = get_partitions(t);
    for(int column=0;column<A.length();column++)
    {
	// construct leaf presence/absence mask
	dynamic_bitset<> present(t.n_nodes());
	for(int i=0;i<t.n_leaves();i++)
	    present[i] = not A.gap(column,i);
    
	// compute presence/absence for internal nodes
	connect_all_characters(t, partitions, present);

	// put present characters into the alignment.
	for(int i=t.n_leaves();i<t.n_nodes();i++)
	    if (present[i] != A.character(column,i))
		return false;
    }
    return true;
}

optional<int> find_internal_node(const TreeInterface& t)
{
    for(int n=0;n<t.n_nodes();n++)
	if (t.is_internal_node(n)) return n;

    return boost::none;
}

/// Force internal node states are consistent by connecting leaf characters
void minimally_connect_leaf_characters(alignment& A,const TreeInterface& t)
{
    assert(A.n_sequences() == t.n_nodes());

    auto root_ = find_internal_node(t);
    if (not root_) return;
    int root = *root_;

    vector<int> branches = t.all_branches_toward_node(root);

    // Set the bitmasks for the leaf sequences
    vector<dynamic_bitset<>> character_before_node(A.n_sequences(), dynamic_bitset<>(A.length()));

    // 1. First determine whether there is any character before a branch.
    for(int b: branches)
    {
	int n = t.source(b);
	auto& mask = character_before_node[n];
	auto before = t.branches_before(b);

	if (before.empty())
	{
	    for(int c=0;c<A.length();c++)
		if (A.character(c,n))
		    mask.set(c);
	}
	else
	{
	    for(int bb: before)
		mask |= character_before_node[t.source(bb)];
	}
	  
    }

    // 2. Get the list of internal nodes from the root toward the leaves
    vector<int> nodes = {root};
    for(int i=branches.size()-1;i>=0;i--)
    {
	int b = branches[i];
	int n = t.source(b);
	if (t.is_leaf_node(n)) continue;
	nodes.push_back(n);
    }

    // 3. We are going to write over @character_before_node with @between_characters
    auto& between_characters = character_before_node;

    dynamic_bitset<> temp(A.length());
    for(int n: nodes)
    {
	auto& mask = between_characters[n];

	// One of these branches is going to be between_characters, and the rest will be character_before_node
	auto before = t.neighbors(n);

	// Consider all pairs of edges
	for(int i=0;i<before.size();i++)
	{
	    int n1 = before[i];
	    for(int j=0;j<i;j++)
	    {
		int n2 = before[j];
		temp = between_characters[n1];
		temp &= between_characters[n2];
		mask |= temp;
		// mask |= (between_characers[n1] & between_characters[n2]);
	    }
	}
    }

    // 4. Copy new bitmasks to alignment matrix
    // FIXME -- ideally we could keep this as a bitmask...
    for(int n: nodes)
    {
	auto& present = between_characters[n];

	for(int column=0;column<A.length();column++)
	{
	    // put present characters into the alignment.
	    if (present[column])
		A.set_value(column, n, alphabet::not_gap );
	    else
		A.set_value(column, n, alphabet::gap );
	}
    }
}    

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const TreeInterface& t,const vector<int>& ignore)
{
    // Only check if A in fact has internal node sequences.
    if (A.n_sequences() == t.n_leaves()) return;

    assert(A.n_sequences() == t.n_nodes());

    auto partitions = get_partitions(t);
    for(int column=0;column<A.length();column++) {
	dynamic_bitset<> present(t.n_nodes());
	for(int i=0;i<t.n_nodes();i++) 
	    present[i] = not A.gap(column,i);
    
	if (not all_characters_connected(t,partitions,present,ignore)) {
	    cerr<<"Internal node states are inconsistent in column "<<column<<endl;
	    cerr<<A<<endl;
	    throw myexception()<<"Internal node states are inconsistent in column "<<column;
	}
    }
}

void check_alignment(const alignment& A,const TreeInterface& t,bool internal_sequences) 
{
    // First check that there are no illegal letters
    check_letters_OK(A);

    // Next check that the internal sequences haven't changed
    check_leaf_sequences(A,t.n_leaves());

    if (not internal_sequences) return;

    // Next check that only N/X and - are found at internal nodes
    check_internal_sequences_composition(A,t.n_leaves());
  
    // Finally check that the internal node states are consistent
    check_internal_nodes_connected(A,t);
}

