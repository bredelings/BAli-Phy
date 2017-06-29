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

vector<dynamic_bitset<>> get_states_for_leaf_characters(const alignment& A, const TreeInterface& t)
{
    vector<dynamic_bitset<>> present(t.n_nodes(), dynamic_bitset<>(A.length()));

    // Set the bitmasks for the leaf sequences
    for(int n=0;n<t.n_nodes();n++)
    {
	auto& mask = present[n];
	if (t.is_leaf_node(n))
	{
	    for(int c=0;c<A.length();c++)
		if (A.character(c,n))
		    mask.set(c);
	}
    }

    return present;
}

vector<dynamic_bitset<>> get_states_for_all_characters(const alignment& A, const TreeInterface& t)
{
    vector<dynamic_bitset<>> present(t.n_nodes(), dynamic_bitset<>(A.length()));

    // Set the bitmasks for the leaf sequences
    for(int n=0;n<t.n_nodes();n++)
    {
	auto& mask = present[n];
	for(int c=0;c<A.length();c++)
	    if (A.character(c,n))
		mask.set(c);
    }

    return present;
}

/// Force internal node states to be consistent by connecting leaf characters
vector<dynamic_bitset<>> get_connected_states(const vector<dynamic_bitset<>>& states, const TreeInterface& t)
{
    assert(A.n_sequences() == t.n_nodes());

    // Set the bitmasks for the leaf sequences
    vector<dynamic_bitset<>> character_before_node = states;

    int root = 0;

    vector<int> branches = t.all_branches_toward_node(root);

    // 1. First determine whether there is any character at-or-before a branch.
    for(int b: branches)
    {
	int n = t.source(b);
	auto& mask = character_before_node[n];
	auto before = t.branches_before(b);

	for(int bb: before)
	    mask |= character_before_node[t.source(bb)];
    }

    // 2. Get the list of internal nodes from the root toward the leaves
    vector<int> nodes = {root};
    for(int i=branches.size()-1;i>=0;i--)
    {
	int b = branches[i];
	int n = t.source(b);
	nodes.push_back(n);
    }

    // 3. We are going to write over @character_before_node with @between_characters
    auto& between_characters = character_before_node;

    dynamic_bitset<> temp(states[0].size());
    for(int n: nodes)
    {
	auto& mask = between_characters[n];

	// If this is a leaf node then we are already done.
	if (t.is_leaf_node(n)) continue;

        // If there's a character at this node, then we count it.
	mask = states[n];
	
	// One of these branches is going to be between_characters, and the rest will be character_before_node
	auto before = t.neighbors(n);

	// If there is a character at any pair of input edges, then count it.
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

    return between_characters;
}    

/// Force internal node states are consistent by connecting leaf characters
void minimally_connect_leaf_characters(alignment& A,const TreeInterface& t)
{
    auto between_characters = get_connected_states(get_states_for_leaf_characters(A, t), t);

    for(int n = 0; n < t.n_nodes(); n++)
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

optional<int> check_characters_present(const alignment& A, const vector<dynamic_bitset<>>& present)
{
    for(int n=0; n < present.size(); n++)
	for(int c = 0; c < A.length(); c++)
	    if (present[n].test(c) and A.gap(c,n))
		return c;
    return boost::none;
}

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const TreeInterface& t,const vector<int>& ignore)
{
    // Only check if A in fact has internal node sequences.
    if (A.n_sequences() == t.n_leaves()) return;

    auto between_characters = get_connected_states(get_states_for_all_characters(A, t), t);

    auto bad_column = check_characters_present(A, between_characters);
    
    if (bad_column)
    {
	cerr<<"Internal node states are inconsistent in column "<<*bad_column<<endl;
	cerr<<A<<endl;
	throw myexception()<<"Internal node states are inconsistent in column "<<*bad_column;
    }
}

/// \brief Check if internal node characters are only present between leaf charaters.
///
/// \param A The alignment
/// \param T The tree
bool check_leaf_characters_minimally_connected(const alignment& A,const TreeInterface& t)
{
    assert(A.n_sequences() == t.n_nodes());

    auto between_characters = get_connected_states(get_states_for_leaf_characters(A, t), t);

    auto bad_column = check_characters_present(A, between_characters);
    
    return not bad_column;
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

