/*
  Copyright (C) 2004-2009,2017 Benjamin Redelings

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

#include "alignment-util.H"

#include <boost/program_options.hpp>
#include "util/set.H"
#include "util/io.H"
#include "util/range.H"
#include "tree/tree.H"
#include "tree/sequencetree.H"

using std::string;
using std::vector;
using std::valarray;
using std::cout;
using std::cerr;
using std::endl;
using std::istream;

using boost::dynamic_bitset;
using std::optional;

using boost::program_options::variables_map;

alignment chop_internal(alignment A, int n_leaves)
{
    return reorder_sequences(A,iota<int>(n_leaves));
}

alignment chop_internal(alignment A, const SequenceTree& T)
{
    return chop_internal(A, T.n_leaves());
}

alignment chop_internal(alignment A, bool keep_empty_columns) 
{
    int N = (A.n_sequences()+2)/2;

    bool internal_nodes = true;
    for(int i=N;i<A.n_sequences();i++) {
	if (A.seq(i).name.size() == 0 or A.seq(i).name[0] != 'A') {
	    internal_nodes = false; 
	    break;
	}
	for(int column=0;column<A.length();column++) {
	    if (alphabet::is_letter_class( A(column,i) )) {
		internal_nodes = false; 
		break;
	    }
	}
    }

    if (not internal_nodes)
	return A;

    vector<int> D;
    for(int i=N;i<A.n_sequences();i++)
	D.push_back(i);

    A.del_sequences(D);

    if (not keep_empty_columns)
	remove_empty_columns(A);

    return A;
}

alignment add_internal(alignment A,const SequenceTree& T) 
{
    // Complain if A and T don't correspond
    if (A.n_sequences() != T.n_leaves())
	throw myexception()<<"Number of sequence in alignment doesn't match number of leaves in tree"
			   <<"- can't add internal sequences";

    // Add empty sequences
    vector<sequence> S;
    for(int i=T.n_leaves();i<T.n_nodes();i++) 
    {
	sequence s;

	if (T.get_label(i) == "")
	    throw myexception()<<"Adding internal sequences: Tree has missing internal node name!";

	s.name = T.get_label(i);

	S.push_back(s);
    }

    A.add_sequences(S);

    return A;
}


/// Construct a mapping of letters to columns for each leaf sequence
vector< vector<int> > column_lookup(const matrix<int>& M)
{
    int nleaves = M.size2();

    vector< vector<int> > result(nleaves);

    for(int i=0;i<nleaves;i++) {
	vector<int>& columns = result[i];
	columns.reserve(M.size2());
	for(int column=0;column<M.size1();column++)
        {
	    if (M(column, i) >= 0)
            {
                assert(M(column,i) == columns.size());
		columns.push_back(column);
            }
	}
    }

    return result;
}

/// Construct a mapping of letters to columns for each leaf sequence
vector< vector<int> > column_lookup(const alignment& A,int nleaves) 
{
    if (nleaves == -1)
	nleaves = A.n_sequences();

    vector< vector<int> > result(nleaves);

    for(int i=0;i<nleaves;i++) {
	vector<int>& columns = result[i];
	columns.reserve(A.length());
	for(int column=0;column<A.length();column++) {
	    if (A.character(column,i))
		columns.push_back(column);
	}
    }

    return result;
}

/// Replace each letter with its position in its sequence
matrix<int> M(const alignment& A1) 
{
    matrix<int> A2(A1.length(),A1.n_sequences());
    for(int i=0;i<A2.size2();i++) {
	int pos=0;
	for(int column=0;column<A2.size1();column++) {
	    if (A1.character(column,i)) {
		A2(column,i) = pos;
		pos++;
	    }
	    else
		A2(column,i) = A1(column,i);
	}

	assert(pos == A1.seqlength(i));
    }
    return A2;
}

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const matrix<int>& M1, int column, int s1, int s2, 
	     const matrix<int>& M2,
	     const vector< vector< int> >& column_indices) 
{
    if (M1(column,s1) == alphabet::gap and M1(column,s2)==alphabet::gap)
	return true;

    // Turn this into a statement about what s1[column] matches
    if (M1(column,s1)==alphabet::gap)
	std::swap(s1,s2);

    // which column in A2 has the A1(column,s1)-th feature of s1 ?
    int column2 = column_indices[s1][ M1(column,s1) ];
    return (M2(column2,s2) == M1(column,s2));
}


bool A_constant(alignment A1, alignment A2, const dynamic_bitset<>& ignore) {
    assert(A1.n_sequences() == A2.n_sequences());

    // equality holds if we have internal node sequences -- otherwise ignore is larger
    assert(A1.n_sequences() <= ignore.size());

    // convert to feature-number notation
    matrix<int> M1 = M(A1);
    matrix<int> M2 = M(A2);

    // lookup and cache the column each feature is in
    vector< vector< int> > column_indices = column_lookup(A2);

    //----- Check that the sequence lengths match ------//
    for(int i=0;i<M1.size2();i++) {
	if (ignore[i]) continue;

	if (A1.seqlength(i) != A2.seqlength(i))
	    return false;
    }

    //----- Check that each homology in A1 is in A2 -----//
    for(int column=0; column<A1.length(); column++)
	for(int s1=0; s1 < A1.n_sequences(); s1++) {
	    if (ignore[s1]) continue;
	    for(int s2=s1+1; s2 < A1.n_sequences(); s2++) {
		if (ignore[s2]) continue;
		if (not A_match(M1,column,s1,s2,M2,column_indices))
		    return false;
	    }
	}

    return true;
}

void check_names_unique(const alignment& A)
{
    // check that names are all unique
    for(int i=0;i<A.n_sequences();i++) {
	for(int j=0;j<i;j++)
	    if (A.seq(i).name == A.seq(j).name)
		throw myexception()<<"Sequence name '"<<A.seq(i).name<<"' occurs multiple times in the alignment!";
    }
}

bool names_are_unique(const alignment& A)
{
    // check that names are all unique
    for(int i=0;i<A.n_sequences();i++)
	for(int j=0;j<i;j++)
	    if (A.seq(i).name == A.seq(j).name)
		return false;
    return true;
}

vector<dynamic_bitset<>> get_states_for_leaf_characters(const alignment& A, const Tree& t)
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

vector<dynamic_bitset<>> get_states_for_all_characters(const alignment& A, const Tree& t)
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
vector<dynamic_bitset<>> get_connected_states(const vector<dynamic_bitset<>>& states, const Tree& t)
{
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

/// Check that internal nodes don't have letters (or anything wierder!)
void check_internal_sequences_composition(const alignment& A,int n_leaves) {

    for(int column=0;column<A.length();column++)
	for(int i=n_leaves;i<A.n_sequences();i++) 
	    if (A(column,i) == alphabet::gap)
		;
	    else if (A(column,i) == alphabet::not_gap)
		;
	    else if (A(column,i) >= 0)
	    {
		const alphabet& a = A.get_alphabet();
		if (a.is_letter(A(column,i)))
		    throw myexception()<<"Only '-' and 'N'/'X' are allowed in internal node sequences, but found illegal letter '"<<a.lookup(A(column,i))<<"' in column "<<column<<" of internal sequence '"<<A.seq(i).name<<"'.";
		else
		    throw myexception()<<"Only '-' and 'N'/'X' are allowed in internal node sequences, but found illegal index '"<<A(column,i)<<"' in column "<<column<<" of internal sequence '"<<A.seq(i).name<<"'.";
	    }
}

/// \brief Check if internal node characters are only present between leaf charaters.
///
/// \param A The alignment
/// \param T The tree
bool check_leaf_characters_minimally_connected(const alignment& A,const Tree& t)
{
    assert(A.n_sequences() == t.n_nodes());

    auto between_characters = get_connected_states(get_states_for_leaf_characters(A, t), t);

    auto bad_column = check_characters_present(A, between_characters);
    
    return not bad_column;
}

/// Force internal node states are consistent by connecting leaf characters
void minimally_connect_leaf_characters(alignment& A,const Tree& t)
{
    assert(A.n_sequences() == t.n_nodes());

    auto between_characters = get_connected_states(get_states_for_leaf_characters(A, t), t);

    for(int n = 0; n < t.n_nodes(); n++)
    {
	auto& present = between_characters[n];

	if (t.is_leaf_node(n)) continue;

	for(int column=0;column<A.length();column++)
	{
	    // put present characters into the alignment.
	    if (present[column])
		A.set_value(column, n, alphabet::not_gap );
	    else
		A.set_value(column, n, alphabet::gap );
	}
    }
    remove_empty_columns(A);
}    


void connect_leaf_characters(alignment& A,const Tree& t)
{
    assert(A.n_sequences() == t.n_nodes());

    auto between_characters = get_connected_states(get_states_for_leaf_characters(A, t), t);

    for(int n = 0; n < t.n_nodes(); n++)
    {
	auto& present = between_characters[n];

	if (t.is_leaf_node(n)) continue;

	for(int column=0;column<A.length();column++)
	{
	    // put present characters into the alignment.
	    if (present[column] and A.gap(column,n))
		A.set_value(column, n, alphabet::not_gap );
	}
    }
}

std::optional<int> check_characters_present(const alignment& A, const vector<dynamic_bitset<>>& present)
{
    for(int n=0; n < present.size(); n++)
	for(int c = 0; c < A.length(); c++)
	    if (present[n].test(c) and A.gap(c,n))
		return c;
    return {};
}

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const Tree& t)
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

void letters_OK(const alignment& A) {
    check_letters_OK(A);
}

void check_letters_OK(const alignment& A) {
    const alphabet& a = A.get_alphabet();

    bool bad=false;
    for(int i=0;i<A.length();i++)
	for(int j=0;j<A.n_sequences();j++)
	    if (A(i,j) >=0 and A(i,j) < a.size())
		; // this is a letter
	    else if (A(i,j) >= a.n_letters() and A(i,j) < a.n_letter_classes())
		; // this is a letter class
	    else if (A(i,j) == alphabet::gap)
		; // this is a '-'
	    else if (A(i,j) == alphabet::not_gap)
		; // this is a '*'
	    else if (A(i,j) == alphabet::unknown)
		; // this is a '?'
	    else {
		bad = true;
		cerr<<"A("<<i<<","<<j<<") = "<<A(i,j)<<endl;
	    }
    if (bad)
	std::abort();
}

void check_leaf_sequences(const alignment& A,int n_leaves) {

    vector<sequence> sequences = A.convert_to_sequences();

    const alphabet& a = A.get_alphabet();

    for(int i=0;i<n_leaves;i++) {

        // FIXME - how to distinquish between NO sequence (for site-compression) and an empty sequence?
        if (A.seq(i).size() == 0) continue;

        sequences[i].strip_gaps();
	if (not (a(sequences[i]) == a(A.seq(i)))) {
	    cerr<<"leaf sequence "<<i<<" corrupted!\n";

	    cerr<<"orig: "<<A.seq(i)<<endl;

	    cerr<<"new : "<<sequences[i]<<endl;

	    std::abort();
	}
    }
}

void check_alignment(const alignment& A,const Tree& T,bool internal_sequences) 
{
    // First check that there are no illegal letters
    check_letters_OK(A);

    // Next check that the internal sequences haven't changed
    check_leaf_sequences(A,T.n_leaves());

    if (not internal_sequences) return;

    // Next check that only N/X and - are found at internal nodes
    // check_internal_sequences_composition(A,T.n_leaves());
  
    // Finally check that the internal node states are consistent
    check_internal_nodes_connected(A,T);
}

vector<const_branchview> branches_toward_from_node(const Tree& T,int n) {
    vector<const_branchview> branches;
    branches.reserve(2*T.n_branches());

    branches = branches_from_node(T,n);
    for(int i=0;i<T.n_branches();i++)
	branches.push_back(branches[i]);

    for(int i=0;i<T.n_branches();i++)
	branches[i] = branches[branches.size()-1-i].reverse();

    return branches; 
}


matrix<int> get_SM(const alignment& A,const Tree& T) {
    matrix<int> SM(A.length(),2*T.n_branches());
    
    vector<const_branchview> branches = branches_toward_from_node(T,T.n_leaves());

    // Compute the sub-alignments
    vector<const_branchview> temp;temp.reserve(2);
    for(int i=0;i<branches.size();i++) {
	int b = branches[i];


	int l=0;
	for(int c=0;c<SM.size1();c++) {
	    SM(c,b) = alphabet::gap;

	    // for leaf branches fill from the alignment
	    if (branches[i].source().is_leaf_node()) {
		if (not A.gap(c,b))
		    SM(c,b) = l++;
	    }

	    // for internal branches fill from the previous branches
	    else {
		temp.clear();
		append(T.directed_branch(b).branches_before(),temp);
		assert(temp.size() == 2);

		if (SM(c,temp[0]) != -1 or SM(c,temp[1]) != -1)
		    SM(c,b) = l++;
	    }

	}
    }

    return SM;
}

double asymmetric_pairs_distance(const alignment& A1,const alignment& A2) {

    matrix<int> M1 = M(A1);
    matrix<int> M2 = M(A2);

    // lookup and cache the column each feature is in
    vector< vector< int> > column_indices2 = column_lookup(A2);

    return asymmetric_pairs_distance(M1,M2,column_indices2);
}


double asymmetric_pairs_distance(const matrix<int>& M1,const matrix<int>& M2,
				   const vector< vector<int> >& column_indices2)
{
    int mismatch=0;

    for(int column=0;column<M1.size1();column++) 
	for(int i=0;i<M1.size2();i++)
	    for(int j=0;j<i;j++)
	    {
		if (M1(column,i) == alphabet::unknown or M1(column,j) == alphabet::unknown)
		    continue;

		if (M1(column,i) != alphabet::gap or M1(column,j)!= alphabet::gap) {
		    if (not A_match(M1,column,i,j,M2,column_indices2)) 
		    {
			if (M1(column,i) != alphabet::gap)
			    mismatch++;
			if (M1(column,j) != alphabet::gap)
			    mismatch++;
		    }
		}
	    }

    return mismatch;
}

double homologies_total(const matrix<int>& M1) 
{
    long int total=0;

    for(int column=0;column<M1.size1();column++) 
	for(int i=0;i<M1.size2();i++)
	    if (M1(column,i) != alphabet::gap and M1(column,i) != alphabet::unknown)
		total++;

    return total;
}

double homologies_preserved(const matrix<int>& M1,const matrix<int>& M2,
			      const vector< vector<int> >& column_indices2)
{
    long int match=0;
    long int mismatch=0;

    for(int column=0;column<M1.size1();column++) 
	for(int i=0;i<M1.size2();i++)
	    if (M1(column,i) != alphabet::gap and M1(column,i) != alphabet::unknown)
		for(int j=0;j<M1.size2();j++)
		    if (j != i) {
			if (A_match(M1,column,i,j,M2,column_indices2))
			    match++;
			else
			    mismatch++;
		    }
	
    assert(homologies_total(M1) == homologies_total(M2));
    assert(homologies_total(M1) == match + mismatch);

    return match;
}

double homologies_distance(const matrix<int>& M1,const matrix<int>& M2,
			   const vector< vector<int> >& column_indices2)
{
    unsigned total = homologies_total(M1);
    unsigned match = homologies_preserved(M1,M2,column_indices2);
    return double(total-match)/total;
}
vector<int> get_splitgroup_columns(const matrix<int>& M1,
				   int column,
				   const matrix<int>& /* M2 */,
				   const vector< vector<int> >& columns) 
{
    vector<int> label(M1.size2());
    for(int i=0;i<label.size();i++) {
	if (M1(column,i) == alphabet::gap or M1(column,i) == alphabet::unknown)
	    label[i] = M1(column,i);
	else
	    label[i] = columns[i][M1(column,i)];
    }

    /*
    // If letter from the original column is in a column with a gap here
    // then put this gap in the same column as the letter
    for(int i=0;i<label.size();i++) 
    {
    if (label[i] != alphabet::gap) continue;

    for(int j=0;j<label.size();j++) 
    if (label[j] != alphabet::gap and label[j] != alphabet::unknown and M2(label[j],i) == alphabet::gap) {
    label[i] = label[j];
    break;
    }
    }
    */

    return label;
}

double asymmetric_splits_distance(const alignment& A1,const alignment& A2) 
{

    matrix<int> M1 = M(A1);
    matrix<int> M2 = M(A2);

    // lookup and cache the column each feature is in
    vector< vector< int> > column_indices2 = column_lookup(A2);

    return asymmetric_splits_distance(M1,M2,column_indices2);
}

double asymmetric_splits_distance2(const alignment& A1,const alignment& A2) 
{

    matrix<int> M1 = M(A1);
    matrix<int> M2 = M(A2);

    // lookup and cache the column each feature is in
    vector< vector< int> > column_indices2 = column_lookup(A2);

    return asymmetric_splits_distance2(M1,M2,column_indices2);
}

double asymmetric_splits_distance(const matrix<int>& M1,const matrix<int>& M2,
				    const vector< vector<int> >& column_indices2)
{
    int distance=0;

    for(int column=0;column<M1.size1();column++) 
    {
	vector<int> columns = get_splitgroup_columns(M1,column,M2,column_indices2);

	vector<int> uniq;uniq.reserve(columns.size());
	for(int i=0;i<columns.size();i++)
	    if (columns[i] != alphabet::unknown and 
		columns[i] != alphabet::gap and
		not includes(uniq,columns[i]))
		uniq.push_back(columns[i]);

	int splits = uniq.size();
	int delta = (splits-1);
	assert(delta >= 0);
	distance += delta;
    }

    return distance;
}

double asymmetric_splits_distance2(const matrix<int>& M1,const matrix<int>& M2,
				     const vector< vector<int> >& column_indices2)
{
    int distance=0;

    for(int column=0;column<M1.size1();column++) 
    {
	vector<int> columns = get_splitgroup_columns(M1,column,M2,column_indices2);

	vector<int> uniq;uniq.reserve(columns.size());
	for(int i=0;i<columns.size();i++)
	    if (columns[i] != alphabet::unknown and 
		columns[i] != alphabet::gap and
		not includes(uniq,columns[i]))
		uniq.push_back(columns[i]);

	int splits = uniq.size();
	int delta = splits*(splits-1)/2;
	assert(delta >= 0);
	distance += delta;
    }

    return distance;
}

double pairs_distance(const alignment& A1,const alignment& A2) 
{
    return asymmetric_pairs_distance(A1,A2) + asymmetric_pairs_distance(A2,A1);
}

double pairs_distance(const matrix<int>& M1,const vector< vector<int> >& column_indices1,
			const matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
    return asymmetric_pairs_distance(M1,M2,column_indices2)
	+ asymmetric_pairs_distance(M2,M1,column_indices1);
}


double splits_distance(const alignment& A1,const alignment& A2) 
{
    return asymmetric_splits_distance(A1,A2)+asymmetric_splits_distance(A2,A1);
}

double splits_distance2(const alignment& A1,const alignment& A2) 
{
    return asymmetric_splits_distance2(A1,A2)+asymmetric_splits_distance2(A2,A1);
}

double splits_distance(const matrix<int>& M1,const vector< vector<int> >& column_indices1,
			 const matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
    return asymmetric_splits_distance(M1,M2,column_indices2)
	+ asymmetric_splits_distance(M2,M1,column_indices1);
}

double splits_distance2(const matrix<int>& M1,const vector< vector<int> >& column_indices1,
			  const matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
    return asymmetric_splits_distance2(M1,M2,column_indices2)
	+ asymmetric_splits_distance2(M2,M1,column_indices1);
}


void check_disconnected(const alignment& A,const dynamic_bitset<>& mask)
{
    dynamic_bitset<> g1 = mask;
    dynamic_bitset<> g2 = ~mask;

    for(int i=0;i<A.length();i++) {
	if (not (all_gaps(A,i,g1) or all_gaps(A,i,g2))) {
	    cerr<<"bad homology in column i!"<<endl<<endl;
	    cerr<<A<<endl;
	    std::abort();
	}
    }
}

void check_disconnected(const alignment& A, const Tree& T, const std::vector<int>& disconnected)
{
    assert(disconnected.size() == T.n_branches());

    for(int b=0;b<disconnected.size();b++)
	if (disconnected[b]) {
	    dynamic_bitset<> mask = T.partition(b);
	    check_disconnected(A,mask);
	}
  
}

double fraction_identical(const alignment& A,int s1,int s2,bool gaps_count) 
{
    unsigned total=0;
    unsigned same =0;
    for(int i=0;i<A.length();i++) {
	if (A.gap(i,s1) and A.gap(i,s2)) 
	    continue;

	if (not gaps_count and (A.gap(i,s1) or A.gap(i,s2)))
	    continue;

	total++;

	if (A(i,s1) == A(i,s2))
	    same++;
    }

    double f = 1;
    if (total > 0)
	f = double(same)/total;

    return f;
}


double fraction_homologous(const alignment& A,int s1,int s2) 
{
    unsigned total=0;
    unsigned same =0;
    for(int i=0;i<A.length();i++) 
    {
	if (not A.character(i,s1) and not A.character(i,s2)) 
	    continue;

	total++;

	if (A.character(i,s1) and A.character(i,s2))
	    same++;
    }

    double f = 1;
    if (total > 0)
	f = double(same)/total;

    return f;
}



unsigned n_homologous(const alignment& A,int s1,int s2) 
{
    unsigned same =0;
    for(int i=0;i<A.length();i++) 
    {
	if (A.character(i,s1) and A.character(i,s2))
	    same++;
    }

    return same;;
}

void count_gaps(const alignment& A, int c, valarray<int>& counts)
{
    const alphabet& a = A.get_alphabet();
    assert(counts.size() == 2);

    counts = 0;
    for(int i=0;i<A.n_sequences();i++) 
    {
	int l = A(c,i);
	if (a.is_feature(l))
	    counts[0]++;
	else if (l == alphabet::gap)
	    counts[1]++;
    }
}


void count_letters(const alignment& A, int c, valarray<int>& counts)
{
    const alphabet& a = A.get_alphabet();
    assert(counts.size() == a.size());

    counts = 0;
    for(int i=0;i<A.n_sequences();i++) 
    {
	int l = A(c,i);
	if (a.is_letter(l))
	    counts[l]++;
    }
}

int n_letters_with_count_at_least(const valarray<int>& count,int level) 
{
    int n = 0;
    for(int l=0;l<count.size();l++)
	if (count[l] >= level) n++;
    return n;
}

bool informative_counts(const valarray<int>& counts)
{
    return (n_letters_with_count_at_least(counts,2) >= 2);
}

bool variable_counts(const valarray<int>& counts)
{
    return (n_letters_with_count_at_least(counts,1) >= 2);
}

// This function ignores information in ambiguous letters
dynamic_bitset<> letter_informative_sites(const alignment& A)
{
    const alphabet& a = A.get_alphabet();

    valarray<int> counts(0, a.size());

    dynamic_bitset<> columns(A.length());
    for(int c=0; c<A.length(); c++)
    {
	count_letters(A,c,counts);
	if (informative_counts(counts))
	    columns[c] = true;
    }
    return columns;
}

// This function ignores information in ambiguous letters
unsigned n_letter_informative_sites(const alignment& A)
{
    const alphabet& a = A.get_alphabet();

    valarray<int> counts(0, a.size());

    unsigned n=0;
    for(int c=0; c<A.length(); c++)
    {
	count_letters(A,c,counts);
	if (informative_counts(counts))
	    n++;
    }
    return n;
}

// This function ignores information in ambiguous letters
dynamic_bitset<> letter_variable_sites(const alignment& A)
{
    const alphabet& a = A.get_alphabet();

    valarray<int> counts(0, a.size());

    dynamic_bitset<> columns(A.length());
    for(int c=0; c<A.length(); c++)
    {
	count_letters(A,c,counts);
	if (variable_counts(counts))
	    columns[c] = true;
    }
    return columns;
}

// This function ignores information in ambiguous letters
unsigned n_letter_variable_sites(const alignment& A)
{
    const alphabet& a = A.get_alphabet();

    valarray<int> counts(0, a.size());

    unsigned n=0;
    for(int c=0; c<A.length(); c++)
    {
	count_letters(A,c,counts);
	if (variable_counts(counts))
	    n++;
    }
    return n;
}

dynamic_bitset<> gap_informative_sites(const alignment& A)
{
    valarray<int> counts(0, 2);

    dynamic_bitset<> columns(A.length());
    for(int c=0; c<A.length(); c++)
    {
	count_gaps(A,c,counts);
	if (informative_counts(counts))
	    columns[c] = true;
    }
    return columns;
}

unsigned n_gap_informative_sites(const alignment& A)
{
    valarray<int> counts(0, 2);

    unsigned n=0;
    for(int c=0; c<A.length(); c++)
    {
	count_gaps(A,c,counts);
	if (informative_counts(counts))
	    n++;
    }
    return n;
}


dynamic_bitset<> gap_variable_sites(const alignment& A)
{
    valarray<int> counts(0, 2);

    dynamic_bitset<> columns(A.length());
    for(int c=0; c<A.length(); c++)
    {
	count_gaps(A,c,counts);
	if (variable_counts(counts))
	    columns[c] = true;
    }
    return columns;
}

unsigned n_gap_variable_sites(const alignment& A)
{
    valarray<int> counts(0, 2);

    unsigned n=0;
    for(int c=0; c<A.length(); c++)
    {
	count_gaps(A,c,counts);
	if (variable_counts(counts))
	    n++;
    }
    return n;
}


vector<unsigned> sequence_lengths(const alignment& A,unsigned n)
{
    vector<unsigned> lengths(n);
    for(int i=0;i<n;i++)
	lengths[i] = A.seqlength(i);
    return lengths;
}

vector<unsigned> sequence_lengths(const alignment& A)
{
    return sequence_lengths(A,A.n_sequences());
}


alignment select_rows(const alignment& A,const vector<int>& keep)
{
    bool changed=false;

    vector<int> order;
    order.reserve(keep.size());
    for(int i=0;i<keep.size();i++)
	if (keep[i])
	    order.push_back(i);
	else
	    changed=true;

    if (changed)
	return reorder_sequences(A,order);
    else
	return A;
}

void select_columns_inplace(alignment& A, const vector<int>& columns)
{
    int j=0;
    for(int column: columns)
    {
	// Copy column column -> column j
	if (column != j)
	    for(int k=0;k<A.n_sequences();k++)
		A.set_value(j,k, A(column,k) );
	j++;
    }
    A.changelength(j);
}

alignment select_columns(const alignment& A,const vector<int>& sites) 
{
    alignment A2 = A;
    A2.changelength(sites.size());
    for(int i=0;i<sites.size();i++)
    {
        int column = sites[i];
	for(int j=0;j<A2.n_sequences();j++)
	    A2.set_value(i,j, A(column,j) );
    }
    return A2;
}

alignment select_columns(const alignment& A,const dynamic_bitset<>& keep) 
{
    assert(keep.size() == A.length());

    vector<int> columns;
    for(int i = 0; i < keep.size(); i++)
        if (keep[i])
            columns.push_back(i);

    return select_columns(A, columns);
}

alignment reverse(const alignment& A)
{
    int L = A.length();

    alignment A2 = A;

    // Reverse
    for(int i=0;i<A2.n_sequences();i++) 
	for(int j=0;j<A2.length();j++)
	    A2.set_value(j,i, A(L-j-1,i) );

    return A2;
}

alignment complement(const alignment& A)
{
    const alphabet& a = A.get_alphabet();

    owned_ptr<Nucleotides> N(dynamic_cast<const Nucleotides&>(a));

    if (not N)
	throw myexception()<<"Sequences have alphabet "<<a.name<<" -- reverse complement not allowed";

    alignment A2 = A;

    // Reverse
    for(int i=0;i<A2.n_sequences();i++) 
	for(int j=0;j<A2.length();j++)
	    A2.set_value(j,i, N->complement(A(j,i)) );

    return A2;
}

alignment reverse_complement(const alignment& A)
{
    const alphabet& a = A.get_alphabet();

    owned_ptr<Nucleotides> N(dynamic_cast<const Nucleotides&>(a));

    if (not N)
	throw myexception()<<"Sequences have alphabet "<<a.name<<" -- reverse complement not allowed";

    int L = A.length();

    alignment A2 = A;

    // Reverse
    for(int i=0;i<A2.n_sequences();i++) 
	for(int j=0;j<A2.length();j++)
	    A2.set_value(j,i, N->complement(A(L-j-1,i)) );

    return A2;
}

// FIXME - should perhaps also check names?
// use this function in alignment-gild, alignment-compare, alignment-diff, etc.
void check_same_sequence_lengths(const vector<int>& L, const alignment& A)
{
    if (A.n_sequences() != L.size())
	throw myexception()<<"Expected alignment has "<<L.size()<<", but this one has "<<A.n_sequences();

    for(int i=0;i<L.size();i++)
    {
	int L2 = A.seqlength(i);
	if (L[i] != L2)
	    throw myexception()<<"Sequence "<<i+1<<": length "<<L2<<" differs from expected length "<<L[i];
    }
}

vector<int> alignment_row_letters(const alignment& A, int i)
{
    vector<int> s;
    for(int c=0;c<A.length();c++)
	if (A.character(c,i))
	    s.push_back(A(c,i));
    return s;
}

vector<int> alignment_row_counts(const alignment& A, int i, const vector<int>& counts)
{
    vector<int> s;
    for(int c=0;c<A.length();c++)
	if (A.character(c,i))
	    s.push_back(counts[c]);
    return s;
}

vector<vector<int> > alignment_letters(const alignment& A, int N)
{
    // Construct the new leaf sequences
    vector< vector<int> > S;

    for(int i=0;i<N;i++)
	S.push_back( alignment_row_letters(A,i));

    return S;
}

vector<vector<int> > alignment_letters_counts(const alignment& A, int N, const vector<int>& counts)
{
    // Construct the counts for the new leaf sequences
    vector< vector<int> > S;

    for(int i=0;i<N;i++)
	S.push_back( alignment_row_counts(A,i,counts));

    return S;
}

alignment unalign_all(const alignment& A, int n)
{
    if (n == -1)
	n = A.n_sequences();

    // Choose the length of the new alignment
    int new_length = sum(sequence_lengths(A,n));
    alignment A2 = blank_copy(A,new_length);

    // Clear the new alignment
    for(int i=0;i<A2.length();i++)
	for(int j=0;j<A2.n_sequences();j++)
	    A2.set_value(i,j, alphabet::gap );

    // For each row of the new alignment
    int start_column=0;
    for(int i=0;i<n;i++) 
    {
	/// Collect the letters of the row
	vector<int> s = alignment_row_letters(A,i);

	// write them into the correct position, move start_column
	for(int j=0;j<s.size();j++)
	    A2.set_value(start_column++,i, s[j] );
    }
 
    return A2;
}

bool is_masked_column(const alignment& A, int c)
{
    for(int i=0;i<A.n_sequences();i++)
	if (A(c,i) != alphabet::not_gap)
	    return false;
    return true;
}

bool is_variant_column(const alignment& A, int c)
{
    assert(0 <= c and c < A.length());
    int i=0;
    int l0 = -1;
    for(;i<A.n_sequences() and l0 < 0;i++)
	l0 = A(c,i);

    for(;i<A.n_sequences();i++)
	if (A(c,i) >= 0 and A(c,i) != l0) return true;

    return false;
}

bool is_column_with_gap(const alignment& A, int c)
{
    for(int i=0;i<A.n_sequences();i++)
	if (A(c,i) == alphabet::gap)
	    return true;
    return false;
}

bool is_column_with_gap_fraction(const alignment& A, int c, double fraction)
{
    int n_gaps = 0;
    int n_letters = 0;
    for(int i=0;i<A.n_sequences();i++)
    {
	if (A.gap(c,i))
            n_gaps++;
        else if(A(c,i) >= 0)
            n_letters++;
    }

    return (double(n_gaps)/(n_gaps+n_letters) >= fraction);
}

int count_variant_columns(const alignment& A, int c1, int c2)
{
    int count = 0;
    for(int c=c1;c<=c2 and c<A.length();c++)
	if (is_variant_column(A,c))
	    count++;
    return count;
}

void remove_columns(alignment& A, const std::function<bool(int)>& remove)
{
    int j=0;
    for(int i=0;i<A.length();i++)
	if (not remove(i))
	{
	    // Copy column i -> column j
	    if (i != j)
		for(int k=0;k<A.n_sequences();k++)
		    A.set_value(j,k, A(i,k) );
	    j++;
	}
    A.changelength(j);
}


void mask_column(alignment& A, int column)
{
    for(int k=0;k<A.n_sequences();k++)
    {
	int value = A(column,k);
	if (alphabet::is_feature(value))
	    A.set_value(column,k, alphabet::not_gap);
    }
}

void remove_and_mask_columns(alignment& A, const std::function<int(int)>& remove_or_mask)
{
    int j=0;
    for(int i=0;i<A.length();i++)
    {
	int fate = remove_or_mask(i);

	// Remove this column
	if (fate == 2)
	    continue;

	// Mask the features in this column
	else if (fate == 1)
	    mask_column(A,i);

	// Copy column i -> j
	else if (i != 0)
	    for(int k=0;k<A.n_sequences();k++)
		A.set_value(j,k, A(i,k) );

	j++;
    }

    A.changelength(j);
}

dynamic_bitset<> find_columns(const alignment& A, const std::function<bool(const alignment&,int)>& pred)
{
    dynamic_bitset<> p(A.length());
    for(int i=0;i<A.length();i++)
	p[i] = pred(A,i);
    return p;
}

vector<int> find_columns(const alignment& A, const std::function<bool(const alignment&,int)>& pred, int label)
{
    vector<int> p(A.length(), 0);
    for(int i=0;i<A.length();i++)
	if (pred(A,i))
	    p[i] = label;
    return p;
}

vector<int> find_columns(const alignment& A, const std::function<bool(int)>& keep)
{
    vector<int> columns;
    for(int column=0; column<A.length(); column++)
	if (keep(column))
	    columns.push_back(column);
    return columns;
}

dynamic_bitset<> gap_columns(const alignment& A)
{
    return find_columns(A, is_column_with_gap);
}

vector<int> gap_columns(const alignment& A, int label, double fraction)
{
    auto is_gap_column = [&](const alignment& a, int c) {return is_column_with_gap_fraction(a, c, fraction);};
    return find_columns(A, is_gap_column, label);
}

dynamic_bitset<> variant_columns(const alignment& A)
{
    return find_columns(A, is_variant_column);
}

dynamic_bitset<> variant_column_at_distance(const alignment& A,int d)
{
    auto variants = variant_columns(A);
    auto variants2 = variants;
    for(int i=0;i<A.length();i++)
    {
	if (not variants2[i]) continue;
	if (i-d <0 or not variants[i-d])
	    variants2[i] = false;
    }
    return variants2;
}

