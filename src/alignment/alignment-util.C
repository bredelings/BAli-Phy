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

#include "alignment-util.H"
#include "substitution/substitution-index.H"
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

using boost::program_options::variables_map;

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

void connect_all_characters(const Tree& T,dynamic_bitset<>& present)
{
  assert(present.size() == T.n_nodes());
  
  //---------- for each internal node... -------------//
  for(int n1=T.n_leaves(); n1<T.n_nodes(); n1++) 
  {
    if (present[n1]) continue;

    //------- if it is '-' and not ignored ... -------//
    vector<const_nodeview> neighbors;
    append(T.node(n1).neighbors(),neighbors);
    assert(neighbors.size() == 3);

    //---- check the three attatched subtrees ... ----//
    int total=0;
    for(int i=0;i<neighbors.size();i++)
    {
      dynamic_bitset<> group = T.partition(n1,neighbors[i]);
      if (present.intersects(group))
	total++;
    }

    if (total > 1)
      present[n1] = true;
  }
  assert(all_characters_connected(T,present,vector<int>()));
}

/// Check that any two present nodes are connected by a path of present nodes
bool all_characters_connected(const Tree& T,dynamic_bitset<> present,const vector<int>& _ignore) {
  assert(present.size() == T.n_nodes());

  //--------- set the ignored nodes to 'not present' -----------//
  dynamic_bitset<> ignore(present.size());
  for(int i=0;i<_ignore.size();i++) {
    int n = _ignore[i];
    present[n] = false;
    ignore[n] = true;
  }

  //---------- for each internal node... -------------//
  for(int n1=T.n_leaves(); n1<T.n_nodes(); n1++) {

    if (present[n1] or ignore[n1]) continue;
      
    //------- if it is '-' and not ignored ... -------//
    vector<const_nodeview> neighbors;
    append(T.node(n1).neighbors(),neighbors);
    assert(neighbors.size() == 3);

    //---- check the three attatched subtrees ... ----//
    int total=0;
    for(int i=0;i<neighbors.size();i++) {
      dynamic_bitset<> group = T.partition(n1,neighbors[i]);
      if (present.intersects(group))
	total++;
    }

    //----- nodes should be present in only one. -----//
    if (total > 1)
      return false;
  }
  return true;
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
bool check_leaf_characters_minimally_connected(const alignment& A,const Tree& T)
{
  assert(A.n_sequences() == T.n_nodes());

  for(int column=0;column<A.length();column++)
  {
    // construct leaf presence/absence mask
    dynamic_bitset<> present(T.n_nodes());
    for(int i=0;i<T.n_nodes();i++)
      present[i] = not A.gap(column,i);
    
    // compute presence/absence for internal nodes
    connect_all_characters(T,present);

    // put present characters into the alignment.
    for(int i=T.n_leaves();i<T.n_nodes();i++)
      if (present[i] != A.character(column,i))
	return false;
  }
  return true;
}



/// Force internal node states are consistent by connecting leaf characters
void minimally_connect_leaf_characters(alignment& A,const Tree& T)
{
  assert(A.n_sequences() == T.n_nodes());

  for(int column=0;column<A.length();column++)
  {
    // construct leaf presence/absence mask
    dynamic_bitset<> present(T.n_nodes());
    for(int i=0;i<T.n_nodes();i++)
      present[i] = not A.gap(column,i);
    
    // compute presence/absence for internal nodes
    connect_all_characters(T,present);

    // put present characters into the alignment.
    for(int i=T.n_leaves();i<T.n_nodes();i++) {
      if (present[i])
	A.set_value(column,i, alphabet::not_gap );
      else
	A.set_value(column,i, alphabet::gap );
    }
  }
  remove_empty_columns(A);
}

/// Force internal node states are consistent by connecting leaf characters
void connect_leaf_characters(alignment& A,const Tree& T)
{
  assert(A.n_sequences() == T.n_nodes());

  for(int column=0;column<A.length();column++)
  {
    // construct leaf presence/absence mask
    dynamic_bitset<> present(T.n_nodes());
    for(int i=0;i<T.n_nodes();i++)
      present[i] = not A.gap(column,i);
    
    // compute presence/absence for internal nodes
    connect_all_characters(T,present);

    // put present characters into the alignment.
    for(int i=T.n_leaves();i<T.n_nodes();i++) {
      if (present[i])
	A.set_value(column,i, alphabet::not_gap);
    }
  }
}

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const Tree& T,const vector<int>& ignore) 
{
  // Only check if A in fact has internal node sequences.
  if (A.n_sequences() == T.n_leaves()) return;

  assert(A.n_sequences() == T.n_nodes());

  for(int column=0;column<A.length();column++) {
    dynamic_bitset<> present(T.n_nodes());
    for(int i=0;i<T.n_nodes();i++) 
      present[i] = not A.gap(column,i);
    
    if (not all_characters_connected(T,present,ignore)) {
      cerr<<"Internal node states are inconsistent in column "<<column<<endl;
      cerr<<A<<endl;
      throw myexception()<<"Internal node states are inconsistent in column "<<column;
    }
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
  check_internal_sequences_composition(A,T.n_leaves());
  
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

long int asymmetric_pairs_distance(const alignment& A1,const alignment& A2) {

  matrix<int> M1 = M(A1);
  matrix<int> M2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices2 = column_lookup(A2);

  return asymmetric_pairs_distance(M1,M2,column_indices2);
}


long int asymmetric_pairs_distance(const matrix<int>& M1,const matrix<int>& M2,
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

long int homologies_total(const matrix<int>& M1) 
{
  long int total=0;

  for(int column=0;column<M1.size1();column++) 
    for(int i=0;i<M1.size2();i++)
      if (M1(column,i) != alphabet::gap and M1(column,i) != alphabet::unknown)
	total++;

  return total;
}

long int homologies_preserved(const matrix<int>& M1,const matrix<int>& M2,
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

long int asymmetric_splits_distance(const alignment& A1,const alignment& A2) 
{

  matrix<int> M1 = M(A1);
  matrix<int> M2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices2 = column_lookup(A2);

  return asymmetric_splits_distance(M1,M2,column_indices2);
}

long int asymmetric_splits_distance2(const alignment& A1,const alignment& A2) 
{

  matrix<int> M1 = M(A1);
  matrix<int> M2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices2 = column_lookup(A2);

  return asymmetric_splits_distance2(M1,M2,column_indices2);
}

long int asymmetric_splits_distance(const matrix<int>& M1,const matrix<int>& M2,
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

long int asymmetric_splits_distance2(const matrix<int>& M1,const matrix<int>& M2,
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

long int pairs_distance(const alignment& A1,const alignment& A2) 
{
  return asymmetric_pairs_distance(A1,A2) + asymmetric_pairs_distance(A2,A1);
}

long int pairs_distance(const matrix<int>& M1,const vector< vector<int> >& column_indices1,
			const matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
  return asymmetric_pairs_distance(M1,M2,column_indices2)
    + asymmetric_pairs_distance(M2,M1,column_indices1);
}


long int splits_distance(const alignment& A1,const alignment& A2) 
{
  return asymmetric_splits_distance(A1,A2)+asymmetric_splits_distance(A2,A1);
}

long int splits_distance2(const alignment& A1,const alignment& A2) 
{
  return asymmetric_splits_distance2(A1,A2)+asymmetric_splits_distance2(A2,A1);
}

long int splits_distance(const matrix<int>& M1,const vector< vector<int> >& column_indices1,
			const matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
  return asymmetric_splits_distance(M1,M2,column_indices2)
    + asymmetric_splits_distance(M2,M1,column_indices1);
}

long int splits_distance2(const matrix<int>& M1,const vector< vector<int> >& column_indices1,
			const matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
  return asymmetric_splits_distance2(M1,M2,column_indices2)
    + asymmetric_splits_distance2(M2,M1,column_indices1);
}


vector<object_ptr<const alphabet> > load_alphabets(const variables_map& args) 
{
  vector<object_ptr<const alphabet> > alphabets; 

  if (not args.count("alphabet"))
    return load_alphabets();

  const string name = args["alphabet"].as<string>();

  return load_alphabets(name);
}

alignment load_alignment(const string& filename,const vector<object_ptr<const alphabet> >& alphabets)
{
  vector<sequence> sequences = sequence_format::load_from_file(filename);

  alignment A;

  A.load(alphabets, sequences);
  
  int n_empty = remove_empty_columns(A);
  if (n_empty)
    if (log_verbose) cerr<<"Warning: removed "<<n_empty<<" empty columns from alignment '"<<filename<<"'!\n"<<endl;
  
  if (A.n_sequences() == 0)
    throw myexception()<<"Alignment file "<<filename<<" didn't contain any sequences!";

  return A;
}

int total_length(const vector<sequence>& sequences)
{
  int count = 0;
  for(const auto& sequence: sequences)
    count += sequence.size();
  return count;
}

int letter_count(const string& letters, const string& s)
{
  int count = 0;
  for(int i=0;i<s.size();i++)
    if (letters.find(s[i]) != std::string::npos)
      count++;
  return count;
}

double letter_count(const string& letters, const vector<sequence>& sequences)
{
  int count = 0;
  for(const auto& sequence: sequences)
    count += letter_count(letters, sequence);
  return count;
}

double letter_fraction(const string& letters, const string& gaps, const vector<sequence>& sequences)
{
  int count = letter_count(letters, sequences);
  int total = total_length(sequences) - letter_count(gaps, sequences);

  return double(count)/total;
}

object_ptr<const alphabet> guess_alphabet(const vector<sequence>& sequences)
{
  double ATGCN = letter_fraction("ATGCN","-?",sequences);
  double AUGCN = letter_fraction("AUGCN","-?",sequences);
  if (ATGCN > 0.95 and AUGCN <= ATGCN)
    return new DNA;
  else if (AUGCN > 0.95)
    return new RNA;

  if (std::max(ATGCN,AUGCN) > 0.5)
    throw myexception()<<"Can't guess alphabet";

  if (letter_count("*",sequences) > 0)
    return new AminoAcidsWithStop;
  else
    return new AminoAcids;
}

alignment load_alignment(const string& filename)
{
  vector<sequence> sequences = sequence_format::load_from_file(filename);

  try {
    object_ptr<const alphabet> a = guess_alphabet(sequences);

    alignment A(*a);
    A.load(sequences);
    
    int n_empty = remove_empty_columns(A);
    if (n_empty)
      if (log_verbose) cerr<<"Warning: removed "<<n_empty<<" empty columns from alignment '"<<filename<<"'!\n"<<endl;
    
    if (A.n_sequences() == 0)
      throw myexception()<<"Alignment file "<<filename<<" didn't contain any sequences!";
    
    return A;
  }
  catch (myexception& e)
  {
    throw e<<" for file "<<filename<<".  Please specify RNA, DNA, or AminoAcids.";
  }
}

/// Load an alignment from command line args "--align filename"
alignment load_A(const variables_map& args,bool keep_internal) 
{
  vector<object_ptr<const alphabet> > alphabets = load_alphabets(args);
  
  // ----- Try to load alignment ------ //
  if (not args.count("align")) 
    throw myexception("Alignment file not specified! (--align <filename>)");
  
  string filename = args["align"].as<string>();
  alignment A = load_alignment(filename,alphabets);

  if (not keep_internal)
    A = chop_internal(A);

  return A;
}


using std::vector;
using std::string;
using std::list;

istream& find_alignment(istream& ifile)
{
  string line;
  while (ifile and ifile.peek() != '>')
  {
    if (not portable_getline(ifile,line)) break;
  }

  return ifile;
}

istream& skip_alignment(istream& ifile)
{
  string line;
  do {
    portable_getline(ifile,line);
  } while (ifile and line.size());

  return ifile;
}

istream& find_and_skip_alignment(istream& ifile)
{
  if (find_alignment(ifile))
    skip_alignment(ifile);
  return ifile;
}

istream& find_and_skip_alignments(istream& ifile, int n)
{
  for(int i=0;i<n and ifile;i++)
    find_and_skip_alignment(ifile);
  return ifile;
}

alignment load_next_alignment(istream& ifile, const vector<object_ptr<const alphabet> >& alphabets)
{
  string line;

  if (not find_alignment(ifile))
    throw myexception()<<"No alignment found.\n";

  // READ the alignment
  try {
    alignment A;
    A.load(alphabets, sequence_format::read_fasta, ifile);
    
    // strip out empty columns
    remove_empty_columns(A);
    
    // complain if there are no sequences in the alignment
    if (A.n_sequences() == 0) 
      throw myexception(string("Alignment didn't contain any sequences!"));
    
    return A;
  }
  catch (std::exception& e) {
    throw myexception()<<"Error loading alignment.\n  Exception: "<<e.what()<<"\n";
  }
}

alignment load_next_alignment(istream& ifile, object_ptr<const alphabet> a)
{
  vector<object_ptr<const alphabet> > alphabets;
  alphabets.push_back(a);
  return load_next_alignment(ifile,alphabets);
}

alignment load_next_alignment(istream& ifile, const alphabet& a)
{
  object_ptr<const alphabet> aa ( a.clone() );
  return load_next_alignment(ifile,aa);
}

alignment reorder_sequences(const alignment& A, const vector<string>& names)
{
  // Check the names and stuff.
  vector<string> n2 = sequence_names(A);

  if (names == n2) return A;

  alignment A2;
  try {
    vector<int> new_order = compute_mapping(names,n2);

    A2 = reorder_sequences(A,new_order);
  }
  catch(bad_mapping<string>& e)
  {
    e.clear();
    if (e.size2 < e.size1)
      e<<"Alignment has too few sequences! (Got "<<A.n_sequences()<<", expected "<<names.size()<<")\n";

    if (e.size1 < e.size2)
      e<<"Alignmnent has too many sequences! (Got "<<A.n_sequences()<<", expected "<<names.size()<<")\n";

    if (e.from == 0)
      e<<"Alignment is missing sequence \""<<e.missing<<"\".";
    else
      e<<"Alignment has extra sequence \""<<e.missing<<"\".";
    throw e;
  }

  return A2;
}

alignment load_next_alignment(istream& ifile, const alphabet& a, const vector<string>& names)
{
  object_ptr<const alphabet> aa ( a.clone() );
  alignment A = load_next_alignment(ifile,aa);
  return reorder_sequences(A,names);
}

int thin_alignments(list<alignment>& alignments)
{
  int remaining = 0;

  // Remove every other alignment
  typedef list<alignment>::iterator iterator_t;

  for(iterator_t loc = alignments.begin();loc!=alignments.end();) 
  {
    iterator_t j = loc++; 

    alignments.erase(j);

    if (loc == alignments.end())  break;

    loc++;
    remaining++;
  }

  return remaining;
}

bool thin_alignments(list<alignment>& alignments,int max)
{
  int total = alignments.size();
  if (total <= max or max == -1)  return false;

  assert(total <= max*2);

  // We have this many extra alignments
  const int extra = total - max;
  
  vector<int> kill(extra);
  for(int i=0;i<kill.size();i++)
    kill[i] = int( double(i+0.5)*total/extra);
  std::reverse(kill.begin(),kill.end());
  
  int i=0;
  typedef list<alignment>::iterator iterator_t;
  for(iterator_t loc = alignments.begin();loc!=alignments.end();i++) {
    if (i == kill.back()) {
      kill.pop_back();
      iterator_t j = loc++;
      alignments.erase(j);
      total--;
    }
    else
      loc++;
  }
  assert(kill.empty());
  return true;
}

istream& load_more_alignments(list<alignment>& alignments, istream& ifile, const vector<string>& names, 
			   const alphabet& a, int maxalignments, int subsample=1) 
{
  int total = alignments.size();

  while(find_alignment(ifile))
  {
    try {
      // READ the next alignment
      alignment A = load_next_alignment(ifile,a,names);

      // STORE the alignment
      alignments.push_back(A);
      total++;
    }
    catch (std::exception& e) {
      if (alignments.empty())
	throw e;

      cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

    // If there are too many alignments
    if (maxalignments != -1 and total > 2*maxalignments) {
      // start skipping twice as many alignments
      subsample *= 2;

      if (log_verbose) cerr<<"Went from "<<total;
      total = thin_alignments(alignments);
      if (log_verbose) cerr<<" to "<<total<<" alignments.\n";

    }

    // skip over alignments due to subsampling
    find_and_skip_alignments(ifile,subsample-1);
  }
  return ifile;
}

list<alignment> load_alignments(istream& ifile, const vector<string>& names, const alphabet& a,
				int skip, int maxalignments) 
{
  list<alignment> alignments;
  
  // we are using every 'skip-th' alignment
  int total = 0;

  alignment A(a);

  find_and_skip_alignments(ifile,skip);

  load_more_alignments(alignments,ifile,names,a,maxalignments);

  //------------  If we have too many alignments--------------//
  if (thin_alignments(alignments, maxalignments) and log_verbose)
  {
    cerr<<"Went from "<<total;
    cerr<<" to "<<alignments.size()<<" alignments.\n";
  }

  return alignments;
}

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets, 
				     int skip, int maxalignments)
{
  list<alignment> alignments;
  
  // we are using every 'skip-th' alignment
  int total = 0;

  find_and_skip_alignments(ifile,skip);

  alignment A = load_next_alignment(ifile,alphabets);

  vector<string> names = sequence_names(A);

  alignments.push_back(A);

  load_more_alignments(alignments,ifile,names,A.get_alphabet(),maxalignments);

  //------------  If we have too many alignments--------------//
  if (thin_alignments(alignments, maxalignments) and log_verbose)
  {
    cerr<<"Went from "<<total;
    cerr<<" to "<<alignments.size()<<" alignments.\n";
  }

  return alignments;
}

std::list<alignment> load_alignments(std::istream& ifile, const vector<string>& names, 
				     const std::vector<object_ptr<const alphabet> >& alphabets, 
				     int skip, int maxalignments)
{
  list<alignment> alignments;
  
  // we are using every 'skip-th' alignment
  int total = 0;

  find_and_skip_alignments(ifile,skip);

  alignment A = load_next_alignment(ifile,alphabets);

  {
    vector<int> mapping = compute_mapping(names, sequence_names(A));

    A = reorder_sequences(A,mapping);
  }

  alignments.push_back(A);

  load_more_alignments(alignments,ifile,names,A.get_alphabet(),maxalignments);

  //------------  If we have too many alignments--------------//
  if (thin_alignments(alignments, maxalignments) and log_verbose)
  {
    cerr<<"Went from "<<total;
    cerr<<" to "<<alignments.size()<<" alignments.\n";
  }

  return alignments;
}



vector<alignment> load_alignments(istream& ifile, const vector<object_ptr<const alphabet> >& alphabets) {
  vector<alignment> alignments;
  
  vector<string> n1;

  alignment A;
  while(ifile) {

    // CHECK if an alignment begins here
    if (ifile.peek() != '>') {
      string line;
      portable_getline(ifile,line);
      continue;
    }
    
    // READ the next alignment
    try {
      if (alignments.empty()) {
	A.load(alphabets,sequence_format::read_fasta,ifile);
	n1 = sequence_names(A);
      }
      else 
	ifile>>A;
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

    // STRIP out empty columns
    remove_empty_columns(A);

    // COMPLAIN if there are no sequences in the alignment
    if (A.n_sequences() == 0) 
      throw myexception(string("Alignment didn't contain any sequences!"));
    
    // Check the names and stuff.
    vector<string> n2 = sequence_names(A);

    if (n1 != n2) {
      // inverse of the mapping n2->n1
      vector<int> new_order = compute_mapping(n1,n2);
      A = reorder_sequences(A,new_order);
    }

    // STORE the alignment if we're not going to skip it
    alignments.push_back(A);
  }

  if (log_verbose) std::cerr<<"Loaded "<<alignments.size()<<" alignments.\n";

  return alignments;
}

alignment find_first_alignment(std::istream& ifile, const vector<object_ptr<const alphabet> >& alphabets) 
{
  alignment A;

  // for each line (nth is the line counter)
  string line;
  while(ifile) {
    
    // CHECK if an alignment begins here
    if (ifile.peek() != '>') {
      string line;
      portable_getline(ifile,line);
      continue;
    }
    
    try {
      // read alignment into A
      alignment A2;
      A2.load(alphabets,sequence_format::read_fasta,ifile);
      A = A2;

      // strip out empty columns
      remove_empty_columns(A);
      break;
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

  }

  if (A.n_sequences() == 0) 
    throw myexception()<<"No alignments found.";

  return A;
}

alignment find_last_alignment(std::istream& ifile, const vector<object_ptr<const alphabet> >& alphabets) 
{
  alignment A;

  // for each line (nth is the line counter)
  string line;
  while(ifile) {
    
    // CHECK if an alignment begins here
    if (ifile.peek() != '>') {
      string line;
      portable_getline(ifile,line);
      continue;
    }
    
    try {
      // read alignment into A
      alignment A2;
      A2.load(alphabets,sequence_format::read_fasta,ifile);
      A = A2;

      // strip out empty columns
      remove_empty_columns(A);
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }
  }

  if (A.n_sequences() == 0) 
    throw myexception()<<"No alignments found.";

  return A;
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

alignment select_columns(const alignment& A,const vector<int>& sites) 
{
  alignment A2 = A;
  A2.changelength(sites.size());
  for(int i=0;i<sites.size();i++) {
    int column = sites[i];
    for(int j=0;j<A2.n_sequences();j++)
      A2.set_value(i,j, A(column,j) );
  }
  return A2;
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

vector<vector<int> > alignment_letters(const alignment& A, int N)
{
  // Construct the new leaf sequences
  vector< vector<int> > S;

  for(int i=0;i<N;i++)
    S.push_back( alignment_row_letters(A,i));

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
