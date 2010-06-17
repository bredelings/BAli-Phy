/*
   Copyright (C) 2005,2009 Benjamin Redelings

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

#include "substitution-index.H"
#include "util.H"

#ifdef NDEBUG
#define IF_DEBUG(x)
#else
#define IF_DEBUG(x) x
#endif

using std::vector;

using boost::dynamic_bitset;

/* 
 * June 2010 - Rewriting substitution index.
 *
 * 1. Goal: Keep BOTH substitution index methods.
 *
 * 2. Implement each method as a separate class.
 *
 * 3. Separate the substitution index from the alignment, and
 *     connect it to the likelihood cache.
 *
 * 4. Allow the likelihood cache for each partition to
 *    know which of the two types of substitution indexing
 *     it is doing.
 *
 * 5. How must the likelihood calculation change to reflect
 *    the different subA indices?
 * 
 * 6. How are we going to keep the NOTE length in sync
 *    with the alignment length?
 *    - any resampling of the alignment means that the
 *      subA-indices must be recalculated.
 *    - however, some projections still will not change...
 *      but their associations with COLUMNS will change.
 *    - ... lazily? Pieces invalidated when length
 *      changes?
 *    
 * 7. How shall we deal with large insertions of ++++ that
 *    have no leaf characters behind them?
 *
 * So... where are the NOTES used?
 *    - 
bali-phy.C:      add_leaf_seq_note(*P[i].A, T.n_leaves());
bali-phy.C:      add_subA_index_note(*P[i].A, T.n_branches());

alignment.C:  for(int i=0;i<notes.size();i++)
alignment.C:    notes[i].resize(l+1,notes[i].size2());
 *
 * subA_update_index_single(int branch )
 *  + keep any alignment column where the node at the source of
 *    this branch, or either of its child branches, is present.
 *  + record a -1 for each column/branch where the node at the source
 *    of the column/branch is absent.
 *  + otherwise, record present character's into the sequence at
 *    that node.
 *
 * Likelihood:
 *  + If we have a -1 at the branch,
 *    - We should have a index >=0 in at most one child subA index.
 *    - We therefore apply frequencies, and add to other_subst.
 *  + If we have a +1 at the branch, we progagate any children
 *    and store at this index.
 */



/* The algorithms in this file provide each column in a sub-alignment
 * with a name that persists through alignment and tree changes that
 * do not make the column non-identifiable.  The name is a column
 * number, so that naming columns corresponds to ordering the columns
 * in the sub-alignment.   We need persistent names in order to cache
 * conditional likelihoods at columns in the sub-alignment,
 * specifically in order to cache across internal-node resampling.
 *
 * Our naming scheme for sub-alignment columns satisfies two
 * important properties: 
 * a) ordering depends only on pairwise alignments behind b and the
 *    topology of the subtree behind b
 * b) ordering depends only on the pairwise alignments PROJECTED TO
 *    THE LEAF SEQUENCES and does not change when the alignment of
 *    leaf sequences is unchanged. 
 *
 * a -> Each sub-alignment corresponds to a directed branch b on the
 * tree, and contains the leaf sequences BEHIND b. If alignments on
 * branches behind b do not change, then the sub-alignment b should
 * not change either.  This isn't automatic because the columns of the
 * sub-alignment may not be fully ordered.  Note that changing the
 * topology behind b would necessitate invalidating the cached
 * conditional likelihoods anyway, so we lose nothing by not
 * preserving column numbers in that case.
 * 
 * b-> This gives the useful property that re-sampling internal nodes
 * does not invalidate any of the names.  However, this implies that
 * the ordering of columns in a sub-alignment can conflict with
 * ordering in the full alignment, or with the order of sub-alignments
 * which include this sub-alignment. 
 */


namespace substitution {

  /// Are there characters present in column c of the alignment A at any of the nodes?
  static inline bool any_present(const alignment& A,int c, const vector<int>& nodes) {
    for(int i=0;i<nodes.size();i++)
      if (not A.gap(c,nodes[i])) 
	return true;
    return false;
  }


  int n_non_null_entries(const ublas::matrix<int>& m)
  {
    int total = 0;
    for(int i=0;i<m.size1();i++)
      for(int j=0;j<m.size2();j++)
	if (m(i,j) != -1)
	  total++;
    return total;
  }

  int n_non_empty_columns(const ublas::matrix<int>& m)
  {
    int total = 0;
    for(int i=0;i<m.size1();i++)
    {
      bool empty = true;
      for(int j=0;j<m.size2() and empty;j++)
	if (m(i,j) != -1)
	  empty = false;
      if (not empty)
	total++;
    }
    return total;
  }

  /// Select rows for branches \a branches, removing columns with all entries == -1
  ublas::matrix<int> subA_index(const subA_index_t& I, const vector<int>& branches)
  {
    // the alignment of sub alignments
    const int L = I.size1() - 1;
    ublas::matrix<int> subA(L, branches.size());

    // copy sub-A indices for each branch
    for(int j=0;j<branches.size();j++) 
    {
      if (branches[j] == -1)
	for(int c=0;c<L;c++)
	  subA(c,j) = -1;
      else 
      {
	assert(I.branch_index_valid(branches[j]));

	for(int c=0;c<L;c++)
	  subA(c,j) = I(c+1,branches[j]);
      }
    }

    return subA;
  }

  /// Select rows for branches \a branches, removing columns with all entries == -1
  ublas::matrix<int> subA_index(subA_index_t& I, const vector<int>& branches, const alignment& A,const Tree& T)
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),branches.size());

    // copy sub-A indices for each branch
    for(int j=0;j<branches.size();j++) 
    {
      if (branches[j] == -1)
	for(int c=0;c<A.length();c++)
	  subA(c,j) = -1;
      else 
      {
	IF_DEBUG( subA_index_check_footprint_for_branch(I,A,T,branches[j]) );

	if (not I.branch_index_valid(branches[j]))
	  I.update_branch(A,T,branches[j]);
	for(int c=0;c<A.length();c++)
	  subA(c,j) = I(c+1,branches[j]);
      }
    }

    return subA;
  }

  /// Compute subA index for branches point to \a node.
  ublas::matrix<int> subA_index(subA_index_t& I, int node,const alignment& A,const Tree& T) 
  {
    // compute node branches
    vector<int> b;
    for(const_in_edges_iterator i = T[node].branches_in();i;i++)
      b.push_back(*i);

    return subA_index(I,b,A,T);
  }

  /// Sort columns according to value in last row, removing columns with -1 in last row
  ublas::matrix<int> subA_select(const ublas::matrix<int>& subA1) {
    const int I = subA1.size2()-1;

    // count the number of columns to keep
    int L=0;
    for(int c=0;c<subA1.size1();c++)
      if (subA1(c,I) != alphabet::gap) L++;

    ublas::matrix<int> subA2(L,I);

    for(int c=0;c<subA1.size1();c++) {
      int c2 = subA1(c,I);
      if (c2 == alphabet::gap) continue;

      for(int j=0;j<subA2.size2();j++)
	subA2(c2,j) = subA1(c,j);
    }

    return subA2;
  }

  /// Select rows for branches \a b, and toss columns where the last branch has entry -1
  ublas::matrix<int> subA_index_select(const subA_index_t& I, const vector<int>& b) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(I,b);

    // return processed indices
    return subA_select(subA);
  }


  /// Select rows for branches \a b, and toss columns where the last branch has entry -1
  ublas::matrix<int> subA_index_select(subA_index_t& I, const vector<int>& b,const alignment& A,const Tree& T) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(I,b,A,T);

    // return processed indices
    return subA_select(subA);
  }


  /// Select rows for branches \a b, and toss columns unless at least one character in \a nodes is present.
  ublas::matrix<int> subA_index_any(subA_index_t& I, const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& nodes) 
  {
    vector<int> b2 = b;
    b2.push_back(-1);

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(I,b2,A,T);

    // select and order the columns we want to keep
    const int B = b.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,B) = l++;
      else
	subA(c,B) = alphabet::gap;

    // return processed indices
    return subA_select(subA);
  }

  // Idea is that columns which are (+,+,+) in terms of having leaves, but (+,+,-) in terms of having
  // present characters should get separated into (+,+,-) and (-,-,+), and we should use calc_root( )
  // on the first one, and a new calc_root_unaligned( ) on the second one.

  // So, for example if they are (+,+,+) in terms of having leaves, and (-,-,-) in terms of having 
  // present characters, should end up as (-,-,-) for calc_root( ) and (+,+,+) for calc_root_unaligned( ).

  /// Select rows for branches \a b, and toss ENTRIES where the character at the base of the branch is absent
  ublas::matrix<int> subA_index_aligned(subA_index_t& I, const vector<int>& b,const alignment& A,const Tree& T, bool present)
  {
    vector<int> nodes;
    for(int i=0;i<b.size();i++)
      nodes.push_back(T.directed_branch(b[i]).source());

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(I,b,A,T);

    // select and order the columns we want to keep
    for(int c=0;c<subA.size1();c++)
    {
      for(int i=0;i<nodes.size();i++)
      {
	// zero out entries if the character is absent (if present==true) or present (if present==false)
	if ((not A.character(c,nodes[i])) xor (not present))
	  subA(c, i) = alphabet::gap;
      }
    }

    // return processed indices
    return subA;
  }


  /// Select rows for branches \a b and columns present at nodes, but ordered according to the list of columns \a seq
  ublas::matrix<int> subA_index_any(subA_index_t& I, const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& IF_DEBUG(nodes), const vector<int>& seq) 
  {
    vector<int> b2 = b;
    b2.push_back(-1);

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(I,b2,A,T);

    // select and order the columns we want to keep
    const int B = b.size();
    for(int c=0;c<subA.size1();c++) 
      subA(c,B) = alphabet::gap;

    for(int i=0;i<seq.size();i++) 
      subA(seq[i],B) = i;

#ifndef NDEBUG
    // check reqs...
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	assert(subA(c,B)!=alphabet::gap);
      else
	assert(subA(c,B)==alphabet::gap);
#endif

    return subA_select(subA);
  }


  /// Select rows for branches \a b, but exclude columns in which nodes \a nodes are present.
  ublas::matrix<int> subA_index_none(subA_index_t& I, const vector<int>& b,const alignment& A,const Tree& T,
				     const vector<int>& nodes) 
  {
    vector<int> b2 = b;
    b2.push_back(-1);

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(I,b2,A,T);

    // select and order the columns we want to keep
    const int B = b.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,B) = alphabet::gap;
      else
	subA(c,B) = l++;

    // return processed indices
    return subA_select(subA);
  }

  std::ostream& print_subA(std::ostream& o,const ublas::matrix<int>& I)
  {
    o<<"["<<I.size1()<<","<<I.size2()<<"]\n";
    for(int j=0;j<I.size2();j++) 
      for(int i=0;i<I.size1();i++) {
	o<<I(i,j);
	if (i<I.size1()-1)
	  o<<"  ";
	else
	  o<<std::endl;
      }
    return o;
  }

  bool subA_identical(const ublas::matrix<int>& I1,const ublas::matrix<int>& I2) {
    bool error = false;
    if (I1.size1() != I2.size1()) error=true;
    if (I1.size2() != I2.size2()) error=true;
    
    if (not error) 
      for(int i=0;i<I1.size1() and not error;i++)
	for(int j=0;j<I1.size2() and not error;j++)
	  error = (I1(i,j) != I2(i,j));
    
    return not error;
  }

  // Check that all valid sub-alignments are identical?
  void check_subA(const subA_index_t& I1_, const alignment& A1,const subA_index_t& I2_, const alignment& A2,const Tree& T) 
  {
    for(int b=T.n_leaves();b<2*T.n_branches();b++) 
    {
      if (not I1_.branch_index_valid(b)) continue;
      if (not I2_.branch_index_valid(b)) continue;

      // compute branches-in
      vector<int> branches;
      for(const_in_edges_iterator e = T.directed_branch(b).branches_before();e;e++)
	branches.push_back(*e);
      assert(branches.size() == 2);
      
      vector<int> b2 = branches;
      b2.push_back(b);
	
      ublas::matrix<int> I1 = substitution::subA_index_select(I1_,b2);
      ublas::matrix<int> I2 = substitution::subA_index_select(I2_,b2);

      if (not subA_identical(I1,I2)) 
      {
	// print subAs alignments
	print_subA(std::cerr,I1)<<std::endl;
	std::cerr<<std::endl;
	print_subA(std::cerr,I2)<<std::endl;

	// print leaf sets in each subA
	for(int k=0;k<branches.size();k++) {
	  std::cerr<<"leaf set #"<<k+1<<" = ";
	  int lb = T.directed_branch(branches[k]).reverse();
	  for(int l=0;l<T.n_leaves();l++)
	    if (T.partition(lb)[l])
	      std::cerr<<l<<" ";
	  std::cerr<<std::endl;
	}
	
	// print alignments
	std::cerr<<A1<<std::endl;
	std::cerr<<A2<<std::endl;

	// recompute subAs so we can enter w/ debugger
	I1 = substitution::subA_index_select(I1_,b2);
	I2 = substitution::subA_index_select(I2_,b2);

	std::abort();
      }
    }
  }
} // end namespace substitition

/// create a note with leaf sequences ...
int add_leaf_seq_note(alignment& A,int n) 
{
  assert(n <= A.n_sequences());

  int index = A.add_note(n);

  for(int i=0;i<n;i++) {
    int l=0;
    for(int c=0;c<A.length();c++)
      if (not A.gap(c,i)) {
	A.note(index,l+1,i) = A(c,i);
	l++;
      }
    A.note(index,0,i) = l;
    assert(l == A.seqlength(i));
  }

  return index;
}

/// create a note with leaf sequences ...
int add_leaf_seq_note(alignment& A,const ublas::matrix<int>& M) 
{
  int n = M.size2();

  assert(n < A.n_sequences());

  int index = A.add_note(n);

  for(int i=0;i<n;i++) {
    const int l = M(0,i);
    assert(l == A.seq(i).size()/A.get_alphabet().width());
    for(int j=0;j<l;j++)
      A.note(index,j+1,i) = M(j+1,i);
    A.note(index,0,i) = l;
  }

  return index;
}

void subA_index_t::invalidate_one_branch(int b) 
{
  operator()(0,b) = -1;
}

void subA_index_t::invalidate_all_branches()
{
  for(int i=0;i<size2();i++)
    invalidate_one_branch(i);
}


void subA_index_t::invalidate_directed_branch(const Tree& T,int b) 
{
  vector<const_branchview> branches = branches_after(T,b);

  for(int i=0;i<branches.size();i++)
    invalidate_one_branch(branches[i]);
}


void subA_index_t::invalidate_branch(const Tree& T,int b) 
{
  invalidate_directed_branch(T, b);
  invalidate_directed_branch(T, T.directed_branch(b).reverse());
}



/// return index of lowest-numbered node behind b
int rank(const Tree& T,int b) {
  const dynamic_bitset<>& mask = T.partition(T.directed_branch(b).reverse());
  for(int i=0;i<mask.size();i++)
    if (mask[i])
      return i;

  std::abort();
}


void subA_index_t::update_one_branch(const alignment& A,const Tree& T,int b) 
{
  ublas::matrix<int>& I = *this;

  // lazy resizing
  if (size1() != A.length() + 1)
  {
    for(int i=0;i<size2();i++)
      assert(not branch_index_valid(i));
    resize(A.length()+1, size2());
  }

  // notes for leaf sequences
  if (b < T.n_leaves()) {
    int l=0;
    for(int c=0;c<A.length();c++) {
      if (A.gap(c,b))
	I(c+1,b) = alphabet::gap;
      else
	I(c+1,b) = l++;
    }
    assert(l == leaf_seq_length(A,b));
    I(0,b) = l;
  }
  else {
    // get 2 branches leading into this one
    vector<const_branchview> prev;
    append(T.directed_branch(b).branches_before(),prev);
    assert(prev.size() == 2);

    // sort branches by rank
    if (rank(T,prev[0]) > rank(T,prev[1]))
      std::swap(prev[0],prev[1]);

    // get mappings of previous subA indices into alignment
    vector<vector<int> > mappings;
    for(int i=0;i<prev.size();i++) {
      assert(branch_index_valid(prev[i]));
      mappings.push_back(vector<int>(branch_index_length(prev[i]),-1));
    }

    int l=0;
    for(int c=0;c<A.length();c++) {
      bool present = false;
      for(int i=0;i<mappings.size();i++) {
	int index = I(c+1,prev[i]);
	assert(index < (int)mappings[i].size());

	if (index != -1) {
	  mappings[i][index] = c;
	  present = true;
	}
      }
      if (present) {
	l++;
	I(c+1,b) = -2;
      }
      else
	I(c+1,b) = -1;
    }

    // create subA index for this branch
    I(0,b) = l;
    l = 0;
    for(int i=0;i<mappings.size();i++) {
      for(int j=0;j<mappings[i].size();j++) {
	int c = mappings[i][j];

	// all the subA columns should map to an existing, unique columns of A
	assert(c != -1);

	// subA for b should be present here
	assert(I(c+1,b) != -1);

	if (I(c+1,b) == -2)
	  I(c+1,b) = l++;
      }
    }
    assert(l == I(0,b));
  }
}

void subA_index_t::update_branch(const alignment& A,const Tree& T,int b) 
{
#ifndef NDEBUG  
  subA_index_check_footprint(*this,A,T);
#endif

  // get ordered list of branches to process before this one
  vector<const_branchview> branches; branches.reserve(T.n_branches());
  branches.push_back(T.directed_branch(b));
  
  for(int i=0;i<branches.size();i++) {
    const const_branchview& db = branches[i];
    if (not branch_index_valid(db))
      append(db.branches_before(),branches);
  }
  
  std::reverse(branches.begin(),branches.end());

  // update the branches in order 
  for(int i=0;i<branches.size();i++)
    update_one_branch(A,T,branches[i]);

#ifndef NDEBUG  
  subA_index_check_footprint(*this,A,T);

  // FIXME - we should check the branches that point to the root, but we
  // don't know the root, so just disable the checking here.
  // FIXME - this could actually be very expensive to check every branch,
  //         probably it would be O(b^2)
  if (not subA_index_may_have_invalid_branches())
    subA_index_check_regenerate(*this,A,T);
#endif
}

void subA_index_t::recompute_all_branches(const alignment& A,const Tree& T) 
{
  invalidate_all_branches();

  vector<const_branchview> branches = branches_from_leaves(T);

  for(int i=0;i<branches.size();i++) 
    update_one_branch(A,T, branches[i]);
}

bool allow_invalid_branches_ = false;

int subA_index_may_have_invalid_branches()
{
  return allow_invalid_branches_;
}

void subA_index_allow_invalid_branches(bool allowed)
{
  allow_invalid_branches_ = allowed;
}

void subA_index_check_regenerate(const subA_index_t& I1, const alignment& A,const Tree& T) 
{
  vector<int> branch_names = iota<int>(T.n_branches()*2);

  // compare against calculation from scratch
  subA_index_t I2 = I1;
  I2.recompute_all_branches(A, T);

  for(int i=0;i<branch_names.size();i++) {
    int b = branch_names[i];
    if (I1.branch_index_valid(b)) {
      assert(I1.branch_index_length(b) == I2.branch_index_length(b));
      for(int c=0;c<A.length();c++)
	assert(I1(c+1,b) == I2(c+1,b));
    }
  }
}

void subA_index_check_regenerate(const subA_index_t& I1, const alignment& A,const Tree& T,int root) 
{
  vector<int> branch_names = iota<int>(T.n_branches()*2);

  if (subA_index_may_have_invalid_branches())
    branch_names = directed_names(branches_toward_node(T,root));

  // compare against calculation from scratch
  subA_index_t I2 = I1;
  I2.recompute_all_branches(A, T);

  for(int i=0;i<branch_names.size();i++) {
    int b = branch_names[i];
    if (I1.branch_index_valid(b)) {
      assert(I1.branch_index_length(b) == I2.branch_index_length(b));
      for(int c=0;c<A.length();c++)
	assert(I1(c+1,b) == I2(c+1,b));
    }
  }
}


// If branch 'b' is markes as having an up-to-date index, then
//  * check that the index includes each column for which there are leaf characters behind the branch ...
//  * ... and no others. 
// That is, if a column includes only gaps behind the branch, then it should not be in the branch's index.
void subA_index_check_footprint_for_branch(const subA_index_t& I, const alignment& A, const Tree& T, int b)
{
  // Don't check here if we're temporarily messing with things, and allowing a funny state.
  if (not I.branch_index_valid(b)) return;

  for(int c=0;c<A.length();c++) 
  {
    // Determine if there are any leaf characters behind branch b in column c
    bool leaf_present = false;
    const dynamic_bitset<>& leaves = T.partition(T.directed_branch(b).reverse());
    for(int i=0;i<T.n_leaves();i++)
      if (leaves[i] and not A.gap(c,i))
	leaf_present=true;
    
    // If so, then this column should have a non-null (null==-1) index for this branch.
    if (leaf_present)
      assert(I(c+1,b) != -1);
    // Otherwise, this column should how have an index for this branch.
    else
      assert(I(c+1,b) == -1);
  }
}

// Check that for each branch that is marked as having an up-to-date index,
// we include each column in the index for which there are leaf characters that are behind the branch.
// (We need to propagate conditional likelihoods up to the root, then.)
// Also check that, we do not include in the index any columns for which there are only gaps behind
// the  branch.

void subA_index_check_footprint(const subA_index_t& I, const alignment& A,const Tree& T) 
{
  if (subA_index_may_have_invalid_branches())
    return;

  for(int b=0;b<T.n_branches()*2;b++)
    subA_index_check_footprint_for_branch(I,A,T,b);
}

alignment blank_copy(const alignment& A1,int length) 
{
  alignment A2;

  // make an array w/ the same alphabet & sequences
  A2.a = A1.a;
  A2.sequences = A1.sequences;

  // make a blank array
  A2.array.resize(length, A1.array.size2());

  // make blank notes
  A2.notes.reserve(A1.notes.size());

  if (A1.notes.size() >= 1)
    add_leaf_seq_note(A2,A1.note(0));

  return A2;
}

subA_index_t::subA_index_t(int s1, int s2)
  :ublas::matrix<int>(s1,s2)
{
  invalidate_all_branches();
}
