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

#ifdef NDEBUG_INDEXING
#define IF_DEBUG_I(x)
#else
#define IF_DEBUG_I(x) x
#endif

using std::vector;
using std::pair;

using boost::dynamic_bitset;


vector<pair<int,int> > sparsify_row(const ublas::matrix<int>& M, int r)
{
  vector<pair<int,int> > columns;
  for(int c=0;c<M.size1();c++)
    if (M(c,r) != -1)
      columns.push_back(pair<int,int>(c,M(c,r)));

  return columns;
}

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

int subA_index_t::n_rows() const
{
  return indices.size();
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

/// Select rows for branches \a branches, removing columns with all entries == -1
ublas::matrix<int> subA_index_t::get_subA_index(const vector<int>& branches, bool with_columns) const
{
  const ublas::matrix<int>& I = *this;

  // the alignment of sub alignments
  const int L = I.size1();
  ublas::matrix<int> subA(L, branches.size() + (with_columns?1:0));

  // copy sub-A indices for each branch
  for(int j=0;j<branches.size();j++) 
  {
    assert(branch_index_valid(branches[j]));

    for(int c=0;c<L;c++)
      subA(c,j) = I(c,branches[j]);
  }

  if (with_columns)
    for(int c=0;c<L;c++)
      subA(c,branches.size()) = c;

  return subA;
}

/// Select rows for branches \a branches, removing columns with all entries == -1
ublas::matrix<int> subA_index_t::get_subA_index(const vector<int>& branches, const alignment& A,const Tree& T, bool with_columns)
{
  // copy sub-A indices for each branch
  for(int j=0;j<branches.size();j++) 
  {
    IF_DEBUG_I( check_footprint_for_branch(A,T,branches[j]) );

    if (not branch_index_valid(branches[j]))
      update_branch(A,T,branches[j]);
  }

  return get_subA_index(branches, with_columns);
}

/// Select rows for branches \a branches, removing columns with all entries == -1
ublas::matrix<int> subA_index_t::get_subA_index_sparse(const vector<int>& branches, bool with_columns) const
{
  const ublas::matrix<int>& I = *this;

  // the alignment of sub alignments
  const int L = I.size1();
  ublas::matrix<int> subA(L, branches.size() + (with_columns?1:0));

  // check that all the branches are valid
  for(int j=0;j<branches.size();j++) 
    assert(branch_index_valid(branches[j]));

  // copy sub-A indices for each branch
  int l=0;
  for(int c=0;c<L;c++)
  {
    bool empty = true;
    for(int j=0;j<branches.size();j++) 
    {
      subA(l,j) = I(c,branches[j]);
      if (I(c,branches[j]) != -1)
	empty = false;
    }
    if (with_columns)
      subA(l,branches.size()) = c;

    if (not empty) l++;
  }

  ublas::matrix<int> subA2(l, subA.size2());
  for(int i=0;i<subA2.size1();i++)
    for(int j=0;j<subA2.size2();j++)
      subA2(i,j) = subA(i,j);

  return subA2;
}

/// Select rows for branches \a branches, removing columns with all entries == -1
ublas::matrix<int> subA_index_t::get_subA_index_sparse(const vector<int>& branches, const alignment& A,const Tree& T, bool with_columns)
{
  // copy sub-A indices for each branch
  for(int j=0;j<branches.size();j++) 
  {
    IF_DEBUG_I( check_footprint_for_branch(A,T,branches[j]) );

    if (not branch_index_valid(branches[j]))
      update_branch(A,T,branches[j]);
  }

  return get_subA_index_sparse(branches, with_columns);
}

/// Compute subA index for branches point to \a node.
ublas::matrix<int> subA_index_t::get_subA_index(int node,const alignment& A,const Tree& T) 
{
  // compute node branches
  vector<int> b;
  for(const_in_edges_iterator i = T[node].branches_in();i;i++)
    b.push_back(*i);

  return get_subA_index(b,A,T);
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
ublas::matrix<int> subA_index_t::get_subA_index_select(const vector<int>& b) const
{
  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index(b);

  // return processed indices
  return subA_select(subA);
}


/// Select rows for branches \a b, and toss columns where the last branch has entry -1
ublas::matrix<int> subA_index_t::get_subA_index_select(const vector<int>& b,const alignment& A,const Tree& T) 
{
  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index(b,A,T);

  // return processed indices
  return subA_select(subA);
}


/// Select rows for branches \a b, and toss columns where the last branch has entry -1
ublas::matrix<int> subA_index_t::get_subA_index_vanishing(const vector<int>& b,const alignment& A,const Tree& T) 
{
  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index(b,A,T);

  const int B = b.size()-1;
  int l=0;
  for(int c=0;c<subA.size1();c++)
  {
    bool child_present = false;
    for(int i=0;i<b.size()-1 and not child_present;i++)
      if (subA(c,i) != alphabet::gap)
	child_present = true;

    if (child_present and subA(c,B) == alphabet::gap)
      subA(c,B) = l++;
    else
      subA(c,B) = alphabet::gap;
  }

  // return processed indices
  return subA_select(subA);
}


/// Select rows for branches \a b, and toss columns unless at least one character in \a nodes is present.
ublas::matrix<int> subA_index_t::get_subA_index_any(const vector<int>& b,const alignment& A,const Tree& T,
						    const vector<int>& nodes) 
{
  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index_sparse(b,A,T,true);

  // select and order the columns we want to keep
  const int B = b.size();
  for(int i=0,l=0;i<subA.size1();i++)
  {
    int c = subA(i,b.size());
    if (any_present(A,c,nodes))
      subA(i,B) = l++;
    else
      subA(i,B) = -1;
  }

  // return processed indices
  return subA_select(subA);
}

/// Select rows for branches \a b, but exclude columns in which nodes \a nodes are present.
ublas::matrix<int> subA_index_t::get_subA_index_none(const vector<int>& b,const alignment& A,const Tree& T,
						     const vector<int>& nodes) 
{
  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index_sparse(b,A,T,true);

  // select and order the columns we want to keep
  const int B = b.size();
  for(int i=0,l=0;i<subA.size1();i++)
  {
    int c = subA(i,b.size());
    if (not any_present(A,c,nodes))
      subA(i,B) = l++;
    else
      subA(i,B) = -1;
  }

  // return processed indices
  return subA_select(subA);
}

// Idea is that columns which are (+,+,+) in terms of having leaves, but (+,+,-) in terms of having
// present characters should get separated into (+,+,-) and (-,-,+), and we should use calc_root( )
// on the first one, and a new calc_root_unaligned( ) on the second one.

// So, for example if they are (+,+,+) in terms of having leaves, and (-,-,-) in terms of having 
// present characters, should end up as (-,-,-) for calc_root( ) and (+,+,+) for calc_root_unaligned( ).

/// Select rows for branches \a b, and toss ENTRIES where the character at the base of the branch is absent
ublas::matrix<int> subA_index_t::get_subA_index_aligned(const vector<int>& b,const alignment& A,const Tree& T, bool present)
{
  vector<int> nodes;
  for(int i=0;i<b.size();i++)
    nodes.push_back(T.directed_branch(b[i]).source());

  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index(b,A,T);

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
ublas::matrix<int> subA_index_t::get_subA_index_columns(const vector<int>& b,const alignment& A,const Tree& T,
							const vector<int>& ordered_columns) 
{
  // the alignment of sub alignments
  ublas::matrix<int> subA = get_subA_index(b,A,T,true);

  // select and order the columns we want to keep
  const int B = b.size();
  for(int c=0;c<subA.size1();c++) 
    subA(c,B) = alphabet::gap;

  for(int i=0;i<ordered_columns.size();i++) 
    subA(ordered_columns[i],B) = i;

  return subA_select(subA);
}


std::ostream& print_subA(std::ostream& o,const ublas::matrix<int>& I)
{
  o<<"["<<I.size1()<<","<<I.size2()<<"]\n";
  for(int j=0;j<I.size2();j++) 
  {
    o<<"["<<j<<"]:   ";

    for(int i=0;i<I.size1();i++) 
    {
	o<<I(i,j);
	if (i<I.size1()-1)
	  o<<"  ";
	else
	  o<<std::endl;
    }
  }
  return o;
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
	
    ublas::matrix<int> I1 = I1_.get_subA_index_select(b2);
    ublas::matrix<int> I2 = I2_.get_subA_index_select(b2);

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
      I1 = I1_.get_subA_index_select(b2);
      I2 = I2_.get_subA_index_select(b2);

      std::abort();
    }
  }
}

void subA_index_t::invalidate_one_branch(int b) 
{
  up_to_date[b] = false;
  indices[b].clear();
}

void subA_index_t::invalidate_all_branches()
{
  for(int i=0;i<n_rows();i++)
    invalidate_one_branch(i);
}


void subA_index_t::invalidate_directed_branch(const Tree& T,int b) 
{
  vector<const_branchview> branches = branches_after_inclusive(T,b);

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


void subA_index_t::update_branch(const alignment& A,const Tree& T,int b) 
{
#ifdef DEBUG_INDEXING
  check_footprint(A,T);
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
    if (not branch_index_valid(branches[i]))
      update_one_branch(A,T,branches[i]);

#ifdef DEBUG_INDEXING
  check_footprint(A,T);

  // FIXME - we should check the branches that point to the root, but we
  // don't know the root, so just disable the checking here.
  // FIXME - this could actually be very expensive to check every branch,
  //         probably it would be O(b^2)
  if (not may_have_invalid_branches())
    check_regenerate(*this,A,T);
#endif
}

void subA_index_t::recompute_all_branches(const alignment& A,const Tree& T) 
{
  invalidate_all_branches();

  vector<const_branchview> branches = branches_from_leaves(T);

  for(int i=0;i<branches.size();i++) 
    update_one_branch(A,T, branches[i]);
}

bool subA_index_t::may_have_invalid_branches() const
{
  return allow_invalid_branches_;
}

void subA_index_t::allow_invalid_branches(bool allowed)
{
  allow_invalid_branches_ = allowed;
}

void check_consistent(const subA_index_t& I1, const subA_index_t& IF_DEBUG(I2), const vector<int>& branch_names)
{
  assert(I1.n_rows() == I2.n_rows());

  for(int i=0;i<branch_names.size();i++) 
  {
    int b = branch_names[i];
    if (I1.branch_index_valid(b)) 
    {
      // These lengths need be valid only if there is at least one valid branch
      assert(I1.size1() == I2.size1());

      const int L = I1.branch_index_length(b);
      assert(L == I2.branch_index_length(b));
      for(int c=0;c<L;c++)
	assert(I1(c,b) == I2(c,b));
    }
  }
}

void check_consistent(const subA_index_t& I1, const subA_index_t& I2)
{
  check_consistent(I1, I2, iota<int>(I1.n_rows()));
}

void check_regenerate(const subA_index_t& I1, const alignment& A,const Tree& T) 
{
  vector<int> branch_names = iota<int>(T.n_branches()*2);

  // compare against calculation from scratch
  owned_ptr<subA_index_t> I2 = I1;
  I2->recompute_all_branches(A, T);

  check_consistent(I1, *I2);
}

void check_regenerate(const subA_index_t& I1, const alignment& A,const Tree& T,int root) 
{
  vector<int> branch_names = iota<int>(T.n_branches()*2);

  if (I1.may_have_invalid_branches())
    branch_names = directed_names(branches_toward_node(T,root));

  // compare against calculation from scratch
  owned_ptr<subA_index_t> I2 = I1;
  I2->recompute_all_branches(A, T);

  check_consistent(I1, *I2, branch_names);
}


// Check that for each branch that is marked as having an up-to-date index,
// we include each column in the index for which there are leaf characters that are behind the branch.
// (We need to propagate conditional likelihoods up to the root, then.)
// Also check that, we do not include in the index any columns for which there are only gaps behind
// the  branch.

void subA_index_t::check_footprint(const alignment& A,const Tree& T) const
{
  if (may_have_invalid_branches())
    return;

  for(int b=0;b<T.n_branches()*2;b++)
    check_footprint_for_branch(A,T,b);
}

vector<int> subA_index_t::characters_to_indices(int branch, const alignment& A, const Tree& T)
{
  // Make sure the index for this branch is up to date before we start using it.
  update_branch(A,T,branch);

  int node = T.directed_branch(branch).source();

  vector<int> suba_for_character(A.seqlength(node), -1);

  // walk the alignment row and the subA-index row simultaneously
  for(int c=0,k=0;c<A.length();c++)
  {
    if (not A.character(c,node)) continue;

    int index = (*this)(c,branch);
    assert(index != -1);

    suba_for_character[k++] = index;
  }

  return suba_for_character;
}

subA_index_t::subA_index_t(int s1, int s2)
  :ublas::matrix<int>(s1,s2),
   indices(s2),
   up_to_date(s2),
   allow_invalid_branches_(false)
{
  invalidate_all_branches();
}

/// Map the indices in p1 to the array indices of p2 which contain the same columns.
vector<int> indices_to_present_columns(const vector<pair<int,int> >& p1, const vector<pair<int,int> >& p2)
{
  vector<int> indices_map(p1.size(), -1);

  int I1 = 0;
  int I2 = 0;
  while(I1 < p1.size() or I2 < p2.size())
  {
    if (I2 >= p2.size())
    {
      I1++;
    }
    else if (I1 >= p1.size())
    {
      I2++;
    }
    else if (p1[I1].first < p2[I2].first)
    {
      I1++;
    }
    else if (p1[I1].first > p2[I2].first)
    {
      I2++;
    }
    else  // p1[I1].first == p2[I2].first)
    {
      int index = p1[I1].second;
      indices_map[index] = I2;
      I1++;
      I2++;
    }
  }

  return indices_map;
}

/// Get the sorted list of columns present in either p1 or p2, with -1 for each index.
vector<pair<int,int> > combine_columns(const vector<pair<int,int> >& p1, const vector<pair<int,int> >& p2)
{
  vector<pair<int,int> > p3;

  int I1 = 0;
  int I2 = 0;
  while(I1 < p1.size() or I2 < p2.size())
  {
    int c = -1;
    if (I2 >= p2.size())
    {
      c = p1[I1].first;
      I1++;
    }
    else if (I1 >= p1.size())
    {
      c = p2[I2].first;
      I2++;
    }
    else if (p1[I1].first < p2[I2].first)
    {
      c = p1[I1].first;
      I1++;
    }
    else if (p1[I1].first > p2[I2].first)
    {
      c = p2[I2].first;
      I2++;
    }
    else  // p1[I1].first == p2[I2].first)
    {
      c = p1[I1].first;
      I1++;
      I2++;
    }

    p3.push_back(pair<int,int>(c,-1));
  }
  return p3;
}

void subA_index_leaf::update_one_branch(const alignment& A,const Tree& T,int b) 
{
  ublas::matrix<int>& I = *this;

  // lazy resizing
  if (size1() != A.length())
  {
    for(int i=0;i<n_rows();i++)
      assert(not branch_index_valid(i));
    resize(A.length(), n_rows());
  }

  // Reset to have no characters
  indices[b].clear();

  // notes for leaf sequences
  if (b < T.n_leaves()) 
  {
    int k=0;
    for(int c=0;c<A.length();c++)
      if (A.character(c,b))
	indices[b].push_back(std::pair<int,int>(c,k++));
  }
  else {
    // get 2 branches leading into this one
    vector<const_branchview> prev;
    append(T.directed_branch(b).branches_before(),prev);
    assert(prev.size() == 2);

    // sort branches by rank
    if (rank(T,prev[0]) > rank(T,prev[1]))
      std::swap(prev[0],prev[1]);

    // get the sorted list of present columns
    indices[b] = combine_columns(indices[prev[0]], indices[prev[1]]);

    int l=0;
    for(int i=0;i<prev.size();i++)
    {
      vector<int> index_to_present_columns = indices_to_present_columns(indices[prev[i]], indices[b]);

      for(int j=0;j<index_to_present_columns.size();j++)
      {
	int k = index_to_present_columns[j];
	if (indices[b][k].second == -1)
	  indices[b][k].second = l++;
      }
    }
    assert(l == indices[b].size());
  }

  // notes for leaf sequences
  if (b < T.n_leaves()) {
    int l=0;
    for(int c=0;c<A.length();c++) {
      if (A.gap(c,b))
	I(c,b) = alphabet::gap;
      else
	I(c,b) = l++;
    }
    up_to_date[b] = true;
    assert(l == indices[b].size());
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
	int index = I(c,prev[i]);
	assert(index < (int)mappings[i].size());

	if (index != -1) {
	  mappings[i][index] = c;
	  present = true;
	}
      }
      if (present) {
	l++;
	I(c,b) = -2;
      }
      else
	I(c,b) = -1;
    }

    // create subA index for this branch
    assert(l == indices[b].size());
    l = 0;
    for(int i=0;i<mappings.size();i++) {
      for(int j=0;j<mappings[i].size();j++) {
	int c = mappings[i][j];

	// all the subA columns should map to an existing, unique columns of A
	assert(c != -1);

	// subA for b should be present here
	assert(I(c,b) != -1);

	if (I(c,b) == -2)
	  I(c,b) = l++;
      }
    }
    assert(l == indices[b].size());
  }

  assert(indices[b] == sparsify_row(*this,b));

  up_to_date[b] = true;
}

// If branch 'b' is markes as having an up-to-date index, then
//  * check that the index includes each column for which there are leaf characters behind the branch ...
//  * ... and no others. 
// That is, if a column includes only gaps behind the branch, then it should not be in the branch's index.
void subA_index_leaf::check_footprint_for_branch(const alignment& A, const Tree& T, int b) const
{
  // Don't check here if we're temporarily messing with things, and allowing a funny state.
  if (not branch_index_valid(b)) return;

#ifndef NDEBUG
  const ublas::matrix<int>& I = *this;
#endif

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
      assert(I(c,b) != -1);
    // Otherwise, this column should how have an index for this branch.
    else
      assert(I(c,b) == -1);
  }
}

subA_index_leaf::subA_index_leaf(int s1, int s2)
  :subA_index_t(s1,s2)
{
}


void subA_index_internal::update_one_branch(const alignment& A,const Tree& T,int b) 
{
  assert(not up_to_date[b]);
  ublas::matrix<int>& I = *this;

  // lazy resizing
  if (size1() != A.length())
  {
    for(int i=0;i<n_rows();i++)
      assert(not branch_index_valid(i));
    resize(A.length(), n_rows());
  }

  // Actually update the index
  int node = T.directed_branch(b).source();

  int l=0;
  for(int c=0;c<A.length();c++) {
    if (A.character(c,node))
    {
      indices[b].push_back(pair<int,int>(c,l));
      I(c,b) = l++;
    }
    else
      I(c,b) = alphabet::gap;
  }
  assert(l == A.seqlength(node));

  assert(indices[b] == sparsify_row(*this,b));

  up_to_date[b] = true;
}

void subA_index_internal::check_footprint_for_branch(const alignment& A, const Tree& T, int b) const
{
  assert(A.n_sequences() == T.n_nodes());

  // Don't check here if we're temporarily messing with things, and allowing a funny state.
  if (not branch_index_valid(b)) return;

#ifndef NDEBUG
  const ublas::matrix<int>& I = *this;
#endif

  int node = T.directed_branch(b).source();

  for(int c=0;c<A.length();c++) 
  {
    // Determine if there is an internal node character present at the base of this branch
    bool internal_node_present = A.character(c,node);

    // If so, then this column should have a non-null (null==-1) index for this branch.
    if (internal_node_present)
      assert(I(c,b) != -1);
    // Otherwise, this column should how have an index for this branch.
    else
      assert(I(c,b) == -1);
  }
}

subA_index_internal::subA_index_internal(int s1, int s2)
  :subA_index_t(s1,s2)
{
}
