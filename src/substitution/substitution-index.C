// #define DEBUG_INDEXING
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
#include "models/parameters.H"

#ifdef NDEBUG
#define IF_DEBUG(x)
#else
#define IF_DEBUG(x) x
#endif

#ifdef DEBUG_INDEXING
#define IF_DEBUG_I(x) x
#else
#define IF_DEBUG_I(x)
#endif

using std::vector;
using std::pair;

using boost::dynamic_bitset;

int total_subA_index_matrix=0;
int total_subA_index_branch=0;

/// Take a list of [(column,index)] sorted by column and extract the sorted list of columns.
vector<int> convert_to_column_list(const vector<pair<int,int> >& column_indices)
{
  vector<int> order(column_indices.size(),-1);
  for(int i=0;i<column_indices.size();i++)
    order[column_indices[i].second] = column_indices[i].first;
  
  return order;
}

/// Take a sorted list of [column] and convert it to [(column,index)]
Vector<pair<int,int> > convert_to_column_index_list(const vector<int>& column_indices)
{
  Vector<pair<int,int> > indices(column_indices.size());
  for(int i=0;i<column_indices.size();i++)
    indices[i] = {column_indices[i],i};
  
  return indices;
}

/// Are there characters present in column c of the alignment A at any of the nodes?
static inline bool any_present(const alignment& A,int c, const vector<int>& nodes) {
  for(int i=0;i<nodes.size();i++)
    if (not A.gap(c,nodes[i])) 
      return true;
  return false;
}


int n_non_null_entries(const matrix<int>& m)
{
  int total = 0;
  for(int i=0;i<m.size1();i++)
    for(int j=0;j<m.size2();j++)
      if (m(i,j) != -1)
	total++;
  return total;
}

int n_non_empty_columns(const matrix<int>& m)
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

const Parameters& subA_index_t::P() const
{
  return *DP->P;
}

const alignment& subA_index_t::A() const
{
  return DP->A();
}

TreeInterface subA_index_t::T() const
{
  return DP->t();
}

const expression_ref& subA_index_t::row_expression(int r) const
{
  return P().evaluate( row_indices[r] );
}

const Vector<std::pair<int,int> >& subA_index_t::row(int r) const
{
  return row_expression(r).as_<Vector<pair<int,int>>>();
}

int subA_index_t::n_rows() const
{
  return row_indices.size();
}

bool subA_identical(const matrix<int>& I1,const matrix<int>& I2) {
  bool error = false;
  if (I1.size1() != I2.size1()) error=true;
  if (I1.size2() != I2.size2()) error=true;
    
  if (not error) 
    for(int i=0;i<I1.size1() and not error;i++)
      for(int j=0;j<I1.size2() and not error;j++)
	error = (I1(i,j) != I2(i,j));
    
  return not error;
}

/// Find the current active (e.g. smallest) column - w/o incrementing I at all.  We could perhaps call this current_column( ).
int next_column(const vector< expression_ref >& indices, const vector<int>& I)
{
  int m = -1;
  for(int i=0;i<indices.size();i++)
  {
    const auto& index = indices[i].as_<Vector<pair<int,int>>>();

    int ii = I[i];
    
    if (ii >= index.size()) continue;

    if (m == -1)
      m = index[ii].first;
    else
      m = std::min(m, index[ii].first);
  }
  return m;
}

/// Find the current active (e.g. smallest) column - w/o incrementing I at all.  We could perhaps call this current_column( ).
int next_column(const vector< expression_ref >& indices, const vector< vector<int> >& sequence_indices, const vector<int>& I)
{
  int m = -1;
  for(int i=0;i<indices.size();i++)
  {
    const auto& index = indices[i].as_<Vector<pair<int,int>>>();

    int ii = I[i];
    if (ii >= index.size()) continue;

    if (m == -1)
      m = index[ii].first;
    else
      m = std::min(m, index[ii].first);
  }

  for(int n=0;n<sequence_indices.size();n++)
  {
    int i = n + indices.size();

    int ii = I[i];
    if (ii >= sequence_indices[n].size()) continue;

    if (m == -1)
      m = sequence_indices[n][ii];
    else
      m = std::min(m, sequence_indices[n][ii]);
  }

  return m;
}

/// Select rows for branches \a branches, removing columns with all entries == -1
matrix<int> subA_index_t::get_subA_index(const vector<int>& branches, bool with_columns) const
{
  // Compute the total length of branch indices.
  // Also check that the indices are up-to-date.

  int total_length = 0;
  for(int i=0;i<branches.size();i++)
  {
    int B = branches[i];
    total_length += branch_index_length(B);
  }

  // The alignment of sub alignments
  matrix<int> subA3(total_length, branches.size() + (with_columns?1:0));

  vector<expression_ref> sub_indices;
  for(int b: branches)
    sub_indices.push_back(row_expression(b));

  vector<int> I(branches.size(),0);
  int L = 0;
  while(true)
  {
    int c = next_column(sub_indices, I);
    if (c == -1) break;

    for(int i=0;i<sub_indices.size();i++)
    {
      const auto& index = sub_indices[i].as_<Vector<pair<int,int>>>();

      int j = I[i];

      if (j < index.size() and index[j].first == c) 
      {
	subA3(L,i) = index[j].second;
	I[i]++;
      }
      else
	subA3(L,i) = -1;
    }

    if (with_columns)
      subA3(L,branches.size()) = c;

    L++;
  }

  matrix<int> subA4(L, subA3.size2() );
  for(int i=0;i<subA4.size1();i++)
    for(int j=0;j<subA4.size2();j++)
      subA4(i,j) = subA3(i,j);

  total_subA_index_matrix++;
  return subA4;
}

/// align sub-alignments corresponding to branches in b
matrix<int> subA_index_t::get_subA_index_with_nodes(const std::vector<int>& branches,const vector<vector<int>>& sequence_indices, bool with_columns)
{
   // Compute the total length of branch indices.
  // Also check that the indices are up-to-date.

  int total_length = 0;
  for(int B: branches)
    total_length += branch_index_length(B);

  for(const auto& v: sequence_indices)
    total_length += v.size();

  // The alignment of sub alignments
  int C = sequence_indices.size() + branches.size();
  matrix<int> subA3(total_length, branches.size() + sequence_indices.size() + (with_columns?1:0));

  vector<expression_ref> sub_indices;
  for(int b: branches)
    sub_indices.push_back(row_expression(b));

  vector<int> I(branches.size()+sequence_indices.size(),0);
  int L = 0;
  while(true)
  {
    int c = next_column(sub_indices, sequence_indices, I);
    if (c == -1) break;

    for(int i=0;i<sub_indices.size();i++)
    {
      const auto& index = sub_indices[i].as_<Vector<pair<int,int>>>();

      int j = I[i];

      if (j < index.size() and index[j].first == c) 
      {
	subA3(L,i) = index[j].second;
	I[i]++;
      }
      else
	subA3(L,i) = -1;
    }

    for(int i=0;i<sequence_indices.size();i++)
    {
      int n = i + sub_indices.size();
      int j = I[n];

      if (j < sequence_indices[i].size() and sequence_indices[i][j] == c)
      {
	subA3(L,n) = j;
	I[n]++;
      }
      else
	subA3(L,n) = -1;
    }

    if (with_columns)
      subA3(L,C) = c;

    L++;
  }

  matrix<int> subA4(L, subA3.size2() );
  for(int i=0;i<subA4.size1();i++)
    for(int j=0;j<subA4.size2();j++)
      subA4(i,j) = subA3(i,j);

  return subA4;
}

/// align sub-alignments corresponding to branches in b
matrix<int> subA_index_t::get_subA_index_with_nodes(const std::vector<int>& branches,const std::vector<int>& nodes,bool with_columns)
{
  vector< vector<int> > sequence_indices;
  for(int n: nodes)
    sequence_indices.push_back(A().get_columns_for_characters(n));

  return get_subA_index_with_nodes(branches, sequence_indices, with_columns);
}

/// Sort columns according to value in last row, removing columns with -1 in last row
matrix<int> subA_select(const matrix<int>& subA1) {
  const int I = subA1.size2()-1;

  // count the number of columns to keep
  int L=0;
  for(int c=0;c<subA1.size1();c++)
    if (subA1(c,I) != alphabet::gap) L++;

  matrix<int> subA2(L,I);

  for(int c=0;c<subA1.size1();c++) {
    int c2 = subA1(c,I);
    if (c2 == alphabet::gap) continue;

    for(int j=0;j<subA2.size2();j++)
      subA2(c2,j) = subA1(c,j);
  }

  return subA2;
}

/// Select rows for branches \a b, and toss columns where the last branch has entry -1
matrix<int> subA_index_t::get_subA_index_select(const vector<int>& b) const
{
  // the alignment of sub alignments
  matrix<int> subA = get_subA_index(b);

  // return processed indices
  return subA_select(subA);
}


/// Select rows for branches \a b, and toss columns where the last branch has entry -1
matrix<int> subA_index_t::get_subA_index_vanishing(const vector<int>& b) 
{
  // the alignment of sub alignments
  matrix<int> subA = get_subA_index(b);

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
matrix<int> subA_index_t::get_subA_index_any(const vector<int>& b, const vector<int>& nodes) 
{
  const alignment& AA = A();
  
  // the alignment of sub alignments
  matrix<int> subA = get_subA_index(b,true);

  // select and order the columns we want to keep
  const int B = b.size();
  for(int i=0,l=0;i<subA.size1();i++)
  {
    int c = subA(i,B);
    if (any_present(AA,c,nodes))
      subA(i,B) = l++;
    else
      subA(i,B) = -1;
  }

  // return processed indices
  return subA_select(subA);
}

/// Select rows for branches \a b, but exclude columns in which nodes \a nodes are present.
matrix<int> subA_index_t::get_subA_index_none(const vector<int>& b, const vector<int>& nodes) 
{
  const alignment& AA = A();
  
  // the alignment of sub alignments
  matrix<int> subA = get_subA_index(b,true);

  // select and order the columns we want to keep
  const int B = b.size();
  for(int i=0,l=0;i<subA.size1();i++)
  {
    int c = subA(i,b.size());
    if (not any_present(AA,c,nodes))
      subA(i,B) = l++;
    else
      subA(i,B) = -1;
  }

  // return processed indices
  return subA_select(subA);
}

/// Select rows for branches \a b and columns present at nodes, but ordered according to the list of columns \a seq
matrix<int> subA_index_t::get_subA_index_columns(const vector<int>& b, const vector<int>& index_to_columns) 
{
  // select and order the columns we want to keep
  const int B = b.size();

  // Create the sorted column order
  vector<int> order = iota((int)index_to_columns.size());
  std::sort(order.begin(), order.end(), sequence_order<int>(index_to_columns));
  vector<pair<int,int> > columns(index_to_columns.size());
  for(int i=0;i<columns.size();i++)
  {
    columns[i].first = index_to_columns[order[i]];
    columns[i].second = order[i];
  }

  // The alignment of non-empty columns in b
  matrix<int> subA1 = get_subA_index(b,true);

  // The alignment of indices from branches \a b from columns in the order 
  matrix<int> subA2(columns.size(), B);
  for(int i=0;i<subA2.size1();i++)
    for(int j=0;j<subA2.size2();j++)
      subA2(i,j) = -2;

  // Fill in the entries of the columns in sorted order
  for(int i=0,k=0;i<columns.size();i++)
  {
    int column = columns[i].first;
    int index = columns[i].second;
    
    // Skip unsed columns in subA1.
    while (k<subA1.size1() and subA1(k, B) < column)
      k++;

    if (k<subA1.size1() and column == subA1(k, B))
      for(int j=0;j<B;j++)
	subA2(index,j) = subA1(k,j);
    else
      for(int j=0;j<B;j++)
	subA2(index,j) = -1;
  }

  // The alignment of indices from branches \a b from columns in the order 
  for(int i=0;i<subA2.size1();i++)
    for(int j=0;j<subA2.size2();j++)
      assert( subA2(i,j) != -2 );

  return subA2;
}


std::ostream& print_subA(std::ostream& o,const matrix<int>& I)
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
void check_subA(const subA_index_t& I1_, const alignment& A1,const subA_index_t& I2_, const alignment& A2, const TreeInterface& t) 
{
  for(int b=t.n_leaves();b<2*t.n_branches();b++) 
  {
    // compute branches-in
    vector<int> b2 = t.branches_before(b);
    b2.push_back(b);
	
    matrix<int> I1 = I1_.get_subA_index_select(b2);
    matrix<int> I2 = I2_.get_subA_index_select(b2);

    if (not subA_identical(I1,I2)) 
    {
      // print subAs alignments
      print_subA(std::cerr,I1)<<std::endl;
      std::cerr<<std::endl;
      print_subA(std::cerr,I2)<<std::endl;

      // print leaf sets in each subA
      for(int k=0;k<b2.size();k++) {
	std::cerr<<"leaf set #"<<k+1<<" = ";
	int lb = t.reverse(b2[k]);
	for(int l=0;l<t.n_leaves();l++)
	  if (t.partition(lb)[l])
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

vector<int> subA_index_t::characters_to_indices(int branch)
{
  int node = T().source(branch);

  vector<int> suba_for_character(A().seqlength(node), -1);

  // walk the alignment row and the subA-index row simultaneously
  vector<int> columns = A().get_columns_for_characters(node);

  const auto& index = row(branch);

  for(int i=0,j=0,k=0;j<columns.size();j++)
  {
    int c = columns[j];

    while(i<index.size() and index[i].first < c)
      i++;

    assert(i != -1);

    suba_for_character[k++] = index[i].second;
  }

  return suba_for_character;
}

subA_index_t::subA_index_t(const data_partition* dp, subA_index_kind k, const vector<int>& r)
  :DP(dp),
   kind_(k),
   row_indices(r)
{ }

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
Vector<pair<int,int> > combine_columns(const vector<pair<int,int> >& p1, const vector<pair<int,int> >& p2)
{
  const int s1 = p1.size();
  const int s2 = p2.size();

  Vector<pair<int,int> > p3;
  p3.reserve(s1+s2);

  int I1 = 0;
  int I2 = 0;
  while(I1 < s1 or I2 < s2)
  {
    int c = -1;
    if (I2 >= s2)
    {
      c = p1[I1].first;
      I1++;
    }
    else if (I1 >= s1)
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

subA_index_leaf::subA_index_leaf(const data_partition* dp, const vector<int>& r)
  :subA_index_t(dp, subA_index_t::leaf_index, r)
{
}

subA_index_internal::subA_index_internal(const data_partition* dp, const vector<int>& r)
  :subA_index_t(dp, subA_index_t::internal_index, r)
{
}
