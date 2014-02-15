/*
   Copyright (C) 2004-2005,2009 Benjamin Redelings

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

#include "2way.H"
#include "alignment/alignment-util.H"
#include "substitution/substitution-index.H"

using namespace std;
using boost::dynamic_bitset;

void pairwise_alignment_t::flip()
{
  for(int i=0;i<size();i++)
    (*this)[i] = A2::flip((*this)[i]);
}

pairwise_alignment_t pairwise_alignment_t::flipped() const
{
  pairwise_alignment_t pi (*this);
  pi.flip();
  return pi;
}

int pairwise_alignment_t::length1() const
{
  int total = 0;
  for(int i=0;i<size();i++)
    if ((*this)[i] == A2::states::M or (*this)[i] == A2::states::G2)
      total++;
  return total;
}

int pairwise_alignment_t::length2() const
{
  int total = 0;
  for(int i=0;i<size();i++)
    if ((*this)[i] == A2::states::M or (*this)[i] == A2::states::G1)
      total++;
  return total;
}

int pairwise_alignment_t::count(int s) const
{
  int total = 0;
  for(int i=0;i<size();i++)
    if ((*this)[i] == s)
      total++;
  return total;
}

pairwise_alignment_t::pairwise_alignment_t()
{ }

pairwise_alignment_t::pairwise_alignment_t(const vector<int>& pi)
  :vector<int>(pi)
{ }

pairwise_alignment_t::pairwise_alignment_t(const pairwise_alignment_t& pi)
  :vector<int>(pi)
{ }

bool operator==(const pairwise_alignment_t& pi1, const pairwise_alignment_t& pi2)
{
  if (pi1.size() != pi2.size()) return false;

  for(int i=0;i<pi1.size();i++)
    if (pi1[i] != pi2[i]) return false;

  return true;
}

vector<bitset<64>> convert_to_bits(const pairwise_alignment_t& A, int b1, int b2)
{
  vector<bitset<64>> a;
  a.reserve(A.size());

  for(int s:A)
  {
    bitset<64> bits;
    if (s == A2::states::M)
    {
      bits.set(b1);
      bits.set(b2);
    }
    else if (s == A2::states::G1)
      bits.set(b2);
    else if (s == A2::states::G2)
      bits.set(b1);

    if (bits.any())
      a.push_back(bits);
  }
  return a;
}

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>

using namespace boost;

// bidirectionalS means we store both In and Out edges.
// directionalS ... means we store only Out edges?
typedef adjacency_list< vecS, vecS, bidirectionalS, property<vertex_color_t, default_color_type> > Graph; 
typedef graph_traits<Graph>::vertex_descriptor Vertex;

#include <map>
using std::map;

namespace A2 {


  int flip(int s)
  {
    if (s == states::G1)
      return states::G2;
    else if (s == states::G2) 
      return states::G1;
    else return s;
  }

  pairwise_alignment_t get_pairwise_alignment(const ublas::matrix<int>& M, int n1, int n2)
  {
    pairwise_alignment_t pi;
    pi.reserve(M.size1()+2);
    pi.push_back(A2::states::S);
    for(int c=0;c<M.size1();c++)
    {
      int S = -1;
      if (M(c,n1) >=0 or M(c,n1) == -2)
      {
	if (M(c,n2) >=0 or M(c,n2) == -2)
	  S = A2::states::M;
	else
	  S = A2::states::G2;
      }
      else
      {
	if (M(c,n2) >=0 or M(c,n2) == -2)
	  S = A2::states::G1;
      }
      if (S != -1)
	pi.push_back(S);
    }
    pi.push_back(A2::states::E);

    return pi;
  }

  pairwise_alignment_t get_pairwise_alignment(const alignment& A, int n1, int n2)
  {
    pairwise_alignment_t pi;
    pi.reserve(A.length()+2);
    pi.push_back(A2::states::S);
    for(int c=0;c<A.length();c++)
    {
      int S = -1;
      if (A.character(c,n1))
      {
	if (A.character(c,n2))
	  S = A2::states::M;
	else
	  S = A2::states::G2;
      }
      else
      {
	if (A.character(c,n2))
	  S = A2::states::G1;
      }
      if (S != -1)
	pi.push_back(S);
    }
    pi.push_back(A2::states::E);

    return pi;
  }

  pairwise_alignment_t get_pairwise_alignment_from_path(const vector<int>& path)
  {
    pairwise_alignment_t pi;
    pi.reserve(path.size() + 2);
    pi.push_back(A2::states::S);
    for(int c=0;c<path.size();c++)
      pi.push_back(path[c]);
    pi.push_back(A2::states::E);

    return pi;
  }

}

class graph_alignment
{
  Graph g;

  vector<Vertex> vertices;

  /// 
  vector< vector<int> > column_index;

  Vertex get_vertex(int i) const {return vertices[i];}

  int add_vertex();

public:
  int add_row_character_to_new_column(int row);

  void add_edge(int i, int j);

  void add_row_character_to_column(int row, int column);

  void add_pairwise_alignment(int node1, int node2, const pairwise_alignment_t& pi);

  int get_column(int row, int index) const {return column_index[row][index];}

  int seq_length(int row) const {return column_index[row].size();}

  int n_rows() const {return column_index.size();}

  void link_all_columns();

  vector<int> sort();

  int length() const {return vertices.size();}

  graph_alignment(int r)
    :column_index(r)
  { }
};

int graph_alignment::add_vertex()
{
  vertices.push_back(::add_vertex(g));
  return vertices.size()-1;
}

int graph_alignment::add_row_character_to_new_column(int row)
{
  int new_column = add_vertex();

  add_row_character_to_column(row, new_column);

  return new_column;
}

void graph_alignment::add_edge(int i, int j)
{
  // This should add (possibly duplicate) edges.
  ::add_edge(get_vertex(i), get_vertex(j), g);
}

void graph_alignment::add_row_character_to_column(int row, int column)
{
  int index = column_index[row].size();
  column_index[row].push_back(column);
  assert( get_column(row,index) == column );
}

void graph_alignment::add_pairwise_alignment(int node1, int node2, const pairwise_alignment_t& pi)
{
  using namespace A2;

  // What is the next character to match in the sequence with index 'node1'?
  int index1 = 0;

  int prev_column = -1;
  for(int i=0;i<pi.size();i++)
  {
    int column = -1;
    if (pi[i] == states::M)
    {
      column = get_column(node1, index1++);
      add_row_character_to_column(node2, column);
    }
    else if (pi[i] == states::G1)
    {
      column = add_row_character_to_new_column(node2);
    }
    else if (pi[i] == states::G2)
    {
      column = get_column(node1, index1++);
    }

    // Add edge from previous column to this column
    if (prev_column != -1 and column != -1)
      add_edge(prev_column,column);

    prev_column = column;
  }
  assert(index1 == seq_length(node1));
}

void graph_alignment::link_all_columns()
{
  for(int r=0;r<n_rows();r++)
    for(int j=1;j<seq_length(r);j++)
      add_edge(get_column(r,j-1), get_column(r,j));
}

vector<int> graph_alignment::sort()
{
  boost::property_map<Graph, vertex_index_t>::type id = get(vertex_index, g);

  vector<Vertex> c;
  topological_sort(g, std::back_inserter(c));

  vector<int> order;
  for(int i=c.size()-1;i>=0;i--)
    order.push_back( id[c[i]] );

  return order;
}

// Read a state and optionally pass it down to our children.
void write_match(int b0, int& c, vector<int>& pos, vector<int>& L,
		 const vector<pairwise_alignment_t>& A, 
		 vector<vector<const_branchview>>& children, ublas::matrix<int>& M, const Tree& T)
{
  using namespace A2;

  int node0 = T.directed_branch(b0).target();

  // Read something,
  int S = A[b0][pos[b0]];
  pos[b0]++;
  assert(S == states::M or S == states::G2 or S == states::E);
  
  // Call down to children with a down-tick
  if (S == states::M or S == states::E)
  {
    for(const auto& b: children[b0])
      write_match(b, c, pos, L, A, children, M, T);
  }

  // If we read a match, write a character, and make our children read something
  if (S == states::M)
    M(c, node0) = L[node0]++;
}

// Write out the columns of this pairwise alignment until the first in-tick (M, I, or E)
// But first write out the columns of all insertions in children that occur before this column.
int write_insertions(int b0, int& c, vector<int>& pos, vector<int>& L,
		      const vector<pairwise_alignment_t>& A, 
		      vector<vector<const_branchview>>& children, ublas::matrix<int>& M, const Tree& T)
{
  using namespace A2;

  int node0 = T.directed_branch(b0).target();

  while (true)
  {
    // Find the next state
    int S = A[b0][pos[b0]];

    // Add child insertions that come before this column
    for(const auto& b: children[b0])
      write_insertions(b, c, pos, L, A, children, M, T);

    if (S != states::G1) return S;

    // Emit the current column
    for(const auto& b: children[b0])
      write_match(b, c, pos, L, A, children, M, T);
    M(c, node0) = L[node0]++;
    c++;
    pos[b0]++;
  };
}

ublas::matrix<int> construct2(const Tree& T, const vector<pairwise_alignment_t>& A)
{
  using namespace A2;

  // 1. Record child branches for each branch
  vector<const_branchview> branches = branches_from_node(T, 0);

  vector<vector<const_branchview>> children(2*T.n_branches());
  for(int b=0;b<2*T.n_branches();b++)
    append(T.directed_branch(b).branches_after(),children[b]);

  // 2. Compute total number of alignment columns
  int b0 = branches[0];
  int node0 = T.directed_branch(b0).source();
  int AL = A[b0].length1();
  for(const auto& b: branches)
    AL += A[b].count(states::G1);

  // 3. Initialize index matrix with all -1
  ublas::matrix<int> M(AL, T.n_nodes(), -1);

  // 4. Walk through each column in pairwise alignment A[b0]
  vector<int> pos(2*branches.size(),1);   // Skip the start state
  vector<int> L(T.n_nodes(), 0);

  // Iterate through columns of A[b0]
  int c = 0;
  while (true)
  {
    // Emit insertions that come before the current column, and the current state
    int S = write_insertions(b0, c, pos, L, A, children, M, T);

    // Read the child until it reads from the parent -- that is, finds a non-insert state
    write_match(b0, c, pos, L, A, children, M, T);

    if (S == states::E) break;

    // If this state emits a character at node0, then write it out in column c
    assert(S == states::M or S == states::G2);
    M(c, node0) = L[node0]++;
    c++;
  }
  assert(c == AL);

#ifndef NDEBUG
  // 5. Check that the resulting matrix yields the correct pairwise alignments
  for(int b=0;b<2*T.n_branches();b++)
  {
    pairwise_alignment_t a = A2::get_pairwise_alignment(M, T.directed_branch(b).source(), T.directed_branch(b).target());
    assert(A[b] == a);
  }
#endif

  return M;
}

// http://www.boost.org/doc/libs/1_49_0/libs/graph/doc/adjacency_list.html

ublas::matrix<int> construct(const Tree& T, const vector<pairwise_alignment_t>& A)
{
  graph_alignment a(T.n_nodes());

  // Add the first sequence
  {
    int b0 = (*T.node(0).branches_out()).name();
    int L0 = A[b0].length1();
    for(int i=0;i<L0;i++)
      a.add_row_character_to_new_column(0);
  }

  vector<const_branchview> branches = branches_from_node(T, 0);
  // FIXME - we shouldn't have to reverse this!
  std::reverse(branches.begin(), branches.end());
  for(int i=0;i<branches.size();i++)
  {
    int b = branches[i];

    a.add_pairwise_alignment(branches[i].source(), branches[i].target(), A[b]);
  }

  a.link_all_columns();

  // maps rank -> column_name
  vector<int> order = a.sort();
  // maps column_name -> rank
  order = invert(order);

  // Add the columns for sequence 0
  ublas::matrix<int> M(a.length(), a.n_rows());
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M(i,j) = -1;

  for(int r=0;r<a.n_rows();r++)
    for(int i=0;i<a.seq_length(r);i++)
    {
      // find the column name
      int column = a.get_column(r,i);
      // find the rank of this column name
      column = order[column];
      // set the corresponding value.
      M(column,r) = i;
    }

#ifndef NDEBUG
  for(int b=0;b<2*T.n_branches();b++)
  {
    pairwise_alignment_t a = A2::get_pairwise_alignment(M, T.directed_branch(b).source(), T.directed_branch(b).target());
    assert(A[b] == a);
  }
#endif

  return M;
}

alignment get_alignment(const alignment& A1, const vector< vector<int>>& sequences, const ublas::matrix<int>& M)
{
  assert(A1.n_sequences() == M.size2());

  alignment A2 = A1;
  A2.changelength(M.size1());

  // Overwrite the values in A2
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      A2.set_value(i,j,M(i,j));

  // Set the leaf sequences
  for(int i=0;i<sequences.size();i++)
    for(int c=0;c<A2.length();c++)
      if (A2(c,i) >= 0)
	A2.set_value(c, i, sequences[i][A2(c, i)]);

  // Set the internal node sequences
  for(int i=sequences.size();i<A2.n_sequences();i++)
    for(int c=0;c<A2.length();c++)
      if (A2(c,i) >= 0)
	A2.set_value(c, i, alphabet::not_gap);

  return A2;
}
