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

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>

#include "2way.H"
#include "alignment/alignment-util.H"
#include "util/assert.hh"

using namespace std;
using boost::dynamic_bitset;

void pairwise_alignment_t::flip()
{
    for(int i=0;i<size();i++)
	states_[i] = A2::flip(states_[i]);
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
	if (is_match(i) or is_delete(i))
	    total++;
    return total;
}

int pairwise_alignment_t::length2() const
{
    int total = 0;
    for(int i=0;i<size();i++)
	if (is_match(i) or is_insert(i))
	    total++;
    return total;
}

int pairwise_alignment_t::count(int s) const
{
    int total = 0;
    for(int i=0;i<size();i++)
	if (get_state(i) == s)
	    total++;
    return total;
}

pairwise_alignment_t::pairwise_alignment_t()
{ }

pairwise_alignment_t::pairwise_alignment_t(const vector<int>& pi)
{
    resize(pi.size());
    for(int i=0;i<pi.size();i++)
	set_state(i,pi[i]);
}

bool operator==(const pairwise_alignment_t& pi1, const pairwise_alignment_t& pi2)
{
    if (pi1.size() != pi2.size()) return false;

    for(int i=0;i<pi1.size();i++)
	if (pi1.get_state(i) != pi2.get_state(i)) return false;

    return true;
}

bitmask_8 convert_to_bits(int s, int b1, int b2)
{
    bitmask_8 bits;
    if (s == A2::states::M)
    {
	bits.set(b1);
	bits.set(b2);
    }
    else if (s == A2::states::G1)
	bits.set(b2);
    else if (s == A2::states::G2)
	bits.set(b1);

    return bits;
}

vector<bitmask_8> convert_to_bits(const pairwise_alignment_t& A, int b1, int b2)
{
    vector<bitmask_8> a;
    a.reserve(A.size());

    for(int i=0;i<A.size();i++)
    {
	int s = A.get_state(i);
	auto bits = convert_to_bits(s, b1, b2);
	if (bits.any())
	    a.push_back(bits);
    }
    return a;
}

using namespace boost;

// bidirectionalS means we store both In and Out edges.
// directionalS ... means we store only Out edges?
typedef adjacency_list< vecS, vecS, bidirectionalS, property<vertex_color_t, default_color_type> > Graph; 
typedef graph_traits<Graph>::vertex_descriptor Vertex;

#include <map>
using std::map;

namespace A2
{

    pairwise_alignment_t get_pairwise_alignment(const matrix<int>& M, int n1, int n2)
    {
	pairwise_alignment_t pi;
	pi.reserve(M.size1());
	for(int c=0;c<M.size1();c++)
	{
	    bool d1 = (M(c,n1) >=0 or M(c,n1) == -2);
	    bool d2 = (M(c,n2) >=0 or M(c,n2) == -2);
	    pi.push_back(d1,d2);
	}

	return pi;
    }

    pairwise_alignment_t get_pairwise_alignment(const alignment& A, int n1, int n2)
    {
	pairwise_alignment_t pi;
	pi.reserve(A.length());
	for(int c=0;c<A.length();c++)
	{
	    bool d1 = A.character(c,n1);
	    bool d2 = A.character(c,n2);
	    pi.push_back(d1,d2);
	}

	return pi;
    }

    pairwise_alignment_t get_pairwise_alignment_from_path(const vector<int>& path)
    {
	// Path should end in an E state.
	assert(path.size() > 0 and path.back() == A2::states::E);

	pairwise_alignment_t pi;
	int L = path.size()-1;
	assert(L >= 0);
	pi.reserve(L);
	for(int c=0;c<L;c++)
	    pi.push_back(path[c]);
	return pi;
    }

    vector<int> get_path_from_pairwise_alignment(const pairwise_alignment_t& A)
    {
	vector<int> path;
	path.reserve(A.size()+1);

	// Add the non-silent states
	for(int i=0;i<A.size();i++)
	    path.push_back(A.get_state(i));

	// Add the E at the end of the path.
	path.push_back(A2::states::E);

	return path;
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
	if (pi.is_match(i))
	{
	    column = get_column(node1, index1++);
	    add_row_character_to_column(node2, column);
	}
	else if (pi.is_insert(i))
	{
	    column = add_row_character_to_new_column(node2);
	}
	else if (pi.is_delete(i))
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

// this code currently relies on start and end states...

struct Construct
{
    int c=0;
    vector<pairwise_alignment_t> A;
    vector<int> pos;
    vector<int> L;
    vector<vector<int>> children;
    matrix<int> M;
    int write_insertions(int n0);
    void write_match(int n0);
    Construct(const vector<pairwise_alignment_t>& a, const TreeInterface& t)
	:A(t.n_nodes()),
	 pos(t.n_nodes(),0),
	 L(t.n_nodes(),0),
	 children(t.n_nodes())
	{ 
	    using namespace A2;

	    assert(t.n_nodes() > 1);

	    // 1. Record child branches for each branch
	    vector<int> branches = t.all_branches_from_node(0);

	    for(int b: branches)
	    {
		int target = t.target(b);

		A[target] = a[b];

		children[target] = t.branches_after(b);
		for(int& x: children[target])
		    x = t.target(x);
	    }

	    // 2. Compute total number of alignment columns
	    int b0 = branches[0];
	    int source0 = t.source(b0);
	    int target0 = t.target(b0);
	    int AL = a[b0].length1();
	    for(const auto& b: branches)
		AL += a[b].count(states::G1);

	    // 3. Initialize index matrix with all -1
	    M.resize(AL, t.n_nodes(), -1);

	    // Iterate through columns of A[b0]
	    while (true)
	    {
		// Emit insertions that come before the current column, and the current state
		int S = write_insertions(target0);
      
		// Read the child until it reads from the parent -- that is, finds a non-insert state
		write_match(target0);
      
		if (S == states::E) break;
      
		// If this state emits a character at node0, then write it out in column c
		assert(S == states::M or S == states::G2);
		M(c, source0) = L[source0]++;
		c++;
	    }
	    assert(c == AL);

#ifndef NDEBUG
	    // 5. Check that the resulting matrix yields the correct pairwise alignments
	    for(int b=0;b<2*t.n_branches();b++)
	    {
		pairwise_alignment_t a2 = A2::get_pairwise_alignment(M, t.source(b), t.target(b));
		assert(a[b] == a2);
	    }
#endif
	}
};



// Read a state and optionally pass it down to our children.
void Construct::write_match(int node0)
{
    using namespace A2;

    // Read something,
    int S = states::E;
    if (pos[node0] < A[node0].size())
    {
	S = A[node0].get_state(pos[node0]);
	pos[node0]++;
    }
    assert(S == states::M or S == states::G2 or S == states::E);

    // Call down to children with a down-tick
    if (S == states::M or S == states::E)
    {
	for(const auto& n: children[node0])
	    write_match(n);
    }

    // If we read a match, write a character, and make our children read something
    if (S == states::M)
	M(c, node0) = L[node0]++;
}

// Write out the columns of this pairwise alignment until the first in-tick (M, I, or E)
// But first write out the columns of all insertions in children that occur before this column.
int Construct::write_insertions(int node0)
{
    using namespace A2;

    while (true)
    {
	// Add child insertions that come before this column
	for(const auto& n: children[node0])
	    write_insertions(n);

	// Find the next state
	if (pos[node0] >= A[node0].size()) return states::E;

	int S = A[node0].get_state(pos[node0]);

	if (S != states::G1) return S;

	// Emit the current column
	for(const auto& n: children[node0])
	    write_match(n);
	M(c, node0) = L[node0]++;
	c++;
	pos[node0]++;
    };
}

matrix<int> construct(const TreeInterface& t, const vector<pairwise_alignment_t>& A)
{
    using namespace A2;

    Construct C(A,t);

    return C.M;
}

const EVector& childAlignments(const expression_ref& a)
{
    return a.sub()[2].as_<EVector>();
}

void getNodes(const expression_ref& a, set<int>& nodes)
{
    nodes.insert(a.sub()[0].as_int());

    for(auto& childAlignment: childAlignments(a))
        getNodes(childAlignment, nodes);
}

set<int> getNodes(const expression_ref& a)
{
    set<int> nodes;

    getNodes(a, nodes);

    return nodes;
}

int getLength(const expression_ref& a)
{
    int L = a.sub()[1].as_<Box<pairwise_alignment_t>>().count_insert();
    for(auto& childAlignment: childAlignments(a))
        L += getLength(childAlignment);
    return L;
}

void getBranches(int source, const expression_ref& a, set<std::tuple<int,int>>& branches)
{
    int node = a.sub()[0].as_int();
    branches.insert({source, node});

    for(auto& childAlignment: childAlignments(a))
        getBranches(node, childAlignment, branches);
}

set<std::tuple<int,int>> getBranches(const expression_ref& a)
{
    set<std::tuple<int,int>> branches;

    int node = a.sub()[0].as_int();
    for(auto& childAlignment: childAlignments(a))
        getBranches(node, childAlignment, branches);

    return branches;
}

typedef std::map<int,object_ptr<EVector>> AEVectors;

struct BranchAlignment
{
    int node;
    const pairwise_alignment_t& pairwise_alignment;
    std::vector<std::shared_ptr<BranchAlignment>> children;

    int pos = 0; // Not used at top level -- goes with pairwise alignment.
    int L = 0;
    EVector& A;
    BranchAlignment(const expression_ref& a, AEVectors& AA)
        :node(a.sub()[0].as_int()),
         pairwise_alignment(a.sub()[1].as_<Box<pairwise_alignment_t>>()),
         A(*AA.at(node))
    {
        for(auto& c: a.sub()[2].as_<EVector>())
            children.push_back(std::make_shared<BranchAlignment>(c, AA));
    }
};

int write_insertions(int& c, BranchAlignment& n0);
void write_match(int& c, BranchAlignment& n0);

// Read a state and optionally pass it down to our children.
void write_match(int& c, BranchAlignment& a)
{
    using namespace A2;

    auto& pa = a.pairwise_alignment;
    auto& pos = a.pos;

    // Read something,
    int S = states::E;

    if (pos < pa.size())
    {
	S = pa.get_state(pos);
        pos++;
    }
    assert(S == states::M or S == states::G2 or S == states::E);
  
    // Call down to children with a down-tick
    if (S == states::M or S == states::E)
    {
	for(const auto& child: a.children)
	    write_match(c, *child);
    }

    // If we read a match, write a character, and make our children read something
    if (S == states::M)
	a.A[c] = a.L++;
}

// Write out the columns of this pairwise alignment until the first in-tick (M, I, or E)
// But first write out the columns of all insertions in children that occur before this column.
int write_insertions(int& c, BranchAlignment& a)
{
    using namespace A2;

    auto& pa = a.pairwise_alignment;

    auto& pos = a.pos;

    while (true)
    {
	// Add child insertions that come before this column
	for(const auto& child: a.children)
	    write_insertions(c, *child);

	// Find the next state
	if (pos >= pa.size()) return states::E;

	int S = pa.get_state(pos);

	if (S != states::G1) return S;

	// Emit the current column
	for(const auto& child: a.children)
	    write_match(c, *child);

	a.A[c] = a.L++;
	c++;
	pos++;
    };
}

typedef Box<std::map<int,expression_ref>> EIntMap;

object_ptr<EIntMap> construct2(const expression_ref& a)
{
    // 1. Extract the node names
    auto nodes = getNodes(a);
    assert(nodes.size() > 1);

    // 2. Find the total alignment length
    int AL = getLength(a);

    // 3. Create the EVectors for the alignment rows
    AEVectors A;
    for(auto& node: nodes)
        A.insert({node, object_ptr<EVector>(new EVector(AL, -1))});

    // 4. Create a tree-alignment structure with modifiable state.
    BranchAlignment na(a, A);

    using namespace A2;

    // 5. Fill in the non-gap elements
    int c = 0;
    write_insertions(c, na);
    assert(c == AL);

    // 6. Check that the resulting rows yield the correct pairwise alignments
#ifndef NDEBUG
    /*
    for(auto [source, target]: getBranches(a))
    {
        pairwise_alignment_t a2 = A2::get_pairwise_alignment(M.at(source), M.at(target));
        assert(a[b] == a2);
    }
    */
#endif

    // 7. Construct the immutable EIntMap from the mutable AEVectors.
    object_ptr<EIntMap> A2(new EIntMap);
    for(auto& [node, row]: A)
        A2->insert({node,row});

    return A2;
}


alignment get_alignment(const alignment& A1, const vector< vector<int>>& sequences, const matrix<int>& M)
{
    assert(A1.n_sequences() == M.size2());

    return get_alignment(A1.get_alphabet(), A1.seqs(), sequences, M);
}

alignment get_alignment(const alphabet& a, const vector<sequence>& seqs, const vector< vector<int>>& sequences, const matrix<int>& M)
{
    alignment A2(a,seqs, M.size1());

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

pairwise_alignment_t get_pairwise_alignment_from_bits(const std::vector<HMM::bitmask_t>& bit_path, int i, int j)
{
  assert(i != j);

  pairwise_alignment_t pi;
  pi.reserve(bit_path.size());

  for(const auto& bits: bit_path)
    pi.push_back(bits.test(i), bits.test(j));
  
  return pi;
}

pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path, const HMM& H, int n1, int n2)
{
  assert(H.all_bits().test(n1));
  assert(H.all_bits().test(n2));

  return get_pairwise_alignment_from_bits(get_bits_from_path(path, H), n1, n2);
}

int n_indels(const pairwise_alignment_t& a)
{
    using namespace A2;

    if (a.size() < 1) return 0;

    int total = (a.is_insert(0) or a.is_delete(0))?1:0;

    for(int i=1;i<a.size();i++)
	if (a.is_insert(i) and not a.is_insert(i-1))
	    total++;
	else if (a.is_delete(i) and not a.is_delete(i-1))
	    total++;

    return total;
}

int total_length_indels(const pairwise_alignment_t& a)
{
    return a.count_insert() + a.count_delete();
}

int pairwise_alignment_t::count_indels() const
{
    using namespace A2;

    int n_indels = 0;

    int last_state = states::M;
    for(int current_state: states_)
    {
        if (current_state != last_state and ((current_state == states::G1) or (current_state == states::G2)))
            n_indels++;
        last_state = current_state;
    }

    return n_indels;
}

pairwise_alignment_t make_unaligned_pairwise_alignment(int L1, int L2)
{
    pairwise_alignment_t pi;
    pi.resize(L1+L2);
    for(int i=0;i<L1;i++)
	pi.set_delete(i);
    for(int i=0;i<L2;i++)
	pi.set_insert(L1+i);
    return pi;
}

pairwise_alignment_t make_left_aligned_pairwise_alignment(int L1, int L2)
{
    pairwise_alignment_t pi;
    pi.resize(std::max(L1,L2));
    int M = min(L1,L2);
    for(int i=0; i<M; i++)
	pi.set_match(i);
    for(int i=M; i<L1; i++)
	pi.set_delete(i);
    for(int i=M; i<L2; i++)
	pi.set_insert(i);

    assert(pi.length1() == L1);
    assert(pi.length2() == L2);

    return pi;
}
