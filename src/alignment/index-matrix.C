/*
   Copyright (C) 2007-2009 Benjamin Redelings

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

#include "index-matrix.H"
#include "alignment/alignment-util.H"
#include "util.H"

using namespace std;

Edges::Edges(const vector<int>& L)
  :lookup(L.size())
{
  for(int i=0;i<L.size();i++)
    lookup[i].resize(L.size());

  for(int i=0;i<L.size();i++)
    for(int j=0;j<L.size();j++) {
      lookup[i][j].clear();
      lookup[i][j].resize(L[i],end());
    }
}

void Edges::build_index() 
{
  foreach(e,*this) 
  {
    if (e->x1 >= 0)
      lookup[e->s1][e->s2][e->x1] = e;

    if (e->x2 >= 0)
      lookup[e->s2][e->s1][e->x2] = e;
  }
}

double Edges::PP(int s1, int x1, int s2, int x2) const 
{
  assert(x1 >= 0);

  if (lookup[s1][s2][x1] == end())
    return 0;

  const Edge& e = *lookup[s1][s2][x1];
  if (e.s1 != s1) {
    std::swap(s1,s2);
    std::swap(x1,x2);
    assert(e.s1 == s1);
  }

  if (e.x2 == x2)
    return e.p;
  else
    return 0;
}

int Edges::index_in_sequence(int s1,int x1,int s2) const 
{
  assert(x1 >= 0);

  if (lookup[s1][s2][x1] == end())
    return -3;

  const Edge& e = *lookup[s1][s2][x1];
  if (e.s1 == s1) {
    return e.x2;
  }
  else {
    assert(e.s1 == s2);
    return e.x1;
  }
}


void add_edges(Edges& E, const vector< matrix<int> >& Ms,
	       int s1,int s2,int L1, int L2,double cutoff) 
{ 
  matrix<int> count(L1+1,L2+1);
  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++)
      count(i,j) = 0;

  // get counts of each against each
  for(int i=0;i<Ms.size();i++) {
    const matrix<int>& M = Ms[i];

    for(int c=0;c<M.size1();c++) {
      int index1 = M(c,s1);
      int index2 = M(c,s2);
      if (index1 != -3 and index2 != -3)
	count(index1 + 1, index2 + 1)++;
    }
  }
  count(0,0) = 0;


  // determine Ml pairs
  for(int i=0;i<count.size1();i++) 
    for(int j=0;j<count.size2();j++) 
    {
      double Pr = double(count(i,j))/Ms.size();

      if (Pr > cutoff) {
	Edge e;
	e.s1 = s1;
	e.x1 = i-1;

	e.s2 = s2;
	e.x2 = j-1;

	e.count = count(i,j);
	e.p  = Pr;

	E.insert(e);
      }
    }
}

index_matrix unaligned_matrix(const vector<int>& L) 
{
  index_matrix M(sum(L),L);
  
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M(i,j) = -3;
  
  int c=0;
  for(int i=0;i<M.size2();i++) {
    for(int j=0;j<M.length(i);j++,c++) {
      M.column(i,j) = c;
      M(c,i) = j;
    }
  }

  M.unknowns = M.size1()*(M.size2()-1);

  //  if (M.unknowns != M.n_unknown()) {std::cerr<<"A";abort();}
  //  if (M.columns != M.n_columns()) {std::cerr<<"B";abort();}

  return M;
}

index_matrix::index_matrix(const alignment& A)
  :matrix<int>(M(A)),
   column_index(A.n_sequences()),
   columns(A.length()),
   unknowns(0)
{
  vector<int> L(A.n_sequences());

  for(int i=0;i<L.size();i++) {
    column_index[i].resize(A.seqlength(i));
    for(int j=0;j<L[i];j++)
      column(i,j) = -1;
  }

  for(int c=0;c<size1();c++) {
    for(int s=0;s<size2();s++) {
      int index = (*this)(c,s);
      if (index >= 0)
	column(s,index) = c;
    }
  }

  for(int i=0;i<L.size();i++)
    for(int j=0;j<L[i];j++)
      assert(column(i,j) >= 0);
}

bool index_matrix::columns_conflict(int c1, int c2) const
{
  for(int i=0;i<size2();i++) {

    // if either value is 'unknown', then we can't conflict
    if (index(c1,i) == -3 or index(c2,i) == -3)
      continue;

    // two gaps can be merged
    if (index(c1,i) == -1 and index(c2,i) == -1)
      continue;

    // two letters, or a letter and a gap
    return true;
  }

  return false;
}

bool index_matrix::consistent(int c, int s2,int x2, const Edges& E,double cutoff) const 
{
  bool ok = true;
  for(int s1=0;s1<size2() and true;s1++) {
    int x1 = index(c,s1);
    if (x1 < 0) continue;

    if (E.PP(s1,x1,s2,x2) < cutoff)
      ok = false;
  }
  return ok;
}

bool index_matrix::consistent(int c1, int c2,const Edges& E, double cutoff) const 
{
  for(int s2=0;s2<size2() and true;s2++) {
    int x2 = index(c2,s2);
    if (x2 == -3) continue;

    if (not consistent(c1,s2,x2,E,cutoff)) return false;
  }
  return true;
}

unsigned count_unknowns(const index_matrix& M,int c)
{
  unsigned total = 0;
  for(int i=0;i<M.size2();i++)
    if (M(c,i) == alphabet::unknown)
      total++;
  return total;
}

void index_matrix::merge_columns(int c1, int c2) 
{
  //  if (n_unknown() != unknowns)
  //    {cerr<<"D";abort();}

  int before = count_unknowns(*this,c1)+count_unknowns(*this,c2);

  if (c1 > c2) std::swap(c1,c2);

  for(int i=0;i<size2();i++) 
  {
    // don't need to move an 'unknown'
    if (index(c2,i) == alphabet::unknown)
      continue;

    // need to move a 'gap', and can merge with another 'gap'
    if (index(c2,i) == alphabet::gap) 
      assert(index(c1,i) == alphabet::unknown or index(c1,i) == alphabet::gap);

    // need to move a letter, and cannot merge w/ anything.
    else {
      assert(index(c2,i) >= 0);
      assert(index(c1,i) == alphabet::unknown);

      column(i,index(c2,i)) = c1;
    }

    index(c1,i) = index(c2,i);
    index(c2,i) = alphabet::unknown;
  }

  int after = count_unknowns(*this,c1);

  unknowns = unknowns + after - before;
  columns--;

  //  if (n_unknown() != unknowns)
  //    {cerr<<"E";abort();}

}


unsigned index_matrix::n_unknown() const
{
  unsigned total = 0;
  for(int i=0;i<size1();i++) {
    unsigned c_total_u = 0;
    unsigned c_total = 0;
    for(int j=0;j<size2();j++)
      if ((*this)(i,j) == alphabet::unknown)
	c_total_u++;
      else if ((*this)(i,j) >= 0)
	c_total++;
    if (c_total)
      total += c_total_u;
  }

  return total;
}

unsigned index_matrix::n_columns() const
{
  unsigned total = 0;
  for(int i=0;i<size1();i++) {
    unsigned c_total = 0;
    for(int j=0;j<size2();j++)
      if ((*this)(i,j) >= 0)
	c_total++;
    if (c_total)
      total++;
  }

  return total;
}

void index_matrix::check_column_indices() const
{
  for(int i=0;i<size2();i++)
    for(int c=0;c<size1();c++) {
      int x = operator()(c,i);
      if (x >= 0) 
	assert(column(i,x) == c);
    }
}

map<unsigned,pair<unsigned,unsigned> > index_matrix::merge(const Edges& E,double cutoff,bool strict)
{
  map<unsigned,pair<unsigned,unsigned> > graph;

  //-------- Merge some columns --------//
  foreach(e,E) 
  {
    if (e->p < cutoff) break;

    if (e->x2 == -1) {
      int c1 = column(e->s1,e->x1);

      if (strict and not consistent(c1,e->s2,-1,E,cutoff))
	continue;

      if (index(c1,e->s2) == -3) {
	unknowns--;
	index(c1,e->s2) = -1;
      }
    }
    else if (e->x1 == -1) {
      int c2 = column(e->s2,e->x2);

      if (strict and not consistent(c2,e->s1,-1,E,cutoff))
	continue;

      if (index(c2,e->s1) == -3) {
	unknowns--;
	index(c2,e->s1) = -1;
      }
    }
    else 
    {
      assert(e->x1 >= 0 and e-> x2>=0);

      int c1 = column(e->s1,e->x1);
      int c2 = column(e->s2,e->x2);

      if (c1 == c2)
	continue;

      if (columns_conflict(c1,c2))
	continue;

      if (strict and not consistent(c1,c2,E,cutoff))
	continue;
	  
      merge_columns(c1,c2);
    }

    graph[e->count] = pair<unsigned,unsigned>(columns,unknowns);
    //      if (n_columns() != columns)
    //	abort();
    //      if (n_unknown() != unknowns)
    //	{cerr<<"C";abort();}
  }

  return graph;
}

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>

using namespace boost;

typedef adjacency_list< vecS, vecS, bidirectionalS> Graph; 
typedef graph_traits<Graph>::vertex_descriptor Vertex;
typedef graph_traits<Graph>::edge_descriptor Edge_t;

struct cycle_exception: public std::exception {
  int x;
  int y;
  const char* what() const throw() {
    std::ostringstream w;
    w << "Adding edge "<<x<<"->"<<y<<" creates a cycle!";
    return w.str().c_str();
  }
  cycle_exception(int x_,int y_) throw() 
    :x(x_),y(y_) { }
  virtual ~cycle_exception() throw() {}
};

struct online_topo_sort
{
private:
  void do_add_edge(int x,int y);

public:
  Graph g;

  // these two mappings should always be inverse of each other
  vector<int> order;
  vector<Vertex> vertices;

  mutable vector<int> mark;

  void allow_edge(int x,int y);
  void add_edge(int x,int y);
  void merge_nodes(int x,int y);

  bool try_add_edge(int x,int y);
  bool try_merge_nodes(int x,int y);

  bool less_than(int x,int y) const;
  
  bool has_edge(int x,int y) const;

  void check_order() const;
  void check_in_edges(int n) const;
  void check_out_edges(int n) const;
  void check_node(int n) const;

  online_topo_sort(int n)
    :g(n),
     mark(n,0)
  {
    order = iota(n);

    for(int i=0;i<n;i++) {
      vertices.push_back(vertex(i,g));
      assert(get(vertex_index,g,i) == i);
    }
  }
};

void online_topo_sort::do_add_edge(int x,int y)
{
  Vertex vx = vertex(x,g);
  Vertex vy = vertex(y,g);

  assert(get(vertex_index,g,vx) == x);
  assert(get(vertex_index,g,vy) == y);

  assert(get(vertex_index,g,vertices[x]) == x);
  assert(get(vertex_index,g,vertices[y]) == y);

  assert(order[x] < order[y]);

  if ( not has_edge(x,y) )
    ::add_edge(vx,vy,g);
}

void check_empty(const vector<int>& mark)
{
  for(int i=0;i<mark.size();i++)
    if (mark[i]) abort();
}


vector<int> merge(const vector<int>& a, const vector<int>& b)
{
  vector<int> result;
  result.reserve(a.size()+b.size());
  int i=0,j=0;
  while ( i<a.size() or j<b.size()) 
  {
    if (i < a.size() and j < b.size()) {
      if (a[i] < b[j])
	result.push_back(a[i++]);
      else
	result.push_back(b[j++]);
    }
    else if (i < a.size())
      result.push_back(a[i++]);
    else
      result.push_back(b[j++]);
  }
  return result;
}


bool online_topo_sort::has_edge(int x, int y) const
{
  Edge_t e;
  bool found;
  Vertex vx = vertex(x,g);
  Vertex vy = vertex(y,g);

  tie(e,found) = edge(vx,vy,g);
  return found;
}

bool online_topo_sort::try_add_edge(int x, int y)
{
  try {
    add_edge(x,y);
    return true;
  }
  catch (const cycle_exception& c)
  {
    return false;
  }
}

// y is the one that goes away when they are merged
bool online_topo_sort::try_merge_nodes(int x,int y)
{
  try {
    merge_nodes(x,y);
    return true;
  }
  catch (const cycle_exception& c)
  {
    return false;
  }
}

void online_topo_sort::allow_edge(int x,int y)
{
  assert(0 <= x and x < num_vertices(g));
  assert(0 <= y and y < num_vertices(g));

  if (order[x] < order[y])
    return;

#ifndef NDEBUG
  check_empty(mark);
#endif
  vector<int> stack;

  //find RF (i.e. items after  y, in the range order[y]..order[x])
  vector<int> items_after_y; 

  stack.push_back(y);
  while(not stack.empty()) 
  {
    int vn = stack.back(); stack.pop_back();

    if (mark[vn]) continue; // items MAY be on the stack twice

    if (vn == x) {
      for(int i=0;i<items_after_y.size();i++)
	mark[items_after_y[i]] = 0;
#ifndef NDEBUG
      check_empty(mark);
#endif
      throw cycle_exception(x,y);
    }

    mark[vn] = 1;
    items_after_y.push_back(vn);

    graph_traits<Graph>::out_edge_iterator vi, vend;
    for(tie(vi,vend) = out_edges(vertices[vn],g); vi != vend; ++vi)
    { 
      Vertex vo = target(*vi,g);
      int von = get(vertex_index,g,vo);
      if (order[von] <= order[x] and not mark[von])
	// this line causes an ICE in 4.2 if -funroll-loops is set.
	// error is in get_biv_step at loop-iv.c:775
	stack.push_back(von);
    }
  }
  sort(items_after_y.begin() , items_after_y.end() , sequence_order<int>(order));

  //find RB (i.e. items before x, in the range order[y]..order[x])
  vector<int> items_before_x;
  stack.push_back(x);
  while(not stack.empty()) 
  {
    int vn = stack.back(); stack.pop_back();

    if (mark[vn]) continue; // items MAY be on the stack twice

    if (vn == y)
      std::abort(); // cycle - but we should have caught this above!

    mark[vn] = 1;
    items_before_x.push_back(vn);

    graph_traits<Graph>::in_edge_iterator vi, vend;
    for(tie(vi,vend) = in_edges(vertices[vn],g); vi != vend; ++vi)
    { 
      Vertex vo = source(*vi,g);
      int von = get(vertex_index,g,vo);
      if (order[von] >= order[y] and not mark[von])
	stack.push_back(von);
    }
  }
  sort(items_before_x.begin(), items_before_x.end(), sequence_order<int>(order));

  //------ unmark items + get ordered list (L) of items ------//
  vector<int> L;
  for(int i=0;i<items_before_x.size();i++) {
    int w = items_before_x[i];
    items_before_x[i] = order[w];
    mark[w] = 0;
    L.push_back(w);
  }

  for(int i=0;i<items_after_y.size();i++) {
    int w = items_after_y[i];
    items_after_y[i] = order[w];
    mark[w] = 0;
    L.push_back(w);
  }
    
  //------------ get ordered list (R) of orders -------------//
  vector<int> R = merge(items_before_x, items_after_y);

  // set the new order of the i-th item to the 
  for(int i=0;i<L.size();i++) 
    order[L[i]] = R[i];

#ifndef NDEBUG
  check_empty(mark);
  check_node(x);
  check_node(y);
#endif

  assert(order[x] < order[y]);
}

void online_topo_sort::check_in_edges(int x) const
{
  Vertex vx = vertex(x,g);

  graph_traits<Graph>::in_edge_iterator vi, vend;

  for(tie(vi,vend) = in_edges(vx,g); vi != vend; ++vi)
  { 
    Vertex v = source(*vi,g);
    int vn  = get(vertex_index,g,v);
    
    if (not(order[vn] < order[x]))
      abort();
  }
}


void online_topo_sort::check_out_edges(int x) const
{
  Vertex vx = vertex(x,g);

  graph_traits<Graph>::out_edge_iterator vi, vend;

  for(tie(vi,vend) = out_edges(vx,g); vi != vend; ++vi)
  { 
    Vertex v = target(*vi,g);
    int vn  = get(vertex_index,g,v);
    
    if (not (order[x] < order[vn]))
      abort();
  }
}


void online_topo_sort::check_node(int n) const
{
  check_in_edges(n);
  check_out_edges(n);
}


// separate add_edge from try_add_edge, which would include a check
void online_topo_sort::add_edge(int x,int y)
{
  if (x == y)
    throw myexception()<<"Trying to make a node less than itself!";

  allow_edge(x,y);

  do_add_edge(x,y);
#ifndef NDEBUG
  check_order();
#endif
}

// y is the one that goes away when they are merged
void online_topo_sort::merge_nodes(int x,int y)
{
  if (order[x] < order[y])
    allow_edge(y,x);
  else if (order[y] < order[x])
    allow_edge(x,y);
  
  Vertex vy = vertices[y];
  assert(get(vertex_index,g,vy) == y);

  // add to x edges that point to y 
  {
    graph_traits<Graph>::in_edge_iterator vi, vend;
    for(tie(vi,vend) = in_edges(vy,g); vi != vend; ++vi)
      { 
	Vertex v = source(*vi,g);
	int vn  = get(vertex_index,g,v);

	assert(order[vn] < order[y]);

	add_edge(vn,x);
      }
  }
  
  // add to x edges that come from y
  {
    graph_traits<Graph>::out_edge_iterator vi, vend;
    for(tie(vi,vend) = out_edges(vy,g); vi != vend; ++vi)
      { 
	Vertex v = target(*vi,g);
	int vn  = get(vertex_index,g,v);

	assert(order[y] < order[vn]);

	add_edge(x,vn);
      }
  }

  // remove edges from vy
  clear_vertex(vy,g);

  // remove_vertex(vy,g); // this would SHIFT all of the node names!
}

bool online_topo_sort::less_than(int x, int y) const
{
  if (order[x] >= order[y])
    return false;

#ifndef NDEBUG
  check_empty(mark);
#endif

  vector<int> items_after_x;
  vector<int> stack;

  stack.push_back(x);
  while(not stack.empty()) 
  {
    int vn = stack.back();

    if (mark[vn]) continue; // items MAY be on the stack twice

    if (vn == y) {
      for(int i=0;i<items_after_x.size();i++)
	mark[items_after_x[i]] = 0;
#ifndef NDEBUG
      check_empty(mark);
#endif
      return false;
    }

    mark[vn] = 1;
    items_after_x.push_back(vn);

    stack.pop_back();

    graph_traits<Graph>::out_edge_iterator vi, vend;
    for(tie(vi,vend) = out_edges(vertices[vn],g); vi != vend; ++vi)
    { 
      Vertex vo = target(*vi,g);
      int von = get(vertex_index,g,vo);
      if (order[von] <= order[y] and not mark[von])
	stack.push_back(von);
    }
  }

  for(int i=0;i<items_after_x.size();i++)
    mark[items_after_x[i]] = 0;

#ifndef NDEBUG
  check_empty(mark);
#endif

  return true;
}

void online_topo_sort::check_order() const
{
  graph_traits<Graph>::vertex_iterator vi, vi_end;
  for (tie(vi, vi_end) = ::vertices(g); vi != vi_end; ++vi) 
  {
    int index1 = get(vertex_index,g,*vi);
    graph_traits<Graph>::out_edge_iterator ei, eend;
    for(tie(ei,eend) = out_edges(*vi,g); ei != eend; ++ei)
    { 
      int index2 = get(vertex_index,g,target(*ei,g));
      assert(order[index1] < order[index2]);
    }
  }
}

map<unsigned,pair<unsigned,unsigned> > index_matrix::merge2(const Edges& E,double cutoff,bool strict)
{
  map<unsigned,pair<unsigned,unsigned> > plot;

  vector<int> order = iota<int>(size2());
  vector<int> i_order = invert(order);

  //----- Create initial graph of index matrix ----//
  using namespace boost;
  // what properties should this graph have?

  online_topo_sort S(size1());

  for(int c=0;c<size1();c++) 
    for(int i=0;i<size2();i++) 
    {
      int x = operator()(c,i);

      if (x == -1 or x == -3) 
	continue;
      else if (x+1 < length(i)) {
	int c2 = column(i,x+1);
	S.add_edge(c,c2);
      }
    }

#ifndef NDEBUG
  // complain if we find a cycle...
  get_ordered_matrix(*this);
  S.check_order();
#endif

  //-------- Merge some columns --------//
  foreach(e,E) 
  {
    if (e->p < cutoff) break;

    // add a gap, if we are able
    if (e->x2 == -1) {
      int c1 = column(e->s1,e->x1);

      if (strict and not consistent(c1,e->s2,-1,E,cutoff))
	continue;

      if (index(c1,e->s2) == -3) {
	unknowns--;
	index(c1,e->s2) = -1;
      }
    }
    // add a gap, if we are able
    else if (e->x1 == -1) {
      int c2 = column(e->s2,e->x2);

      if (strict and not consistent(c2,e->s1,-1,E,cutoff))
	continue;

      if (index(c2,e->s1) == -3) {
	unknowns--;
	index(c2,e->s1) = -1;
      }
    }
    // merge two columns
    else 
    {
      assert(e->x1 >= 0 and e-> x2>=0);

      int c1 = column(e->s1,e->x1);
      int c2 = column(e->s2,e->x2);

      if (c1 == c2)
	continue;

      if (columns_conflict(c1,c2))
	continue;

      if (strict and not consistent(c1,c2,E,cutoff))
	continue;
	  
      try {
	if (c1 > c2) std::swap(c1,c2);
	S.merge_nodes(c1,c2);
	merge_columns(c1,c2);
      }
      catch (cycle_exception& c)
      { } // don't do anything
    }

    plot[e->count] = pair<unsigned,unsigned>(columns,unknowns);
    //      if (n_columns() != columns)
    //	abort();
    //      if (n_unknown() != unknowns)
    //	{cerr<<"C";abort();}
  }

#ifndef NDEBUG
  S.check_order();
#endif
  return plot;
}

    

bool skips(const matrix<int>& M,int c,const vector<int>& index) 
{
  for(int i=0;i<M.size2();i++) 
  {
    if (M(c,i) < 0) continue;

    assert(M(c,i) > index[i]);

    if (M(c,i) > index[i]+1)
      return true;
  }

  return false;
}

// There is a better algorithm in Section 7 "The mathematics of 
// distance-based alignment" in the text S1 supplement of the the FSA paper.
matrix<int> get_ordered_matrix(const index_matrix& M)
{
#ifndef NDEBUG
  M.check_column_indices();
#endif

  //-------- sort columns of M ----------//
  vector<int> index(M.size2(),-1);
  vector<int> columns;
  while(true) {

    bool all_done=true;
    for(int i=0;i<index.size();i++)
      if (index[i]+1 < M.length(i))
	all_done=false;

    if (all_done) break;
    
    int c1=-1;
    for(int i=0;i<index.size();i++) 
    {
      // skip this sequence if its already done
      if (index[i]+1 >= M.length(i)) continue;
      
      // what is the column where the next index in this sequence appears.
      c1 = M.column(i,index[i]+1);
      
      // if this column does not involve skipping a letter in a sequence besides @i
      if (skips(M,c1,index)) 
	c1 = -1;
      else
	break;
    }
    
    // If we didn't find any column that contains a 'next letter' without
    // skipping something, then ... we have a cycle?
    if (c1 == -1)
    {
      abort();
    }
    
    columns.push_back(c1);
    
    // record these letters as processed.
    for(int i=0;i<M.size2();i++)
      if (M(c1,i) >= 0) {
	index[i]++;
	assert(M(c1,i) == index[i]);
      }
    
  }

  matrix<int> M2(columns.size(),M.size2());

  for(int i=0;i<M2.size1();i++) {
    for(int j=0;j<M2.size2();j++)
      M2(i,j) = M(columns[i],j);
  }

  return M2;
}

alignment get_alignment(const matrix<int>& M, const alignment& A1) 
{
  alignment A2 = A1;
  A2.changelength(M.size1());

  // Reconstruct the list of letters
  vector<vector<int> > sequences;
  for(int i=0;i<A1.n_sequences();i++) {
    vector<int> sequence;
    for(int c=0;c<A1.length();c++) {
      if (A1.character(c,i))
	sequence.push_back(A1(c,i));
    }
    sequences.push_back(sequence);
  }

  // Plug the letters into their slots
  for(int i=0;i<A2.n_sequences();i++) {
    for(int c=0;c<A2.length();c++) {
      int index = M(c,i);

      if (index >= 0)
	index = sequences[i][index];

      A2.set_value(c,i, index);
    }
  }

  return A2;
}

alignment get_ordered_alignment(const alignment& A)
{
  // There is a better algorithm in Section 7 "The mathematics of 
  // distance-based alignment" in the text S1 supplement of the the FSA paper.
  return get_alignment(get_ordered_matrix(index_matrix(A)),A);
}
