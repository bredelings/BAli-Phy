/*
   Copyright (C) 2004-2006,2009 Benjamin Redelings

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

#include "distance-methods.H"

#include <list>
#include <valarray>
#include "boost/tuple/tuple.hpp"
#include "util.H"
#include "inverse.H"

using std::list;
using std::vector;

using boost::dynamic_bitset;
using namespace boost::tuples;

/// Compute the sum of all path lengths delta[b] passing through each branch b.
vector<double> FastMTM(const Tree& T,const Matrix& D,
		       const vector<vector<int> >& leaf_sets) 
{
  const int N = T.n_leaves();

  vector<double> d(T.n_branches(),-1);

  // compute delta[i]*D for leaf taxa [i]
  for(int i=0;i<T.n_leafbranches();i++) {
    double sum = 0;
    for(int j=0;j<N;j++)
      if (j!=i)
	sum += D(i,j);
    d[i] = sum;
  }
  
  vector<const_branchview> branches = branches_from_node(T,T.n_leaves());
  
  for(int b=0;b<branches.size();b++) 
  {
    if (branches[b].is_leaf_branch()) continue;

    vector<const_branchview> children;
    append(branches[b].branches_after(),children);
    
    double temp = 0;
    for(int i=0;i<children.size();i++)
      temp += d[ children[i].undirected_name() ];

    double sum = 0;
    for(int i=0;i<children.size();i++)
      for(int j=0;j<i;j++) {
      
	const vector<int>& xset = leaf_sets[children[i]];
	const vector<int>& yset = leaf_sets[children[j]];

	for(int x=0;x<xset.size();x++)
	  for(int y=0;y<yset.size();y++)
	    sum += D(xset[x],yset[y]);
      }

    d[branches[b].undirected_name()] = temp - 2.0*sum;
  }
  
  return d;
}

vector<double> SlowMTM(const Tree& T,const Matrix& D,
		       const vector<vector<int> >& leaf_sets) 
{
  vector<double> d(T.n_branches(),0);

  for(int b=0;b<T.n_branches();b++) {
    vector<int> xset = leaf_sets[T.directed_branch(b)];
    vector<int> yset = leaf_sets[T.directed_branch(b).reverse()];

    for(int i=0;i<xset.size();i++)
      for(int j=0;j<yset.size();j++)
	d[b] += D(xset[i],yset[j]);
  }
  return d;
}

// Solve (A^t * W * A) b = (A^t * W * d) , where W is "diagonal" (on paths)

// M1 = sum_ij (A[k,ij] * A[l,ij] * W[ij])  (symmetric)

// M2 = sum_ij (A[k,ij] * W[ij] * D[ij])

// M1[k,l] * b[l] = M2[k]

bool A(const Tree& T,int k,int i,int j)
{
  const dynamic_bitset<>& partition = T.partition(k);
  return partition[i] != partition[j];
}

vector<double> LeastSquares(const Tree& T, const Matrix & D, const Matrix& W,
			    const vector<vector<int> >& leaf_sets) 
{
  const int N = T.n_leaves();
  const int B = T.n_branches();

  assert(D.size1() == N);
  assert(D.size2() == N);
  assert(W.size1() == N);
  assert(W.size2() == N);
  Matrix WD = D;
  for(int i=0;i<WD.size1();i++)
    for(int j=0;j<WD.size2();j++)
      WD(i,j) *= W(i,j);

  vector<double> M2v = FastMTM(T,WD,leaf_sets);
  Matrix M2(B,1);
  for(int i=0;i<B;i++)
    M2(i,0) = M2v[i];

  Matrix M1(B,B);
  for(int l=0;l<B;l++) 
  {

    //AWL(i,j) = A[l,ij] * W[ij];
    Matrix ALW = W;
    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
	if (not A(T,l,i,j))
	  ALW(i,j) = 0;

    vector<double> column = FastMTM(T,ALW,leaf_sets);

    for(int k=0;k<B;k++)
      M1(k,l) = column[k];
  }

  Matrix x = solve(M1,M2);

  assert(x.size1() == B);
  assert(x.size2() == 1);

  vector<double> b(B);
  for(int i=0;i<b.size();i++)
    b[i] = x(i,0);

  return b;
}

vector<double> LeastSquares(const Tree& T, const Matrix & D,
			    const vector<vector<int> >& leaf_sets) 
{
  const int N = T.n_leaves();

  Matrix I(N,N);
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      I(i,j) = 1.0;

  return LeastSquares(T,D,I,leaf_sets);
}


vector<double> FastLeastSquares(const Tree& T, const Matrix & D,
				const vector<vector<int> >& leaf_sets) 
{
  assert(is_Cayley(T));

  vector<double> delta = FastMTM(T,D,leaf_sets);

  vector<double> b(T.n_branches());

  int i=0;
  vector<const_branchview> branches;
  for(;i<T.n_leafbranches();i++) {
    // get the 2 neighboring branches, pointing *outwards*.
    branches.clear();
    append(T.directed_branch(i).branches_after(),branches);
    assert(branches.size() == 2);

    int j = branches[0].undirected_name();
    int k = branches[1].undirected_name();
    double Nj = leaf_sets[branches[0]].size();
    double Nk = leaf_sets[branches[1]].size();
    
    b[i] = (1+Nj+Nk)*delta[i]-(1+Nj-Nk)*delta[j] - (1-Nj+Nk)*delta[k];
    b[i] /= (4*Nj*Nk);
  }

  for(;i<T.n_branches();i++) {
    // get the 4 neighboring branches, pointing *outwards*.
    branches.clear();
    append(T.directed_branch(i).branches_after(),branches);
    append(T.directed_branch(i).branches_before(),branches);
    branches[2] = branches[2].reverse();
    branches[3] = branches[3].reverse();
    assert(branches.size() == 4);

    int j = branches[0].undirected_name();
    int k = branches[1].undirected_name();
    int l = branches[2].undirected_name();
    int m = branches[3].undirected_name();

    double Nj = leaf_sets[branches[0]].size();
    double Nk = leaf_sets[branches[1]].size();
    double Nl = leaf_sets[branches[2]].size();
    double Nm = leaf_sets[branches[3]].size();

    double N  = T.n_leaves();
    assert(Nj + Nk + Nl + Nm <= N);
    
    b[i] = (N/Nj + N/Nk + N/Nl + N/Nm - 4)*delta[i] +
      (Nj + Nk)/(Nj*Nk)*((2*Nk-N)*delta[j] + (2*Nj-N)*delta[k]) +
      (Nl + Nm)/(Nl*Nm)*((2*Nm-N)*delta[l] + (2*Nl-N)*delta[m]);
    
    b[i] /= (4*(Nj+Nk)*(Nl+Nm));
  }

  return b;
}


Matrix EdgesDistanceMatrix(const Tree& T) 
{
  const int N = T.n_nodes();
  const int n = T.n_leaves();
 
  vector<const_branchview> branches;
  branches.push_back(T.branch(0));
  branches.push_back(T.branch(0).reverse());

  list<int> points;

  Matrix D1(N,N);
  {
    int node = T.branch(0).target();
    D1(0,node) = D1(node,0) = 1;
  }
    
  while(not branches.empty()) 
  {
    int dead_point = branches.back().target();

    vector<const_branchview> new_branches;
    append(branches.back().branches_after(), new_branches);
    branches.pop_back();
    
    if (new_branches.empty()) {
      points.push_back(dead_point);
      continue;
    }

    for(int i=0;i<new_branches.size();i++) 
    {
      int p1 = new_branches[i].target();

      // Add distances from i -> leaves
      foreach(p2,points) {
	D1(p1,*p2) = D1(*p2,p1) = D1(*p2,dead_point) + 1;
      }

      // Add distances from i -> active points 
      for(int j=0;j<branches.size();j++)
      {
	int p2 = branches[j].target();
	D1(p1,p2) = D1(p2,p1) = D1(p2,dead_point) + 1;
      }

      // Add distances from i -> new points
      for(int j=0;j<i;j++) 
      {
	int p2 = new_branches[j].target();
	D1(p1,p2) = D1(p2,p1) = 2;
      }
    }

    branches.insert(branches.end(), new_branches.begin(), new_branches.end());
  }

  assert(points.size() == T.n_leaves());
  foreach(p,points)
    assert(T.node(*p).is_leaf_node());


  // Create the final matrix from the larger matrix
  Matrix D(n,n);
  for(int i=0;i<n;i++)
    D(i,i) = 0;

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      D(i,j) = D(j,i) = D1(i,j);

  /*
  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++) {
      if (not (std::abs(D(i,j) - T.edges_distance(i,j)) < 1.0e-8))
	std::cerr<<"D("<<i<<","<<j<<") = "<<D(i,j)<<"    T(i,j) = "<<T.edges_distance(i,j)<<endl;
      assert( std::abs(D(i,j) - T.edges_distance(i,j)) < 1.0e-8);
    }
  */

  return D;
}

Matrix DistanceMatrix(const Tree& T) 
{
  const int N = T.n_nodes();
  const int n = T.n_leaves();
 
  vector<const_branchview> branches;
  branches.push_back(T.branch(0));
  branches.push_back(T.branch(0).reverse());

  list<int> points;

  Matrix D1(N,N);
  {
    int node = T.branch(0).target();
    D1(0,node) = D1(node,0) = branches[0].length();
  }
    
  while(not branches.empty()) 
  {
    int dead_point = branches.back().target();

    vector<const_branchview> new_branches;
    append(branches.back().branches_after(), new_branches);
    branches.pop_back();
    
    if (new_branches.empty()) {
      points.push_back(dead_point);
      continue;
    }

    for(int i=0;i<new_branches.size();i++) 
    {
      int p1 = new_branches[i].target();

      double L = new_branches[i].length();

      // Add distances from i -> leaves
      foreach(p2,points) {
	D1(p1,*p2) = D1(*p2,p1) = D1(*p2,dead_point) + L;
      }

      // Add distances from i -> active points 
      for(int j=0;j<branches.size();j++)
      {
	int p2 = branches[j].target();
	D1(p1,p2) = D1(p2,p1) = D1(p2,dead_point) + L;
      }

      // Add distances from i -> new points
      for(int j=0;j<i;j++) 
      {
	int p2 = new_branches[j].target();
	D1(p1,p2) = D1(p2,p1) = L + new_branches[j].length();
      }
    }

    branches.insert(branches.end(), new_branches.begin(), new_branches.end());
  }

  assert(points.size() == T.n_leaves());
  foreach(p,points)
    assert(T.node(*p).is_leaf_node());


  // Create the final matrix from the larger matrix
  Matrix D(n,n);
  for(int i=0;i<n;i++)
    D(i,i) = 0;

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      D(i,j) = D(j,i) = D1(i,j);

  /*
  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++) {
      if (not (std::abs(D(i,j) - T.distance(i,j)) < 1.0e-8))
	std::cerr<<"D("<<i<<","<<j<<") = "<<D(i,j)<<"    T(i,j) = "<<T.distance(i,j)<<endl;
      assert( std::abs(D(i,j) - T.distance(i,j)) < 1.0e-8);
    }
  */
  return D;
}

Matrix C(const Matrix& M) {
  Matrix I = M;
  for(int i=0;i<I.size1();i++)
    for(int j=0;j<I.size2();j++)
      I(i,j) = 1.0 - I(i,j);

  return I;
}

double sum_1(const Matrix& D, int x)
{
  int N = D.size1();
  assert(N = D.size2());

  double total = 0;

  for(int i=0;i<N;i++)
    total += D(i,x);

  return total;
}

double sum_2(const Matrix& D, int x)
{
  int N = D.size1();
  assert(N = D.size2());

  double total = 0;

  for(int i=0;i<N;i++)
    total += D(x,i);

  return total;
}

std::pair<int,int> arg_min_dist(const Matrix& D)
{
  assert(D.size1() >= 2);
  assert(D.size1() == D.size2());

  int xmin = 0;
  int ymin = 1;
  double value = D(xmin,ymin);

  for(int i=0;i<D.size1();i++)
    for(int j=0;j<D.size2();j++)
      if (i != j and D(i,j) < value)
      {
	xmin = i;
	ymin = j;
	value = D(i,j);
      }

  return std::pair<int,int>(xmin, ymin);
}

int cluster_edges(Tree& T, const tree_edge& e1, const tree_edge& e2)
{
  assert(e1.node1 != e2.node1);
  assert(e1.node2 == e2.node2);

  nodeview u = T.create_node_on_branch( T.branch(e1) );

  T.reconnect_branch(e2.node1, e2.node2, u);

  return u;
}


Tree NJ(Matrix D)
{
  int N = D.size1();

  // Handle N=0
  if (N == 0)
  {
    Tree T;
    return T;
  }

  // Handle N=2
  if (N == 1) {
    Tree T;
    T.add_first_node();
    return T;
  }

  // Handle N=2
  if (N == 2)
  {
    Tree T;
    T.add_first_node();
    T.add_leaf_node(0);
    T.branch(0).set_length(D(0,1));
    return T;
  }

  Tree T = star_tree(N);
  vector<tree_edge> mapping = T.leaf_branches();
  while (1)
  {
    N = D.size1();
    if (N == 3) break;

    vector<double> divergence(N);
    for(int i=0;i<N;i++)
    {
      divergence[i] = 0;
      for(int j=0;j<N;j++)
	if (i != j)
	  divergence[i] += D(i,j);
    }

    Matrix M(N,N);
    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
      {
	M(i,j) = D(i,j) - (divergence[i] + divergence[j])/(N-2);
      }
    
    int f, g;
    tie(f,g) = arg_min_dist(M);
    
    //1. join the two leaves f and g to the new node u
    int u = cluster_edges(T, mapping[f], mapping[g]);
    tree_edge h(u, mapping[f].node2);
    
    //2. set the lengths of the edges from f->u and g->u
    T.directed_branch(mapping[f].node1, u).set_length( D(f,g) / 2 + (divergence[f]-divergence[g]) / (2*(N-2)) );
    T.directed_branch(mapping[g].node1, u).set_length( D(f,g) / 2 + (divergence[g]-divergence[f]) / (2*(N-2)) );
    
    //3. construct the new distance matrix and mapping 
    
    vector<int> new_to_old;
    vector<tree_edge> mapping2;
    for(int i=0;i<mapping.size();i++)
    {
      if (i == f or i ==g)
	;
      else
      {
	new_to_old.push_back(i);
	mapping2.push_back(mapping[i]);
      }
    }
    new_to_old.push_back(-1);
    mapping2.push_back(h);
    
    assert(N-1 == new_to_old.size());
    Matrix D2(N-1, N-1);
    
    for(int i=0; i< N-1; i++)
      for(int j=0;j < N-1; j++)
      {
	int I = new_to_old[i];
	int J = new_to_old[j];
	if (I != -1)
	  assert(mapping2[i] == mapping[I]);
	if (J != -1)
	  assert(mapping2[j] == mapping[J]);
	if (I == -1 and J == -1)
	  D2(i,j) = 0;
	else if (I == -1)
	  D2(i,j) = (D(f,J) + D(g,J) - D(f,g))/2.0;
	else if (J == -1)
	  D2(i,j) = (D(I,f) + D(I,g) - D(f,g))/2.0;
	else
	  D2(i,j) = D(I,J);
      }

    D.resize(N-1, N-1);
    D = D2;
    mapping = mapping2;

    std::cout<<write_no_names(T)<<std::endl;
  }

  //5. if there is just one branch left, the set its length and quit.
  assert(D.size1() == 3);
  assert(mapping[0].node2 == mapping[1].node2);
  assert(mapping[0].node2 == mapping[2].node2);

  double L0 = ( D(0,1) + D(0,2) - D(1,2) ) / 2;
  double L1 = ( D(1,0) + D(1,2) - D(0,2) ) / 2;
  double L2 = ( D(2,0) + D(2,1) - D(0,1) ) / 2;

  T.directed_branch(mapping[0]).set_length( L0 );
  T.directed_branch(mapping[1]).set_length( L1 );
  T.directed_branch(mapping[2]).set_length( L2 );

  return T;
}
