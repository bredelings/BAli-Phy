#include "distance-methods.H"

#include <list>
#include <valarray>
#include "util.H"
#include "inverse.H"

using std::list;

using boost::dynamic_bitset;

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

typedef ublas::matrix<double,ublas::column_major> MatrixC;

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
  MatrixC M2(B,1);
  for(int i=0;i<B;i++)
    M2(i,0) = M2v[i];

  MatrixC M1(B,B);
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

  MatrixC x = solve(M1,M2);

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
    assert(T[*p].is_leaf_node());


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
    assert(T[*p].is_leaf_node());


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

