#include "distance-methods.H"

Matrix probability_to_distance(const Matrix &C) {
  Matrix D(C.size1(),C.size2());
  for(int i=0;i<C.size1();i++)
    for(int j=0;j<C.size2();j++) {
      assert(0 <= C(i,j) and C(i,j) <= 1.0);
      D(i,j) = -log(C(i,j));
    }
  return D;
}

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
  
  vector<int> xset; xset.reserve(T.n_leaves());
  vector<int> yset; yset.reserve(T.n_leaves());
  
  for(int b=0;b<branches.size();b++) {
    if (branches[b].is_leaf_branch()) continue;

    vector<const_branchview> children;
    append(branches[b].branches_after(),children);
    
    assert(children.size() == 2);
    xset = leaf_sets[children[0]];
    yset = leaf_sets[children[1]];

    double temp = 0;
    for(int i=0;i<children.size();i++)
      temp += d[ children[i].undirected_name() ];

    double sum = 0;
    for(int x=0;x<xset.size();x++)
      for(int y=0;y<yset.size();y++)
	sum += D(xset[x],yset[y]);

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

vector<double> FastLeastSquares(const Tree& T, const Matrix & D,
				const vector<vector<int> >& leaf_sets) 
{
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

Matrix DistanceMatrix(const Tree& T) {
  const int n = T.n_leaves();
  Matrix D(n,n);

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      D(i,j) = D(j,i) = T.distance(i,j);

  return D;
}

Matrix C(const Matrix& M) {
  Matrix I = M;
  for(int i=0;i<I.size1();i++)
    for(int j=0;j<I.size2();j++)
      I(i,j) = 1.0 - I(i,j);

  return I;
}

