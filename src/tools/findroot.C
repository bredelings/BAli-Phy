#include "findroot.H"
#include "optimize.H"

#include <vector>

using std::vector;

const double max_float = 3.40282347e+38F;

double rootdistance(const Tree& T,int leaf,int b,double x) 
{
  assert( 0 <= x and x <= 1);

  const_branchview bv = T.directed_branch(b);

  double d=0;
  int from = -1;

  if (T.partition(b)[leaf]) {
    d = bv.length()*(1-x);
    from = bv.target();
  }
  else {
    d = bv.length()*x;
    from = bv.source();
  }
  d += T.distance(from,leaf);
  return d;
}


using namespace optimize;

class rooterror: public function {
  Tree T;
  int b;
  double total;
public:
  double operator()(const optimize::Vector& v) const;
  rooterror(const Tree& T1,int i1): T(T1),b(i1) 
  {
    total = 0;
    for(int b=0;b<T.n_branches();b++)
      total += T.branch(b).length();
    assert(total > 0);
  }
};

double rooterror::operator()(const optimize::Vector& v) const {
  assert(v.size() == 1);
  double x = v[0];
  if (x < 0) return -max_float;
  if (x > 1) return -max_float;

  vector<double> distances(T.n_leaves());
  for(int i=0;i<distances.size();i++)
    distances[i] = rootdistance(T,i,b,x);

  double m1 = 0;
  double m2 = 0;
  for(int i=0;i<distances.size();i++) {
    double E = log(distances[i] + total/1000);
    m1 += E;
    m2 += E*E;
  }
  m1 /= T.n_leaves();
  m2 /= T.n_leaves();
  double Var = (m2 - m1*m1);

  assert(Var >= 0);
  return -Var;
}

void find_root(const Tree& T,int& rootb,double& rootd) {
  vector<double> E(T.n_branches());
  vector<double> x(T.n_branches());

  for(int b=0;b<T.n_branches();b++) {
    rooterror f(T,b);
    optimize::Vector start(0.5,1);
    optimize::Vector end = search_basis(start,f);
    E[b] = f(end);
    x[b] = end[0];
  }

  int best = 0;
  for(int i=1;i<E.size();i++) {
    if (E[i] > E[best]) best=i;
  }

  rootb = best;
  rootd = x[best];
}

RootedSequenceTree find_rooted_tree(const SequenceTree& T)
{
  int b=-1;
  double x=-1;

  find_root(T,b,x);

  RootedSequenceTree RT = add_root(T,b);

  vector<branchview> branches;
  append(RT.root().branches_out(),branches);

  assert(branches.size() == 2);

  if (branch_partition(RT,branches[0]) == branch_partition(T,b))
    x = 1.0 - x;

  branches[0].set_length(x*T.branch(b).length());
  branches[1].set_length((1.0-x)*T.branch(b).length());

  return RT;
}
