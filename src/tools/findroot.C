#include "findroot.H"
#include "optimize.H"

#include <vector>

using std::vector;

const double max_float = 3.40282347e+38F;

double rootdistance(const Tree& T,int leaf,int b,double x) {
  assert( 0 <= x and x <= 1);

  int child = T.branch(b).source();
  int parent = T.branch(b).target();

  double d=0;
  int from = -1;
  // The leaf is on the parent's side of the branch
  if (T.partition(child,parent)[leaf]) {
    d = T.branch(b).length()*(1-x);
    from = parent;
  }
  else {
    d = T.branch(b).length()*x;
    from = child;
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
