#include "findroot.H"
#include "optimize.H"

const double max_float = 3.40282347e+38F;

double rootdistance(const tree& T,int leaf,int b,double x) {
  assert( 0 <= x and x <= 1);

  int child = T.branch(b).child();
  int parent = T.branch(b).parent();

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
  tree T;
  int b;
  double total;
public:
  double operator()(const vector<double>& v) const;
  rooterror(const tree& T1,int i1): T(T1),b(i1) 
  {
    for(int b=0;b<T.branches();b++)
      total += T.branch(b).length();
    assert(total > 0);
  }
};

double rooterror::operator()(const vector<double>& v) const {
  assert(v.size() == 1);
  double x = v[0];
  if (x < 0) return -max_float;
  if (x > 1) return -max_float;

  vector<double> distances(T.leaves());
  for(int i=0;i<distances.size();i++)
    distances[i] = rootdistance(T,i,b,x);

  double m1 = 0;
  double m2 = 0;
  for(int i=0;i<distances.size();i++) {
    double E = log(distances[i] + total/1000);
    m1 += E;
    m2 += E*E;
  }
  m1 /= T.leaves();
  m2 /= T.leaves();
  double Var = (m2 - m1*m1);

  assert(Var >= 0);
  return -Var;
}

void find_root(const tree& T,int& rootb,double& rootd) {
  vector<double> E(T.branches());
  vector<double> x(T.branches());

  for(int b=0;b<T.branches();b++) {
    rooterror f(T,b);
    vector<double> start(1,0.5);
    vector<double> end = search_basis(start,f);
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
