#include "parameters.H"
#include "exponential.H"

void HKY::recalc() {
  assert(a->size()==4);

  const std::valarray<double>& pi = frequencies_;

  const int A = (*a)['A'];
  const int G = (*a)['G'];
  const int C = (*a)['C'];
  const int T = (*a)['T'];

  rates_(A,G) = kappa()*pi[G];
  rates_(A,C) = pi[C];
  rates_(A,T) = pi[T];

  rates_(G,A) = kappa()*pi[A];
  rates_(G,C) = pi[C];
  rates_(G,T) = pi[T];

  rates_(C,A) = pi[A];
  rates_(C,G) = pi[G];
  rates_(C,T) = kappa()*pi[T];

  rates_(T,A) = pi[A];
  rates_(T,G) = pi[G];
  rates_(T,C) = kappa()*pi[C];

  for(int i=0;i<4;i++) {
    double sum=0;
    for(int j=0;j<4;j++) {
      if (i==j) continue;
      sum += rates_(i,j);
    }
    rates_(i,i) = -sum;
  }

  double scale=0;
  for(int i=0;i<4;i++) 
    scale += rates()(i,i)*pi[i];

  rates_ /= -scale;

  std::cerr<<"scale1 = "<<scale<<endl;

  scale=0;
  for(int i=0;i<4;i++) 
    scale += rates()(i,i)*pi[i];

  std::cerr<<"scale2 = "<<scale<<endl;
}

void HKY::fiddle() {
}

Matrix HKY::transition_p(double t) const {
  return exp(rates_,t);
}


void Parameters::setlength(int b,double l) {
  assert(l >= 0);
  assert(b >= 0 and b < T.branches());
  T.branch(b).length = l;
  substitution_[b] = SModel->transition_p(T.branch(b).length);
}


void Parameters::recalc() {
  substitution_.clear();
  for(int i=0;i<T.branches();i++) 
    substitution_.push_back(SModel->transition_p(T.branch(i).length));
}


Parameters::Parameters(SubstitutionModel& SM,const SequenceTree& t)
  :SModel(&SM),T(t)
{
  branch_mean = 1.0;
  recalc();
}
