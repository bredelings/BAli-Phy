#include <fstream>
#include "SModel.H"
#include "exponential.H"
#include "rng.H"

void EquilibriumModel::recalc() {

  // Determine diagonal entries
  for(int i=0;i<S.size1();i++) {
    double sum=0;
    for(int j=0;j<S.size2();j++) {
      if (i==j) continue;
      sum += S(i,j);
    }
    S(i,i) = -sum;
  }

  // Rescale so expected that mutation rate is 1
  double scale=0;
  for(int i=0;i<S.size1();i++) 
    scale += pi[i]*S(i,i)*pi[i];

  S /= -scale;

  // Move from 'S' to 'S+F'
  for(int i=0;i<S.size1();i++)
    for(int j=0;j<S.size2();j++)
      Q(i,j) = S(i,j)*pi[j];


  // Rescale so expected that mutation rate is 1
  scale = 0;
  for(int i=0;i<S.size1();i++) 
    scale += pi[i]*Q(i,i);

  std::cerr<<"scale = "<<scale<<endl;
}

Matrix EquilibriumModel::transition_p(double t) const {
  BMatrix D(a.size(),a.size());
  for(int i=0;i<a.size();i++)
    D(i,i) = pi[i];

  return exp(S,D,t);
}


void HKY::fiddle() {
  const double sigma = 0.05;
  double k = kappa() + gaussian(0,sigma);
  if (k<0) k = -k;
  if (k==0) k = kappa();

  kappa(k);
}

double HKY::prior() {
  return  gsl_ran_lognormal_pdf(kappa(),0,0.1);
}

void HKY::recalc() {
  assert(a.size()==4);

  S(A,G) = kappa();
  S(A,C) = 1;
  S(A,T) = 1;

  S(G,A) = kappa();
  S(G,C) = 1;
  S(G,T) = 1;

  S(C,A) = 1;
  S(C,G) = 1;
  S(C,T) = kappa();

  S(T,A) = 1;
  S(T,G) = 1;
  S(T,C) = kappa();

  EquilibriumModel::recalc();
}

void HKY::setup_alphabet() {
  A = a['A'];
  G = a['G'];
  C = a['C'];
  try {
    T = a['T'];
  }
  catch (bad_letter& e) {
    T = a['U'];
  }
}

void EQU::recalc() {
  for(int i=0;i<a.size();i++)
    for(int j=0;j<a.size();j++)
      S(i,j) = 1;

  EquilibriumModel::recalc();
}

void Empirical::recalc() {
  EquilibriumModel::recalc();
}

void Empirical::load_file(const char* filename) {
  std::ifstream ifile(filename);

  if (not ifile)
    throw myexception(string("Couldn't open file '")+filename+"'");

  for(int i=0;i<a.size();i++)
    for(int j=0;j<i;j++) {
      ifile>>S(i,j);
      S(j,i) = S(i,j);
    }

  for(int i=0;i<a.size();i++)
    ifile>>pi[i];
}

