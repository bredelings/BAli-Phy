#include "matcache.H"

using std::vector;

void Mat_Cache::WeightedFrequencyMatrix(Matrix& F) const
{
    // cache matrix of frequencies
  const int M = n_base_models(); // n_models
  const int S = n_states();      // n_states

  F.resize(M, S);
  
  vector<double> D = distribution();
  for(int m=0;m<M;m++) {
    double p = D[m];
    const vector<double>& f = frequencies(m);
    for(int s=0;s<S;s++) 
      F(m,s) = f[s]*p;
  }
}

// frequencies(m) = base_model(m).frequencies(); ?

void Mat_Cache::FrequencyMatrix(Matrix& F) const
{
    // cache matrix of frequencies
  const int M = F.size1(); // n_models
  const int S = F.size2(); // n_states
  
  for(int m=0;m<M;m++) 
  {
    const vector<double>& f = frequencies(m);
    for(int s=0;s<S;s++) 
      F(m,s) = f[s];
  }
}
