#include "matcache.H"

using std::vector;

void Mat_Cache::WeightedFrequencyMatrix(double* F) const
{
    // cache matrix of frequencies
  const int M = n_base_models(); // n_models
  const int S = n_states();      // n_states

  vector<double> D = distribution();
  for(int m=0;m<M;m++) {
    double p = D[m];
    const vector<double>& f = frequencies(m);
    for(int s=0;s<S;s++) 
      F[m*S + s] = f[s]*p;
  }
}

Matrix Mat_Cache::WeightedFrequencyMatrix() const
{
  Matrix M(n_base_models(), n_states());
  WeightedFrequencyMatrix(M.begin());
  return M;
}

void Mat_Cache::FrequencyMatrix(double* F) const
{
    // cache matrix of frequencies
  const int M = n_base_models(); // n_models
  const int S = n_states();      // n_states

  for(int m=0;m<M;m++) 
  {
    const vector<double>& f = frequencies(m);
    for(int s=0;s<S;s++) 
      F[m*S + s] = f[s];
  }
}

Matrix Mat_Cache::FrequencyMatrix() const
{
  Matrix M(n_base_models(),n_states());
  FrequencyMatrix(M.begin());
  return M;
}
