#include "computation/computation.H"
#include <valarray>
#include "mytypes.H"
#include "sequence/alphabet.H"

using std::valarray;
using std::vector;

extern "C" closure builtin_function_plus_gwF(OperationArgs& Args)
{
  const alphabet& a = *Args.evaluate_as<alphabet>(0);

  double f = *Args.evaluate_as<Double>(1);

  object_ptr< const Vector<double> > pi_ = Args.evaluate_as< Vector<double> >(2);

  object_ptr<MatrixObject> R( new MatrixObject );

  const int n = a.size();

  R->t.resize(n, n);

  // compute frequencies
  vector<double> pi = *pi_;
  normalize(pi);
  assert(a.size() == pi.size());
    
  // compute transition rates
  valarray<double> pi_f(n);
  for(int i=0;i<n;i++)
    pi_f[i] = pow(pi[i],f);

  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      R->t(i,j) = pi_f[i]/pi[i] * pi_f[j];

  // diagonal entries should have no effect
  for(int i=0;i<n;i++)
    R->t(i,i) = 0;

  return R;
}
