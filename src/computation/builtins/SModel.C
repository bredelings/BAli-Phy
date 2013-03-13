#include "computation/computation.H"
#include "math/exponential.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_lExp(OperationArgs& Args)
{
  const EigenValues& L = *Args.evaluate_as<EigenValues>(0);
  const Vector<double>& pi = *Args.evaluate_as< Vector<double> >(1);
  double t = *Args.evaluate_as<Double>(2);

  Matrix E = exp(L, pi, t);
  MatrixObject* M = new MatrixObject;
  M->t.assign_temporary(E);
  return M;
}

using std::vector;

extern "C" closure builtin_function_q_from_s_and_r(OperationArgs& Args)
{
  object_ptr<const SymmetricMatrixObject> S_ = Args.evaluate_as<SymmetricMatrixObject>(0);
  const SymmetricMatrix& S = S_->t;

  object_ptr<const MatrixObject> R_ = Args.evaluate_as<MatrixObject>(1);
  const Matrix& R = R_->t;
    
  const unsigned N = S.size1();
  assert(S.size1() == R.size1());
  assert(S.size1() == R.size2());

  object_ptr<MatrixObject> Q_(new MatrixObject);
  Matrix& Q = Q_->t;
  Q.resize(N,N);

  for(int i=0;i<N;i++) {
    double sum=0;
    for(int j=0;j<N;j++) {
      if (i==j) continue;
      Q(i,j) = S(i,j) * R(i,j);
      sum += Q(i,j);
    }
    Q(i,i) = -sum;
  }

  return Q_;
}

/*
 * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
 * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
 * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
 * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
 *           = pi^-1.2 * exp(S2) * pi^1/2
 */

extern "C" closure builtin_function_get_eigensystem(OperationArgs& Args)
{
  object_ptr<const MatrixObject> Q_ = Args.evaluate_as<MatrixObject>(0);
  const Matrix& Q = Q_->t;

  object_ptr<const Vector<double>> pi_ = Args.evaluate_as< Vector<double> >(1);
  const vector<double>& pi = *pi_;

  const unsigned n = Q.size1();
  assert(Q.size2() == Q.size1());

#ifdef DEBUG_RATE_MATRIX
  cerr<<"scale = "<<rate()<<endl;

  assert(std::abs(sum(pi)-1.0) < 1.0e-6);
  for(int i=0;i<n;i++) {
    double sum = 0;
    for(int j=0;j<n;j++)
      sum += Q(i,j);
    assert(abs(sum) < 1.0e-6);
  }
#endif

  //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
  vector<double> sqrt_pi(n);
  vector<double> inverse_sqrt_pi(n);
  for(int i=0;i<n;i++) {
    sqrt_pi[i] = sqrt(pi[i]);
    inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];
  }

  //--------------- Calculate eigensystem -----------------//
  ublas::symmetric_matrix<double> S(n,n);
  for(int i=0;i<n;i++)
    for(int j=0;j<=i;j++) {
      S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
      // check reversibility of rate matrix
      if (i != j) {
	assert (S(i,j) >= 0);
	double p12 = Q(i,j)*pi[i];
	double p21 = Q(j,i)*pi[j];
	assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
      }
      else
	assert (Q(i,j) <= 0);
#endif
    }

  //---------------- Compute eigensystem ------------------//
  return object_ptr<const EigenValues>(new EigenValues(S));
}


extern "C" closure builtin_function_get_equilibrium_rate(OperationArgs& Args)
{
  object_ptr<const alphabet> a_ = Args.evaluate_as<alphabet>(0);
  const alphabet& a = *a_;

  object_ptr<const Vector<unsigned> > smap_ = Args.evaluate_as< Vector<unsigned> >(1);
  const vector<unsigned>& smap = smap_->t;

  object_ptr<const MatrixObject > Q_ = Args.evaluate_as< MatrixObject >(2);
  const Matrix& Q = Q_->t;

  object_ptr<const Vector<double> > pi_ = Args.evaluate_as< Vector<double> >(3);
  const vector<double> pi = pi_->t;

  assert(Q.size2() == Q.size1());
  const unsigned N = smap.size();
    
  double scale=0;

  if (N == a.size()) 
  {
    for(int i=0;i<Q.size1();i++) 
      scale -= pi[i]*Q(i,i);
  }
  else 
  {
    for(int s1=0;s1<N;s1++)
    {
      double temp = 0;
      for(int s2=0;s2<N;s2++)
	if (smap[s1] != smap[s2])
	  temp += Q(s1,s2);

      scale += temp*pi[s1];
    }
  }

  return object_ptr<const Double>(new Double(scale/a.width()));
}


extern "C" closure builtin_function_singlet_to_triplet_exchange(OperationArgs& Args)
{
  object_ptr<const Triplets> T_ = Args.evaluate_as<Triplets>(0);
  const Triplets& T = *T_;

  object_ptr<const SymmetricMatrixObject> S_ = Args.evaluate_as<SymmetricMatrixObject>(1);
  const SymmetricMatrix& S2 = S_->t;

  int N = T.size();

  object_ptr<SymmetricMatrixObject> R ( new SymmetricMatrixObject(N) );

  SymmetricMatrix& S = R->t;

  for(int i=0;i<T.size();i++)
    for(int j=0;j<i;j++) 
    {
      int nmuts=0;
      int pos=-1;
      for(int p=0;p<3;p++)
	if (T.sub_nuc(i,p) != T.sub_nuc(j,p)) {
	  nmuts++;
	  pos=p;
	}
      assert(nmuts>0);
      assert(pos >= 0 and pos < 3);
	
      S(i,j) = 0;

      if (nmuts == 1) {

	int l1 = T.sub_nuc(i,pos);
	int l2 = T.sub_nuc(j,pos);
	assert(l1 != l2);

	S(i,j) = S2(l1,l2);
      }
    }

  return R;
}
