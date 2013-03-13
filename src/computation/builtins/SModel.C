#include "computation/computation.H"
#include "math/exponential.H"

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

