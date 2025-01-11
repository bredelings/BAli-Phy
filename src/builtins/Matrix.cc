#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "math/exponential.H"
#include "math/eigenvalue.H"
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/io.H"
#include <valarray>
#include "dp/2way.H"
#include "util/range.H"
#include "substitution/parsimony.H"

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_nrows(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const Matrix& m = arg0.as_<Box<Matrix>>();

    int n1 = m.size1();

    return { n1 } ;
}

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_ncols(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const Matrix& m = arg0.as_<Box<Matrix>>();

    int n2 = m.size2();

    return { n2 } ;
}

// scaleMatrix :: a -> Matrix a -> Matrix a
extern "C" closure builtin_function_scaleMatrix(OperationArgs& Args)
{
    double factor = Args.evaluate(0).as_double();;

    auto arg2 = Args.evaluate(1);
    const Matrix& m = arg2.as_<Box<Matrix>>();

    int n1 = m.size1();
    int n2 = m.size2();

    auto m2 = new Box<Matrix>(n1,n2);
    for(int i=0;i<n1;i++)
	for(int j=0;j<n2;j++)
	    (*m2)(i,j) = factor * m(i,j);

    return m2;
}

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_elementwise_multiply(OperationArgs& Args)
{
    auto arg1 = Args.evaluate(0);
    const Matrix& m1 = arg1.as_<Box<Matrix>>();

    auto arg2 = Args.evaluate(1);
    const Matrix& m2 = arg2.as_<Box<Matrix>>();

    int n1 = m1.size1();
    int n2 = m1.size2();

    if (m2.size1() != n1 or m2.size2() != n2)
	throw myexception()<<"Trying to multiply matrices of unequal sizes ("<<n1<<","<<n2<<") and ("<<m2.size1()<<","<<m2.size2()<<") elementwise";

    auto m3 = new Box<Matrix>(n1,n2);
    for(int i=0;i<n1;i++)
	for(int j=0;j<n2;j++)
	    (*m3)(i,j) = m1(i,j) * m2(i,j);

    return m3;
}

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_elementwise_add(OperationArgs& Args)
{
    auto arg1 = Args.evaluate(0);
    const Matrix& m1 = arg1.as_<Box<Matrix>>();

    auto arg2 = Args.evaluate(1);
    const Matrix& m2 = arg2.as_<Box<Matrix>>();

    int n1 = m1.size1();
    int n2 = m1.size2();

    if (m2.size1() != n1 or m2.size2() != n2)
	throw myexception()<<"Trying to add matrices of unequal sizes ("<<n1<<","<<n2<<") and ("<<m2.size1()<<","<<m2.size2()<<") elementwise";

    auto m3 = new Box<Matrix>(n1,n2);
    for(int i=0;i<n1;i++)
	for(int j=0;j<n2;j++)
	    (*m3)(i,j) = m1(i,j) + m2(i,j);

    return m3;
}


// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_zero(OperationArgs& Args)
{
    int n1 = Args.evaluate(0).as_int();
    int n2 = Args.evaluate(1).as_int();

    auto m = new Box<Matrix>(n1, n2);
    for(int i=0; i<n1; i++)
	for(int j=0; j<n2; j++)
	    (*m)(i,j) = 0;

    return m;
}

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_identity(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    auto m = new Box<Matrix>(n, n);
    for(int i=0;i<n;i++)
	for(int j=0;j<n;j++)
	    (*m)(i,j) = (i==j)?1:0;

    return m;
}

#include <unsupported/Eigen/MatrixFunctions>

extern "C" closure builtin_function_MatrixExp(OperationArgs& Args)
{
    using Eigen::Map;
    using Eigen::Dynamic;
    using Eigen::RowMajor;

    auto arg0 = Args.evaluate(0);
    auto& Q = arg0.as_<Box<Matrix>>();
    int n = Q.size1();
    assert(Q.size2() == n);

    double t = Args.evaluate(1).as_double();

    auto P = new Box<Matrix>(n,n);

    // Using Map<.., Eigen::Aligned> gives a small (0.2%) speedup with codon alphabets.
    // But ensuring alignment is messy on windows.
    Map<const Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> EQ(Q.begin(), n, n);
    Map<Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> EP(P->begin(), n, n);

    EP = (t*EQ).exp();

    for(int i=0; i< n;i++)
    {
        double sum = 0;
        for(int j=0; j< n;j++)
        {
            EP(i,j) = std::max(EP(i,j),0.0);
            sum += EP(i,j);
        }
        for(int j=0; j< n;j++)
            EP(i,j)/= sum;
    }
    
    return P;
}



extern "C" closure builtin_function_lExpRaw(OperationArgs& Args)
{
    auto L = Args.evaluate(0);
    auto pi = (vector<double>) Args.evaluate(1).as_<EVector>();
    double t = Args.evaluate(2).as_double();

    object_ptr<Box<Matrix>> M = new Box<Matrix>;
    *M = exp(L.as_<Box<EigenValues>>(), pi, t);
    return {EMaybe(M)};
}



/*
 * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
 * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
 * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
 * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
 *           = pi^-1.2 * exp(S2) * pi^1/2
 */

extern "C" closure builtin_function_getEigensystemRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const Matrix& Q = arg0.as_< Box<Matrix> >();

    auto pi = vector<double>(Args.evaluate(1).as_<EVector>() );

    const unsigned n = Q.size1();
    assert(Q.size2() == Q.size1());

#ifdef DEBUG_RATE_MATRIX
    assert(std::abs(sum(pi)-1.0) < 1.0e-6);
    for(int i=0;i<n;i++) {
	double sum = 0;
	for(int j=0;j<n;j++)
	    sum += Q(i,j);
	assert(abs(sum) < 1.0e-6);
    }
#endif

    //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
    vector<double> sqrt_pi(n, 1.0);
    vector<double> inverse_sqrt_pi(n, 1.0);
    for(int i=0;i<n;i++) {
        if (pi[i] > 1.0e-13)
        {
            sqrt_pi[i] = sqrt(pi[i]);
            inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];
        }
    }

    //--------------- Calculate eigensystem -----------------//
    Matrix S(n,n);
    for(int i=0;i<n;i++)
	for(int j=0;j<=i;j++) {
	    S(j,i) = S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
	    // check reversibility of rate matrix
	    if (i != j) {
		assert (S(i,j) >= 0);
		double p12 = Q(i,j)*pi[i];
		double p21 = Q(j,i)*pi[j];
		assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
		if (i > j)
		    assert( abs(S(i,j) - S(j,i)) < 1.0e-13 );
	    }
	    else
		assert (Q(i,j) <= 0);
#endif
	}

    //---------------- Compute eigensystem ------------------//
    expression_ref E(new Box<EigenValues>(S));
    return {EMaybe(E)};
}



extern "C" closure builtin_function_transpose(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& M1 = arg0.as_<Box<Matrix>>();

    auto M2p = new Box<Matrix>(M1.size2(), M1.size1());
    auto& M2 = *M2p;
    for(int i=0;i<M2.size1();i++)
        for(int j=0;j<M2.size2();j++)
            M2(i,j) = M1(j,i);

    return M2p;
}
