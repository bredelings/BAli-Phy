#ifndef JAMA_CHOLESKY_H
#define JAMA_CHOLESKY_H

#include "math.h"
	/* needed for sqrt() below. */


namespace JAMA
{

/** 
   <P>
   For a symmetric, positive definite matrix A, this function
   computes the Cholesky factorization, i.e. it computes a lower 
   triangular matrix L such that A = L*L'.
   If the matrix is not symmetric or positive definite, the function
   computes only a partial decomposition.  This can be tested with
   the is_spd() flag.

   <p>Typical usage looks like:
   <pre>
	Array2D<double> A(n,n);
	Array2D<double> L;

	 ... 

	Cholesky<double> chol(A);

	if (chol.is_spd())
		L = chol.getL();
		
  	else
		cout << "facotrization was not complete.\n";

	</pre>


   <p>
	(Adapted from JAMA, a Java Matrix Library, developed by jointly 
	by the Mathworks and NIST; see  http://math.nist.gov/javanumerics/jama).

   */

template <class Real>
class Cholesky
{
	TNT::Array2D<Real> L;		// lower triangular factor
	int isspd;				// 1 if matrix to be factored was SPD


public:

	Cholesky();
	Cholesky(const TNT::Array2D<Real> &A);
	TNT::Array2D<Real> getL() const;
	int is_spd() const;

};

template <class Real>
Cholesky<Real>::Cholesky() : L(0,0), isspd(0) {}

/**
	@return 1, if original matrix to be factored was symmetric 
		positive-definite (SPD).
*/
template <class Real>
int Cholesky<Real>::is_spd() const
{
	return isspd;
}

/**
	@return the lower triangular factor, L, such that L*L'=A.
*/
template <class Real>
TNT::Array2D<Real> Cholesky<Real>::getL() const
{
	return L;
}

/**
	Constructs a lower triangular matrix L, such that L*L'= A.
	If A is not symmetric positive-definite (SPD), only a
	partial factorization is performed.  If is_spd()
	evalutate true (1) then the factorizaiton was successful.
*/
template <class Real>
Cholesky<Real>::Cholesky(const TNT::Array2D<Real> &A)
{


   	int m = A.dim1();
	int n = A.dim2();
	
	isspd = (m == n);

	if (m != n)
	{
		L = TNT::Array2D<Real>(0,0);
		return;
	}

	L = TNT::Array2D<Real>(n,n);


      // Main loop.
     for (int j = 0; j < n; j++) 
	 {
        double d = 0.0;
        for (int k = 0; k < j; k++) 
		{
            Real s = 0.0;
            for (int i = 0; i < k; i++) 
			{
               s += L[k][i]*L[j][i];
            }
            L[j][k] = s = (A[j][k] - s)/L[k][k];
            d = d + s*s;
            isspd = isspd && (A[k][j] == A[j][k]); 
         }
         d = A[j][j] - d;
         isspd = isspd && (d > 0.0);
         L[j][j] = sqrt(d > 0.0 ? d : 0.0);
         for (int k = j+1; k < n; k++) 
		 {
            L[j][k] = 0.0;
         }
	}
}

}
// namespace JAMA

#endif
// JAMA_CHOLESKY_H
