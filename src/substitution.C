#include "substitution.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>

// recalculate a likelihood immediate afterwards, and see if we get the same answer...
// perhaps move the collection root node one branch away?
// then we have to do re-validation...

using std::valarray;
using std::vector;

// This file assumes that 
// * the matrix is reversible.  This means that we evaluate
//   frequencies at the root - even for insertions, where they actually
//   apply somewhere down the tree.
//
// * we don't need to work in log space for a single column
//
// * 
namespace substitution {


  struct peeling_info: public vector<int> {
    peeling_info(const Tree&T) { reserve(T.n_branches()); }
  };

  typedef Likelihood_Cache& column_cache_t;

  /// compute log(probability) from conditional likelihoods (S) and equilibrium frequencies as in MModel
  double Pr(const Matrix& S,const MultiModel& MModel) {
    return 0.0;
  }


  double calc_root_probability(const alignment& A, const Parameters& P,const vector<int>& rb,
			       const ublas::matrix<int>& index) 
  {
    return 0.0;
  }

  void calculate_caches(const alignment& A, const Parameters& P,column_cache_t cache) {
  }

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const alignment& A,const Parameters& P, const vector<int>& b,
			 const vector<int>& req,const vector<int>& seq)
  {
    vector<Matrix> L;
    L.reserve(A.length());

    Matrix S(P.SModel().n_base_models(), A.get_alphabet().size() );
    for(int i=0;i<S.size1();i++)
      for(int j=0;j<S.size2();j++)
	S(i,j) = 1.0;

    for(int i=0;i<seq.size();i++)
      L.push_back(S);
    return L;
  }

  double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes) 
  {
    return 0.0;
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& cache) {
    return 0.0;
  }

  double Pr(const alignment& A,const Parameters& P) {
    return 0.0;
  }
}
