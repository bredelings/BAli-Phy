#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include "alignment.H"
#include "tree.H"
class MatCache;
#include "matcache.H"
class Parameters;
#include "parameters.H"
#include "substitution-cache.H"

namespace substitution {

  /// Find the probabilities of all the data give each letter at the root
  vector<Matrix>
  get_column_likelihoods(const alignment& A,const Parameters& P, const vector<int>& b,
			 const vector<int>& req, const vector<int>& seq);

  Matrix get_rate_probabilities(const alignment& A,const MatCache& MC,const Tree& T,::Likelihood_Cache& cache,
				const MultiModel& MModel);

  /// Full likelihood - all columns, all rates
  double Pr(const alignment& A, const Parameters& P);

  double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes);

  double Pr(const alignment& A,const MatCache& MC,const Tree& T,::Likelihood_Cache& cache,
	    const MultiModel& MModel);
  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& LC);

  // Full likelihood - all columns, all rates (star tree)
  double Pr_star(const alignment& A,const Parameters& P);
  
  double Pr_star_constant(const alignment& A,const Parameters& P);
  double Pr_star_estimate(const alignment& A,const Parameters& P);

  // Full likelihood - if everything is unaligned....
  double Pr_unaligned(const alignment& A,const Parameters& P);

}

#endif
