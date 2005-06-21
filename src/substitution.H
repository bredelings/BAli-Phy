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
			 const vector<int>& req, const vector<int>& seq,int delta=0);

  Matrix get_rate_probabilities(const alignment& A,const MatCache& MC,const Tree& T,::Likelihood_Cache& cache,
				const MultiModel& MModel);

  /// Full likelihood - all columns, all rates
  efloat_t Pr(const alignment& A, const Parameters& P);

  efloat_t other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes);
  
  efloat_t Pr(const alignment& A,const MatCache& MC,const Tree& T,::Likelihood_Cache& cache,
	    const MultiModel& MModel);
  efloat_t Pr(const alignment& A, const Parameters& P,Likelihood_Cache& LC);

  // Full likelihood - all columns, all rates (star tree)
  efloat_t Pr_star(const alignment& A,const Parameters& P);
  
  efloat_t Pr_star_constant(const alignment& A,const Parameters& P);
  efloat_t Pr_star_estimate(const alignment& A,const Parameters& P);

  // Full likelihood - if everything is unaligned....
  efloat_t Pr_unaligned(const alignment& A,const Parameters& P);

  // Full likelihood of the single sequence with the lowest likelihood
  efloat_t Pr_single_sequence(const alignment& A,const Parameters& P);
}

#endif
