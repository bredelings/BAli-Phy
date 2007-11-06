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

  inline double sum(const std::vector<double>& f,int l1,const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l1))
	total += f[l];
    return total;
  }

  inline double sum(const std::valarray<double>& f,int l1,const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l1))
	total += f[l];
    return total;
  }

  inline double sum(const Matrix Q,int l1, int l2, const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l2))
	total += Q(l1,l);
    return total;
  }

  /// Find the probabilities of all the data give each letter at the root
  vector<Matrix>
  get_column_likelihoods(const data_partition&, const vector<int>& b,
			 const vector<int>& req, const vector<int>& seq,int delta=0);

  Matrix get_rate_probabilities(const alignment& A,const MatCache& MC,const Tree& T,::Likelihood_Cache& cache,
				const MultiModel& MModel);

  /// Full likelihood - all columns, all rates
  efloat_t Pr(const data_partition&);

  efloat_t other_subst(const data_partition&, const vector<int>& nodes);
  
  efloat_t Pr(const alignment& A,const MatCache& MC,const Tree& T,::Likelihood_Cache& cache,
	    const MultiModel& MModel);
  efloat_t Pr(const data_partition&,Likelihood_Cache& LC);

  // Full likelihood - all columns, all rates (star tree)
  efloat_t Pr_star(const data_partition&);

  // Full likelihood - if everything is unaligned....
  efloat_t Pr_unaligned(const data_partition&);

  // Full likelihood of the single sequence with the lowest likelihood
  efloat_t Pr_single_sequence(const data_partition&);
}

#endif
