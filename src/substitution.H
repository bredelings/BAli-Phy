#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include "alignment.H"
#include "tree.H"
class MatCache;
#include "matcache.H"
class Parameters;
#include "parameters.H"

namespace substitution {

  /// Find the probabilities of all the data give each letter at the root
  std::valarray<double> get_column_likelihoods(const vector<int>&,const Tree&,const ReversibleModel&, 
			     const vector<Matrix>&,int);

  /// Find the probabilities of the data in 'group' given each letter at the root
  std::valarray<double> get_column_likelihoods(      vector<int> ,const Tree&,const ReversibleModel&, 
			     const vector<Matrix>&,int,const std::valarray<bool>& group);

  /// Full Likelihood of one column
  double Pr(const alignment& A, const Tree& T, const MultiModel& SModel, const MatCache& SM,int);

  /// Full Likelihood - all columns, all rates
  double Pr(const alignment& A, const Tree& T, const MultiModel& SModel, const MatCache& MC);

  /// Full likelihood - all columns, all rates
  double Pr(const alignment& A, const Parameters& P);


  // Column likelihood - specific rate (star tree)
  double Pr_star(const vector<int>& residues,const Tree& T,const ReversibleModel& SModel,
		 const vector<Matrix>& transition_P);
  // Full likelihood - all columns, all rates (star tree)
  double Pr_star(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC);

  // Full likelihood - all columns, all rates (star tree)
  double Pr_star(const alignment& A,const Parameters& P);
  
  double Pr_star_constant(const alignment& A,const Parameters& P);
  double Pr_star_estimate(const alignment& A,const Parameters& P);

  // Full likelihood - if everything is unaligned....
  double Pr_unaligned(const alignment& A,const Parameters& P);

}

#endif
