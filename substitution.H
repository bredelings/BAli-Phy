#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include "alignment.H"
#include "tree.H"
#include "parameters.H"

namespace substitution {

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  std::valarray<double> peel(const vector<int>&,const tree&,const ReversibleModel&, 
			     const vector<Matrix>&,int,const std::valarray<bool>& group);

  /// Find the probabilities of each letter at parent of branch 'b', given data on one side of 'b'
  std::valarray<double> peel(const vector<int>&,const tree&,const ReversibleModel&, 
			     const vector<Matrix>&,int b,bool up);

  /// Column likelihood - for a specific rate
  double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	    const vector<Matrix>& transition_P);

  /// Full Likelihood of one column
  double Pr(const alignment& A, const Parameters& P,int);

  /// Full likelihood - all columns, all rates
  double Pr(const alignment& A, const Parameters& P);


  // Column likelihood - specific rate (star tree)
  double Pr_star(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
		 const vector<Matrix>& transition_P);
  // Full likelihood - all columns, all rates (star tree)
  double Pr_star(const alignment& A,const Parameters& P);
  
  double Pr_star_constant(const alignment& A,const Parameters& P);
  double Pr_star_estimate(const alignment& A,const Parameters& P);

}

#endif
