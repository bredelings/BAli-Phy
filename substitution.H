#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include "alignment.H"
#include "tree.H"
#include "parameters.H"

//FIXME - need a peel function that returns a vector< valarray<double> >

namespace substitution {

// Process half of a tree/column
std::valarray<double> peel(const vector<int>&,const tree&,const ReversibleModel&, 
			   const vector<Matrix>&,int,bool);

std::valarray<double> peel(const vector<int>&,const tree&,const ReversibleModel&, 
			   const vector<Matrix>&,int,int,int);


// Full likelihood of a column or alignment
double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);
double Pr(const alignment& A,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);


// Star likelihood of a column or alignment
// FIXME - need to get the STAR branch-lengths, non BINARY branch-lengths
//         - estimate by minimizing sum of squares from true distance?
double Pr_star(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);
double Pr_star(const alignment& A,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);
double Pr_star_constant(const alignment& A,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);
double Pr_star_estimate(const alignment& A,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);

double Pr(const alignment& A, const Parameters& P);
double Pr_star(const alignment& A,const Parameters& P);
double Pr_star_constant(const alignment& A,const Parameters& P);
double Pr_star_estimate(const alignment& A,const Parameters& P);

}

#endif
