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


// Column likelihood - specific rate
double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P);
// Full likelihood - all columns, all rates
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
