#include "alignment.H"
#include "tree.H"
#include "parameters.H"

std::valarray<double> peel(const vector<int>&,const Parameters&,int,int);

double substitution(const alignment& A,const Parameters& Theta);
double substitution(const vector<int>& residues,const Parameters& Theta,
		    TreeFunc< std::valarray<double> >& distributions);
