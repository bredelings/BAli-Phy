#include "alignment.H"
#include "etree.H"

std::valarray<double> peel(const alphabet&, const vector<int>&,const tree&,int,int);

double substitution(const alignment& A,const tree& T);
double substitution(const alphabet& a,const vector<int>& residues,const tree& T,
		    TreeFunc< std::valarray<double> >& distributions);
