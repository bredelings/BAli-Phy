#include "alignment.H"
#include "tree.H"
#include "parameters.H"

// Process half of a tree/column
std::valarray<double> peel(const vector<int>&,const Parameters&,int,bool);
std::valarray<double> peel(const vector<int>&,const Parameters&,int,int,int);


// Full likelihood of a column or alignment
double substitution(const vector<int>&,const Parameters&);
double substitution(const alignment& A,const Parameters&);


// Star likelihood of a column or alignment
// FIXME - need to get the STAR branch-lengths, non BINARY branch-lengths
//         - estimate by minimizing sum of squares from true distance?
double substitution_star(const vector<int>&,const Parameters&);
double substitution_star(const alignment& A,const Parameters&);
double substitution_star_constant(const alignment& A,const Parameters&);
double substitution_star_estimate(const alignment& A,const Parameters&);
