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
// FIXME - need to pick a root and get branch lengths by summing from that!
double substitution_star(const vector<int>&,const Parameters&);
double substitution_star(const alignment& A,const Parameters&);
