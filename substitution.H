#include "alignment.H"
#include "tree.H"
#include "parameters.H"

// Process half of a tree/column
std::valarray<double> peel(const vector<int>&,const Parameters&,int,bool);
std::valarray<double> peel(const vector<int>&,const Parameters&,int,int,int);


// Process a whole tree/column
double substitution(const vector<int>&,const Parameters&);

// Process all the columns in A
double substitution(const alignment& A,const Parameters&);

// Process a whole tree/column - star tree
double substitution_star(const vector<int>&,const Parameters&);

// Process a whole tree/column - star tree
double substitution_star(const alignment& A,const Parameters&);
