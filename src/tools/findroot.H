#ifndef FINDROOT_H
#define FINDROOT_H
#include "tree.H"

double rootdistance(const Tree& T,int leaf,int b,double dist);

void find_root(const Tree& T,int& rootb,double& rootd);

#endif
