#ifndef FINDROOT_H
#define FINDROOT_H
#include "tree.H"

double rootdistance(const tree& T,int leaf,int b,double dist);

void find_root(const tree& T,int& rootb,double& rootd);

#endif
