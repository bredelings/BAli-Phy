#ifndef FINDROOT_H
#define FINDROOT_H
#include "tree.H"
#include "sequencetree.H"

double rootdistance(const Tree& T,int leaf,int b,double dist);

void find_root(const Tree& T,int& rootb,double& rootd);

RootedSequenceTree find_rooted_tree(const SequenceTree& T);

#endif
