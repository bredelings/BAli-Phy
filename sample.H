#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"

//Resample the alignment parent->child
alignment sample(const alignment& old,const Parameters&,int node1, int node2);

//Resample gap/non-gap for internal nodes, where not already determined
alignment sample(const alignment& old,const Parameters&,int node);

//interface to the previous samplers
alignment sample(const alignment& old,const Parameters& T);

#endif
