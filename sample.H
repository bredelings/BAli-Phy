#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"

//Resample the alignment parent->child
alignment sample_alignment(const alignment& old,const Parameters&,int b);

//Resample gap/non-gap for internal nodes, where not already determined
alignment sample_node(const alignment& old,const Parameters&,int node);

//Resample gap/non-gap for 2 internal nodes, and the topology
void sample_topology(alignment& A,Parameters&,int);

//Resample gap/non-gap for 2 internal nodes, and the topology, with specified branch lengths
void sample_topology(alignment& A,Parameters&,const SequenceTree&, const SequenceTree&, int);

//interface to the above samplers
void sample(alignment& A,Parameters& T);

// which nodes are adjacent to this branch
vector<int> get_nodes(const alignment& A, const tree& T,int b);
#endif
