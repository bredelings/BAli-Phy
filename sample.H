#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"

void change_branch_length(const alignment& A, Parameters& Theta,int b);
void change_branch_length_and_T(alignment& A, Parameters& Theta,int b);

//Resample the alignment parent->child
alignment sample_alignment(const alignment& old,const Parameters&,int b);

//Resample gap/non-gap for internal nodes, where not already determined
alignment sample_node(const alignment& old,const Parameters&,int node);

//Resample gap/non-gap for 2 internal nodes, and the topology
void sample_topology(alignment& A,Parameters&,int);

//Resample gap/non-gap for 2 internal nodes, and the topology, with specified branch lengths
bool sample_topology(alignment& A,Parameters&,const SequenceTree&, const SequenceTree&, int);

// which nodes are adjacent to this branch
vector<int> get_nodes(const alignment& A, const tree& T,int b);
#endif

/*-------------- Top Level Sampling Routines -----------*/

void sample_alignments(alignment& A, Parameters& Theta);
void sample_nodes(alignment& A, Parameters& Theta);
void sample_topologies(alignment& A,Parameters& Theta);
void change_branch_lengths(alignment& A, Parameters& Theta);
void change_parameters(alignment& A,Parameters& Theta);



