#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"
#include "mcmc.H"

MCMC::result_t slide_branch_length(const alignment& A, Parameters& Theta,int b,bool up);

MCMC::result_t change_branch_length(const alignment& A, Parameters& Theta,int b);


// Resample the 3-star alignment, holding the 2/3 order constant.
alignment tri_sample_alignment(const alignment& old,const Parameters& Theta,
			       int node1,int node2);

//Resample the alignment parent->child
alignment sample_alignment(const alignment& old,const Parameters&,int b);

//Resample the alignment parent->child
alignment sample_alignment2(const alignment& old,const Parameters&,int b);

//Resample gap/non-gap for internal nodes, where not already determined
alignment sample_node(const alignment& old,const Parameters&,int node);
//Resample gap/non-gap for internal nodes, where not already determined
alignment sample_node2(const alignment& old,const Parameters&,int node);

//Resample gap/non-gap for 2 internal nodes, and the topology, with specified branch lengths
MCMC::result_t sample_topology(alignment& A,Parameters&,const SequenceTree&, const SequenceTree&, int);

// which nodes are adjacent to this branch
vector<int> get_nodes(const alignment& A, const tree& T,int b);

/*-------------- Top Level Sampling Routines -----------*/

//Resample the alignment parent->child
//MCMC::result_t sample_alignments(alignment& A, Parameters& Theta);
MCMC::result_t sample_alignments_one(alignment& old,Parameters&,int b);
MCMC::result_t sample_alignments2_one(alignment& old,Parameters&,int b);

//MCMC::result_t sample_nodes(alignment& A, Parameters& Theta);
MCMC::result_t sample_nodes_one(alignment& A, Parameters& Theta,int);
MCMC::result_t sample_nodes2_one(alignment& A, Parameters& Theta,int);

//MCMC::result_t sample_topologies(alignment& A,Parameters& Theta);
MCMC::result_t sample_topology(alignment& A,Parameters&,int);

//MCMC::result_t change_branch_lengths(alignment& A, Parameters& Theta);
MCMC::result_t change_branch_length_move(alignment& A, Parameters& Theta,int b);
MCMC::result_t slide_branch_lengths_one(alignment& A, Parameters& Theta,int);
MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& Theta,int b);

//MCMC::result_t slide_branch_lengths(alignment& A, Parameters& Theta);

MCMC::result_t change_parameters(alignment& A,Parameters& Theta);

//MCMC::result_t sample_tri(alignment&, Parameters&);
MCMC::result_t sample_tri_one(alignment&, Parameters&,int);

#endif
