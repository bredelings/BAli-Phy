#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"
#include "mcmc.H"


// These have const A, cant be a "move"
MCMC::result_t slide_branch_length(const alignment& A, Parameters& Theta,int b,bool up);
MCMC::result_t change_branch_length(const alignment& A, Parameters& Theta,int b);


/// Resample the 3-star alignment, holding the n2/n3 order constant.
alignment tri_sample_alignment(const alignment& old,const Parameters& Theta,int node1,int node2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_branch(alignment&,Parameters&,
				 int node1,int node2,int b,double length2);

/// Resample the alignment parent->child
alignment sample_alignment(const alignment& old,const Parameters&,int b);

/// Resample gap/non-gap for internal nodes, where not already determined
alignment sample_node(const alignment& old,const Parameters&,int node);

/// Resample gap/non-gap for 2 adjacent internal nodes, where not already determined
alignment sample_two_nodes(const alignment& old,const Parameters& P,int b);

/// Resample between 3 NNI topologies around branch b
bool three_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,const Parameters& P3,int b);

/// Resample between 2 NNI topologies around branch b
bool two_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,int b);
bool sample_two_NNI_two_nodes_MH(alignment& A,Parameters& P1,const Parameters& P2,int b,double rho);

/*-------------- Top Level Sampling Routines -----------*/

//Resample the alignment parent->child
MCMC::result_t sample_alignments_one(alignment& old,Parameters&,int b);
MCMC::result_t sample_alignments2_one(alignment& old,Parameters&,int b);

MCMC::result_t sample_node_move(alignment& A, Parameters& Theta,int);
MCMC::result_t sample_two_nodes_move(alignment& A, Parameters& Theta,int);

MCMC::result_t three_way_topology_sample(alignment& A,Parameters& P,int b);
MCMC::result_t two_way_topology_sample(alignment& A,Parameters& P,int b);
MCMC::result_t two_way_topology_sample_MH(alignment& A,Parameters& P,int b);
MCMC::result_t two_way_topology_sample2(alignment& A,Parameters& P,int b);
MCMC::result_t three_way_topology_and_alignment_sample(alignment& A,Parameters& P,int b);

MCMC::result_t sample_SPR(alignment& A,Parameters& P,int b);
MCMC::result_t sample_SPR_and_A(alignment& A,Parameters& P,int b);

MCMC::result_t change_branch_length_move(alignment& A, Parameters& Theta,int b);
MCMC::result_t slide_branch_lengths_one(alignment& A, Parameters& Theta,int);
MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& Theta,int b);

MCMC::result_t change_parameters(alignment& A,Parameters& Theta);
MCMC::result_t change_gap_parameters(alignment& A,Parameters& Theta);

MCMC::result_t sample_tri_one(alignment&, Parameters&,int);
MCMC::result_t sample_tri_branch_one(alignment&,Parameters&,int);

#endif
