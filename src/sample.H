#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"
#include "mcmc.H"


// These have const A, cant be a "move"
MCMC::result_t change_branch_length(const alignment& A, Parameters& Theta,int b);
MCMC::result_t change_branch_length_multi(const alignment& A, Parameters& P,int b);


/// Resample the alignment parent->child
void sample_alignment(alignment& A,Parameters&,int b);

/// Resample the 3-star alignment, holding the n2/n3 order constant.
void tri_sample_alignment(alignment& A,Parameters& P,int node1,int node2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_branch(alignment&,Parameters&,
				 int node1,int node2,int b,double rho,double length2);

/// Resample gap/non-gap for internal nodes, where not already determined
void sample_node(alignment& A,Parameters&,int node);

/// Resample gap/non-gap for 2 adjacent internal nodes, where not already determined
void sample_two_nodes(alignment& A,Parameters& P,int b);

/// Resample between 3 NNI topologies around branch b
bool three_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,const Parameters& P3,int b);

/// Resample between 2 NNI topologies around branch b
int two_way_topology_sample(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho,int b);

/*-------------- Top Level Sampling Routines -----------*/

//Resample the alignment parent->child
MCMC::result_t sample_alignments_one(alignment& old,Parameters&,int b);
MCMC::result_t sample_tri_one(alignment&, Parameters&,int);
MCMC::result_t sample_tri_branch_one(alignment&,Parameters&,int);
MCMC::result_t walk_tree_sample_alignments(alignment& A,Parameters& P);

MCMC::result_t sample_node_move(alignment& A, Parameters& Theta,int);
MCMC::result_t sample_two_nodes_move(alignment& A, Parameters& Theta,int);

MCMC::result_t three_way_topology_sample(alignment& A,Parameters& P,int b);
MCMC::result_t two_way_topology_sample(alignment& A,Parameters& P,int b);
MCMC::result_t two_way_topology_sample_MH(alignment& A,Parameters& P,int b);
MCMC::result_t three_way_topology_and_alignment_sample(alignment& A,Parameters& P,int b);

MCMC::result_t sample_SPR(alignment& A,Parameters& P,int b);

MCMC::result_t change_branch_length_move(alignment& A, Parameters& Theta,int b);
MCMC::result_t change_branch_length_multi_move(alignment& A, Parameters& Theta,int b);
MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& Theta,int b);

MCMC::result_t change_parameters(alignment& A,Parameters& Theta);
MCMC::result_t change_gap_parameters(alignment& A,Parameters& Theta);

MCMC::result_t sample_frequencies(alignment& A,Parameters& P);

MCMC::result_t sample_NNI_and_branch_lengths(alignment& A,Parameters& P);

#endif
