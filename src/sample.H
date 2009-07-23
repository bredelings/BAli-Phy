#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "tree.H"
#include "parameters.H"
#include "mcmc.H"


// These have const A, cant be a "move"
void slide_node(Parameters& P, MCMC::MoveStats& Stats, int);
void change_branch_length(Parameters&, MCMC::MoveStats&, int);
void slice_sample_branch_length(Parameters&, MCMC::MoveStats&, int);
void change_branch_length_multi(Parameters&, MCMC::MoveStats&, int);

/// Resample the alignment parent->child
void sample_alignment(Parameters&,int b);

/// Resample the 3-star alignment, holding the n2/n3 order constant.
void tri_sample_alignment(Parameters& P,int node1,int node2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_branch(Parameters&,
				 int node1,int node2,int b,double rho,double length2);

/// Resample the 3-star alignment and branch TYPE, holding the n2/n3 order constant.
bool tri_sample_alignment_branch_model(Parameters&,
				       int node1,int node2);

/// Resample gap/non-gap for internal nodes, where not already determined
void sample_node(Parameters&,int node);

/// Resample gap/non-gap for 2 adjacent internal nodes, where not already determined
void sample_two_nodes(Parameters& P,int b);

/// Resample between 3 NNI topologies around branch b
bool three_way_topology_sample(Parameters& P1,const Parameters& P2,const Parameters& P3,int b);

/// Resample between 2 NNI topologies around branch b
int two_way_topology_sample(vector<Parameters>& p,const vector<efloat_t>& rho,int b);


/*-------------- Top Level Sampling Routines -----------*/

void walk_tree_sample_alignments(Parameters&, MCMC::MoveStats&);
void walk_tree_sample_branch_lengths(Parameters&, MCMC::MoveStats&);
void sample_alignments_one(Parameters&, MCMC::MoveStats&, int);
void sample_tri_one(Parameters&, MCMC::MoveStats&, int);
void sample_tri_branch_one(Parameters&, MCMC::MoveStats&, int);
void sample_tri_branch_type_one(Parameters&, MCMC::MoveStats&, int);

void sample_node_move(Parameters&, MCMC::MoveStats&, int);
void sample_two_nodes_move(Parameters&, MCMC::MoveStats&, int);

void three_way_topology_sample(Parameters&, MCMC::MoveStats&, int);
void two_way_topology_sample(Parameters&, MCMC::MoveStats&, int);
void two_way_NNI_sample(Parameters&, MCMC::MoveStats&, int);
void two_way_NNI_and_branches_sample(Parameters&, MCMC::MoveStats&, int);
void three_way_topology_and_alignment_sample(Parameters&, MCMC::MoveStats&, int);

void sample_SPR_flat(Parameters&, MCMC::MoveStats&);
void sample_SPR_nodes(Parameters&, MCMC::MoveStats&);
void slide_node_move(Parameters&, MCMC::MoveStats&, int);
void scale_means_only(Parameters&,MCMC::MoveStats&);
void change_branch_length_move(Parameters&, MCMC::MoveStats&, int);
void change_branch_length_multi_move(Parameters&, MCMC::MoveStats&, int);
void change_branch_length_and_T(Parameters&, MCMC::MoveStats&, int);
void change_3_branch_lengths(Parameters& P, MCMC::MoveStats& Stats,int); 


void sample_NNI_and_branch_lengths(Parameters&, MCMC::MoveStats&);

void sample_alignment_rates(Parameters&, MCMC::MoveStats&);

#endif
