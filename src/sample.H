#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "alignment.H"
#include "tree.H"
#include "parameters.H"
#include "mcmc.H"


// These have const A, cant be a "move"
void slide_node(const alignment& A, Parameters& P, MCMC::MoveStats& Stats, int);
void change_branch_length(const alignment&, Parameters&, MCMC::MoveStats&, int);
void change_branch_length_multi(const alignment&, Parameters&, MCMC::MoveStats&, int);

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

void walk_tree_sample_alignments(alignment&, Parameters&, MCMC::MoveStats&);
void sample_alignments_one(alignment&, Parameters&, MCMC::MoveStats&, int);
void sample_tri_one(alignment&, Parameters&, MCMC::MoveStats&, int);
void sample_tri_branch_one(alignment&, Parameters&, MCMC::MoveStats&, int);

void sample_node_move(alignment&, Parameters&, MCMC::MoveStats&, int);
void sample_two_nodes_move(alignment&, Parameters&, MCMC::MoveStats&, int);

void three_way_topology_sample(alignment&, Parameters&, MCMC::MoveStats&, int);
void two_way_topology_sample(alignment&, Parameters&, MCMC::MoveStats&, int);
void two_way_NNI_sample(alignment&, Parameters&, MCMC::MoveStats&, int);
void two_way_NNI_and_branches_sample(alignment&, Parameters&, MCMC::MoveStats&, int);
void three_way_topology_and_alignment_sample(alignment&, Parameters&, MCMC::MoveStats&, int);

void sample_SPR_flat(alignment&, Parameters&, MCMC::MoveStats&);
void sample_SPR_nodes(alignment&, Parameters&, MCMC::MoveStats&);
void slide_node_move(alignment&, Parameters&, MCMC::MoveStats&, int);
void scale_branch_lengths_and_mean(alignment&, Parameters&, MCMC::MoveStats&);
void change_branch_length_move(alignment&, Parameters&, MCMC::MoveStats&, int);
void change_branch_length_multi_move(alignment&, Parameters&, MCMC::MoveStats&, int);
void change_branch_length_and_T(alignment&, Parameters&, MCMC::MoveStats&, int);


void sample_NNI_and_branch_lengths(alignment&, Parameters&, MCMC::MoveStats&);

#endif
