/*
   Copyright (C) 2004-2010 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef SAMPLE_H
#define SAMPLE_H

#include "mytypes.H"
#include "tree.H"
#include "parameters.H"
#include "mcmc.H"


void slide_node(owned_ptr<Probability_Model>& P, MCMC::MoveStats& Stats, int);
void change_branch_length(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void slice_sample_branch_length(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void change_branch_length_multi(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);

/// Resample the alignment parent->child
void sample_alignment(Parameters&,int b);

/// Resample the 3-star alignment, holding the n2/n3 order constant.
void tri_sample_alignment(Parameters& P,int node1,int node2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_branch(Parameters&,
				 int node1,int node2,int b,double rho,double length2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_and_parameter(Parameters&,
					int node1,int node2,int p,double rho,double v2);

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
int two_way_topology_sample(std::vector<Parameters>& p,const std::vector<efloat_t>& rho,int b);


/*-------------- Top Level Sampling Routines -----------*/

void walk_tree_sample_alignments(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void walk_tree_sample_branch_lengths(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void walk_tree_sample_NNI_and_branch_lengths(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void walk_tree_sample_NNI(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void walk_tree_sample_NNI_and_A(owned_ptr<Probability_Model>&, MCMC::MoveStats&);

void sample_alignments_one(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void sample_tri_one(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void sample_tri_branch_one(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void sample_parameter_and_alignment_on_branch(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void sample_tri_branch_type_one(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);

void sample_node_move(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void sample_two_nodes_move(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);

void three_way_topology_sample(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void two_way_topology_sample(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void two_way_NNI_sample(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void two_way_NNI_and_branches_sample(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void three_way_topology_and_alignment_sample(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);

void sample_SPR_all(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void sample_SPR_search_all(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void sample_SPR_A_search_all(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void sample_SPR_flat(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void sample_SPR_nodes(owned_ptr<Probability_Model>&, MCMC::MoveStats&);
void slide_node_move(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void scale_means_only(owned_ptr<Probability_Model>&,MCMC::MoveStats&);
void change_branch_length_move(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void change_branch_length_multi_move(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void change_branch_length_and_T(owned_ptr<Probability_Model>&, MCMC::MoveStats&, int);
void change_3_branch_lengths(owned_ptr<Probability_Model>& P, MCMC::MoveStats& Stats,int); 



#endif
