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

#include "tree/tree.H"
#include "models/parameters.H"
#include "result.H"
#include "sample-alignment.H"  // log_double_t sample_alignment(Parameters&,int b,bool);

void slide_node(owned_ptr<context>& P, MCMC::MoveStats& Stats, int);
void change_branch_length(owned_ptr<context>&, MCMC::MoveStats&, int);
void slice_sample_branch_length(owned_ptr<context>&, MCMC::MoveStats&, int);
void slice_sample_node_time(owned_ptr<context>&, MCMC::MoveStats&, int);
void alignment_slice_sample_branch_length(owned_ptr<context>&, MCMC::MoveStats&, int);
void change_branch_length_multi(owned_ptr<context>&, MCMC::MoveStats&, int);

/// Resample the 3-star alignment, holding the n2/n3 order constant.
void tri_sample_alignment(Parameters& P,int node1,int node2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_branch(Parameters&,
				 int node1,int node2,int b,double rho,double length2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool tri_sample_alignment_and_parameter(Parameters&, int node1,int node2, const Proposal&);

/// Resample the 3-star alignment and branch TYPE, holding the n2/n3 order constant.
bool tri_sample_alignment_branch_model(Parameters&,
				       int node1,int node2);

/// Resample the 3-star alignment, holding the n2/n3 order constant.
void cube_sample_alignment(Parameters& P,int node1,int node2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool cube_sample_alignment_branch(Parameters&,
				 int node1,int node2,int b,double rho,double length2);

/// Resample the 3-star alignment and branch length, holding the n2/n3 order constant.
bool cube_sample_alignment_and_parameter(Parameters&,
					int node1,int node2,int p,double rho,double v2);

/// Resample the 3-star alignment and branch TYPE, holding the n2/n3 order constant.
bool cube_sample_alignment_branch_model(Parameters&,
				       int node1,int node2);

/// Resample gap/non-gap for internal nodes, where not already determined
void sample_node(Parameters&,int node);

/// Resample gap/non-gap for 2 adjacent internal nodes, where not already determined
void sample_A5(Parameters& P,int b);

/// Resample between 3 NNI topologies around branch b
bool three_way_topology_sample(Parameters& P1,const Parameters& P2,const Parameters& P3,int b);

/// Resample between 2 NNI topologies around branch b
int two_way_topology_sample(std::vector<Parameters>& p,const std::vector<log_double_t>& rho,int b);


/*-------------- Top Level Sampling Routines -----------*/
std::vector<int> walk_tree_path_toward(const TreeInterface& t, int node);
std::vector<int> walk_tree_path_away(const TreeInterface& t, int node);
std::vector<int> walk_tree_path_toward_and_away(const TreeInterface& t, int node);
std::vector<int> walk_tree_path(const TreeInterface& t, int root);

void walk_tree_sample_alignments(owned_ptr<context>&, MCMC::MoveStats&);
void walk_tree_sample_branch_lengths(owned_ptr<context>&, MCMC::MoveStats&);
void walk_tree_sample_NNI_and_branch_lengths(owned_ptr<context>&, MCMC::MoveStats&);
void walk_time_tree_sample_NNI_and_node_times(owned_ptr<context>&, MCMC::MoveStats&);
void walk_tree_sample_NNI(owned_ptr<context>&, MCMC::MoveStats&);
void walk_tree_sample_NNI_and_A(owned_ptr<context>&, MCMC::MoveStats&);
void realign_from_tips(owned_ptr<context>&, MCMC::MoveStats&);

void sample_alignments_one(owned_ptr<context>&, MCMC::MoveStats&, int);
void sample_tri_one(owned_ptr<context>&, MCMC::MoveStats&, int);
void sample_tri_branch_one(owned_ptr<context>&, MCMC::MoveStats&, int);
void sample_cube_one(owned_ptr<context>&, MCMC::MoveStats&, int);
void sample_cube_branch_one(owned_ptr<context>&, MCMC::MoveStats&, int);
void sample_parameter_and_alignment_on_branch(owned_ptr<context>&, MCMC::MoveStats&, int, const Proposal&);
void sample_tri_branch_type_one(owned_ptr<context>&, MCMC::MoveStats&, int);

void sample_node_move(owned_ptr<context>&, MCMC::MoveStats&, int);
void sample_A5_move(owned_ptr<context>&, MCMC::MoveStats&, int);

void three_way_topology_sample(owned_ptr<context>&, MCMC::MoveStats&, int);
void three_way_time_tree_NNI_sample(owned_ptr<context>&, MCMC::MoveStats&, int);
void two_way_topology_sample(owned_ptr<context>&, MCMC::MoveStats&, int);
void two_way_NNI_sample(owned_ptr<context>&, MCMC::MoveStats&, int);
void two_way_NNI_and_branches_sample(owned_ptr<context>&, MCMC::MoveStats&, int);
void three_way_topology_and_alignment_sample(owned_ptr<context>&, MCMC::MoveStats&, int, std::optional<int> bandwidth = {});

void sample_SPR_all(owned_ptr<context>&, MCMC::MoveStats&);
void sample_SPR_search_all(owned_ptr<context>&, MCMC::MoveStats&);
void sample_SPR_A_search_all(owned_ptr<context>&, MCMC::MoveStats&);
void sample_SPR_flat(owned_ptr<context>&, MCMC::MoveStats&);
void sample_SPR_nodes(owned_ptr<context>&, MCMC::MoveStats&);
void slide_node_move(owned_ptr<context>&, MCMC::MoveStats&, int);
void change_branch_length_move(owned_ptr<context>&, MCMC::MoveStats&, int);
void change_branch_length_multi_move(owned_ptr<context>&, MCMC::MoveStats&, int);
void change_branch_length_and_T(owned_ptr<context>&, MCMC::MoveStats&, int);
void change_3_branch_lengths(owned_ptr<context>& P, MCMC::MoveStats& Stats,int); 



#endif
