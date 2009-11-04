/*
   Copyright (C) 2004-2005,2007,2009 Benjamin Redelings

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

#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>

#include "mytypes.H"
#include "dp-engine.H"
#include "alignment.H"
#include "parameters.H"
#include <boost/dynamic_bitset.hpp>

/// Define type for a function which return the distributions for each column and rate give SOME leaves
typedef vector< Matrix > (*distributions_t)(const data_partition&,const vector<int>&,int,const boost::dynamic_bitset<>&);


/// Distributions function for a star tree
vector< Matrix > distributions_star(const data_partition& P,const vector<int>& seq,int root,const boost::dynamic_bitset<>& group);

/// Distributions function for a full tree
vector< Matrix > distributions_tree(const data_partition& P,const vector<int>& seq,int root,const boost::dynamic_bitset<>& group);


/// Sum of likelihoods for columns which don't contain any characters in sequences mentioned in 'nodes'
efloat_t other_subst(const data_partition& P, const vector<int>& nodes);

/// Sum of branch priors for branches not connected to no
efloat_t other_prior(const data_partition& P, const vector<int>& nodes);


/// Routine for simultaneously sampling between several Parameter choices, and summing out a node
int sample_node_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		      const vector<efloat_t>& rho, bool do_OS,bool do_OP);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_two_nodes_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
			   const vector<efloat_t>& rho, bool do_OS,bool do_OP);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_tri_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		     const vector<efloat_t>& rho, bool do_OS,bool do_OP);



//------------------- Checking Routines ------------------------//

void check_match_P(const data_partition& P, efloat_t OS, efloat_t OP, const vector<int>& path, const DPengine& Matrices);


vector<efloat_t> sample_P(const data_partition& P, efloat_t P_choice, efloat_t rho,
			  const vector<int>& path, const DPengine& Matrices);

void check_sampling_probabilities(const std::vector< std::vector<efloat_t> >& PR);

#endif
