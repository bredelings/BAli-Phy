#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>

#include "mytypes.H"
#include "dp-engine.H"
#include "alignment.H"
#include "parameters.H"

/// Define type for a function which return the distributions for each column and rate give SOME leaves
typedef vector< Matrix > (*distributions_t)(const data_partition&,const vector<int>&,int,const valarray<bool>&);


/// Distributions function for a star tree
vector< Matrix > distributions_star(const data_partition& P,const vector<int>& seq,int root,const valarray<bool>& group);

/// Distributions function for a full tree
vector< Matrix > distributions_tree(const data_partition& P,const vector<int>& seq,int root,const valarray<bool>& group);


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
