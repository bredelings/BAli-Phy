#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>

#include "mytypes.H"
#include "dp-engine.H"
#include "alignment.H"
#include "parameters.H"

/// Define type for a function which return the distributions for each column and rate give SOME leaves
typedef vector< Matrix > (*distributions_t)(const alignment&, const Parameters&,const vector<int>&,int,const valarray<bool>&);


/// Distributions function for a star tree
vector< Matrix > distributions_star(const alignment& A,const Parameters& P,const vector<int>& seq,int root,const valarray<bool>& group);

/// Distributions function for a full tree
vector< Matrix > distributions_tree(const alignment& A,const Parameters& P,const vector<int>& seq,int root,const valarray<bool>& group);


/// Sum of likelihoods for columns which don't contain any characters in sequences mentioned in 'nodes'
efloat_t other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes);

/// Sum of branch priors for branches not connected to no
efloat_t other_prior(const alignment& A, const Parameters& P, const vector<int>& nodes);


/// Routine for simultaneously sampling between several Parameter choices, and summing out a node
bool sample_node_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
bool sample_two_nodes_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
bool sample_tri_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP);



//------------------- Checking Routines ------------------------//

void check_match_P(const alignment& A,const Parameters& P, efloat_t OS, efloat_t OP, const vector<int>& path, const DPengine& Matrices);


vector<efloat_t> sample_P(const alignment& A,const Parameters& P,efloat_t OS, efloat_t OP, efloat_t P_choice,
			  const vector<int>& path, const DPengine& Matrices);

void check_sampling_probabilities(const std::vector< std::vector<efloat_t> >& PR,const std::vector<alignment>& a);

#endif
