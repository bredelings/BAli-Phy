#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>

#include "dp-engine.H"
#include "alignment.H"
#include "parameters.H"

/// Define type for a function which return the distributions for each column and rate give SOME leaves
typedef vector< vector< valarray<double> > > (*distributions_t)(const alignment&, const Parameters&,
							      const vector<int>&,int,const valarray<bool>&);


/// Distributions function for a star tree
vector< vector<valarray<double> > > distributions_star(const alignment& A,const Parameters& P,
						       const vector<int>& seq,int root,const valarray<bool>& group);

/// Distributions function for a full tree
vector< vector<valarray<double> > > distributions_tree(const alignment& A,const Parameters& P,
						       const vector<int>& seq,int root,const valarray<bool>& group);


/// Sum of likelihoods for columns which don't contain any characters in sequences mentioned in 'nodes'
double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes);

/// Sum of branch priors for branches not connected to no
double other_prior(const alignment& A, const Parameters& P, const vector<int>& nodes);

/// Routine for summing/sampling possibilities for sequences at an internal node
DParrayConstrained sample_node_base(alignment& A,const Parameters& P,const std::vector<int>& nodes);

/// Routine for summing/sampling possibilities for sequences at two adjacent internal nodes
DParrayConstrained sample_two_nodes_base(alignment& A,const Parameters& P,const std::vector<int>& nodes);

/// Routine for summing/sampling possibilities for sequence at an internal node, and an adjacent alignment
DPmatrixConstrained tri_sample_alignment_base(alignment& A,const Parameters& P,const std::vector<int>& nodes);


/// Routine for simultaneosly sampling between several Parameter choices, and summing out some nodes
bool sample_two_nodes_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP);

/// Routine for simultaneosly sampling between several Parameter choices, and summing out some nodes
bool sample_tri_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP);



//------------------- Checking Routines ------------------------//

void check_match_P(const alignment& A,const Parameters& P, double OS, double OP, const vector<int>& path, const DPengine& Matrices);


vector<double> sample_P(const alignment& A,const Parameters& P,double OS, double OP, double P_choice,
			const vector<int>& path, const DPengine& Matrices);

#endif
