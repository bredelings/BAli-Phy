#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>

#include "dpmatrix.H"
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
double other_prior(const alignment& A, const Parameters& P, int n0);

/// Routine for summing/sampling possibilities for sequences at an internal node
DParrayConstrained sample_node_base(alignment& A,const Parameters& P,const std::vector<int>& nodes);

/// Routine for summing/sampling possibilities for sequences at two adjacent internal nodes
DParrayConstrained sample_two_nodes_base(alignment& A,const Parameters& P,const std::vector<int>& nodes);

/// Routine for summing/sampling possibilities for sequence at an internal node, and an adjacent alignment
DPmatrixConstrained tri_sample_alignment_base(alignment& A,const Parameters& P,const std::vector<int>& nodes);

#endif
