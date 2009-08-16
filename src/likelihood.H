#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "imodel.H"
#include "alignment.H"
#include "parameters.H"

//-------------- Alignment Priors -----------------//

efloat_t prior_HMM_rootless_scale(const data_partition& P);
efloat_t prior_branch(const alignment&,const indel::PairHMM&,int parent,int child);
efloat_t prior_branch_from_counts(const ublas::matrix<int>& counts, const indel::PairHMM&);
efloat_t prior_HMM_nogiven(const data_partition&);
efloat_t prior_HMM(const data_partition&);
ublas::matrix<int> get_path_counts(const alignment& A,int node1, int node2);

//------------------- Priors ----------------------//
efloat_t topology_weight(const Parameters& P, const SequenceTree& T);

// Tree prior: branch lengths & topology
efloat_t prior_exponential(const SequenceTree& T,double branch_mean);

efloat_t prior_gamma(const SequenceTree& T,double branch_mean);

// Tree prior: branch lengths & topology
efloat_t prior(const Parameters& P, const SequenceTree& T,double branch_mean);
#endif
