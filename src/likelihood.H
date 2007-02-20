#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "imodel.H"
#include "alignment.H"
#include "parameters.H"

//-------------- Alignment Priors -----------------//

efloat_t prior_branch(const alignment&,const indel::PairHMM&,int parent,int child);
efloat_t prior_HMM_nogiven(const data_partition&);
efloat_t prior_HMM(const data_partition&);

//------------------- Priors ----------------------//
efloat_t topology_weight(const Parameters& P, const SequenceTree& T);

// Tree prior: branch lengths & topology
efloat_t prior(const SequenceTree& T,double branch_mean);

// Tree prior: branch lengths & topology
efloat_t prior(const Parameters& P, const SequenceTree& T,double branch_mean);

// Tree prior + SModel prior
efloat_t prior_no_alignment(const Parameters&);
#endif
