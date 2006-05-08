#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "imodel.H"
#include "alignment.H"
#include "parameters.H"

/// This is the type for a probability function
//typedef double (Pr_func)(const alignment&,const Parameters&);

//------------------- Priors ----------------------//
efloat_t topology_weight(const Parameters& P, const SequenceTree& T);

// Tree prior: branch lengths & topology
efloat_t prior(const Parameters& P, const SequenceTree& T,double branch_mean);

// Tree prior + SModel prior
efloat_t prior(const Parameters&);

//-------------- Alignment Priors -----------------//

efloat_t prior_branch(const alignment&,const indel::PairHMM&,int parent,int child);
efloat_t prior_HMM_nogiven(const alignment&,const Parameters&);
efloat_t prior_HMM(const alignment&,const Parameters&);

// --------------- Default model ----------------- //
efloat_t prior3(const alignment& A,const Parameters& P);
efloat_t likelihood3(const alignment& A,const Parameters& P);
efloat_t probability3(const alignment& A,const Parameters& P);


#endif
