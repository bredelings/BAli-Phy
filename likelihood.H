#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "alignment.H"
#include "parameters.H"
#include "substitution.H"

//------------------- Priors ----------------------//
// Tree prior: branch lengths & topology
double prior(const SequenceTree& T,double branch_mean);

// Tree prior + SModel prior
double prior(const Parameters&);

//-------------- Alignment Priors -----------------//

// Full Model: t-gaps
double prior_HMM(const alignment&,const Parameters&);
double prior_HMM_nogiven(const alignment&,const Parameters&);
double prior_branch(const alignment&,const IndelModel&,int parent,int child);

// Reduced Model: s-gaps
double prior_branch_notree_nogiven(const alignment&,const IndelModel&,int);
double prior_branch_notree(const alignment&,const IndelModel&,int);
double prior_HMM_notree(const alignment&,const Parameters&);

// --------------- Probability Models -----------------------//
double Pr_tgaps_tletters(const alignment&,const Parameters&);
double Pr_tgaps_sletters(const alignment&,const Parameters&);
double Pr_sgaps_tletters(const alignment&,const Parameters&);
double Pr_sgaps_sletters(const alignment&,const Parameters&);

inline double probability3(const alignment& A,const Parameters& P) {
  return Pr_tgaps_tletters(A,P);
}

#endif
