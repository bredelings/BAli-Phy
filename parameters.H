#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "alphabet.H"
#include "mytypes.H"
#include "tree.H"
#include <valarray>
#include "smodel.H"
#include "imodel.H"
#include "alignment.H"

/******************************* parameter-containing class **************************/
/// A class to contain all the MCMC state except the alignment
class Parameters {
  vector< vector<Matrix> > transition_P_;

  /*--------We own this----------*/
  /// The substitution::Model
  substitution::MultiRateModel* SModel_;


public:
  /// Is pinning enabled or not?
  vector<double> constants;
  /// Is pinning enabled or not?
  int features;

  /// Which model are we currently running
  int model;


  /// The likelihood function
  double likelihood(const alignment& A,const Parameters& P) const; 

  /// The prior
  double prior(const alignment& A,const Parameters& P) const;

  /// Now we can link moves into probability model?
  double probability(const alignment& A,const Parameters& P) const {
    return prior(A,P) + likelihood(A,P);
  }

  bool accept_MH(const alignment& A1,const Parameters& P1,
		 const alignment& A2,const Parameters& P2) const;

  /// The IndelModel
  IndelModel IModel;

  /*------------tree-------------*/
  SequenceTree T;

  /// Get the substitution::Model
  const substitution::MultiRateModel& SModel() const {return *SModel_;}

  /// Get the alphabet
  const alphabet& get_alphabet() const {return SModel_->Alphabet();}

  /// Symmetrically propose another state
  void fiddle();

  //FIXME - why this hack?
  const vector< Matrix > & transition_P(int r) const { return transition_P_[r]; }

  /// Get the transition matrix for branch 'b', and rate 'r'
  const Matrix& transition_P(int r,int b) const 
  {
    assert(b != T.num_nodes()-2);
    if (b==T.num_nodes()-2) b--;
    return transition_P_[r][b];
  }

  /// Set branch 'b' to have length 'l', and compute the transition matrices
  void setlength(int b,double l);
  
  /// Recalculate all the cached transition matrices
  void recalc();

  /// parameter for exponential prior on branch length
  double branch_mean; 

  Parameters& operator=(const Parameters&);

  Parameters(const Parameters&);
  Parameters(const substitution::MultiRateModel&,const IndelModel&,const SequenceTree&);
  ~Parameters() {delete SModel_;}
};

#endif
