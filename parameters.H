#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "alphabet.H"
#include "mytypes.H"
#include "tree.H"
#include <valarray>
#include "smodel.H"
#include "imodel.H"

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
