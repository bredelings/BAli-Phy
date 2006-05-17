#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <string>
#include <valarray>
#include <map>
#include "alphabet.H"
#include "mytypes.H"
#include "sequencetree.H"
#include "smodel.H"
#include "imodel.H"
#include "alignment.H"
#include "matcache.H"
#include "clone.H"
#include "substitution-cache.H"
#include "util.H"
#include "tools/tree-dist.H"

//------------------------------- parameter-containing class --------------------------//

/// A class to contain all the MCMC state except the alignment
class Parameters: public MatCache, public SuperModel
{
  /// The IndelModel
  OwnedPointer<IndelModel> IModel_;

  /// The substitution::Model
  OwnedPointer<substitution::MultiModel> SModel_;

  void recalc(const vector<int>&);
public:

  /// An object for key-value pairs.
  std::map<std::string,double> keys;

  /// cached branch HMMs
  vector<indel::PairHMM> branch_HMMs;

  /// 1/Temperature parameters, for heated chains: when heated we aren't normalized...
  vector<double> beta;

  vector<double> beta_series;

  /// Partitions to weight
  vector<Partition> partitions;
  vector<efloat_t> partition_weights;

  /// Is pinning enabled or not?
  vector<double> constants;
  /// Is pinning enabled or not?
  int features;

  /// The likelihood function
  efloat_t basic_likelihood(const alignment& A,const Parameters& P) const; 
  efloat_t likelihood(const alignment& A,const Parameters& P) const {return pow(basic_likelihood(A,P),beta[0]);}

  /// The prior
  efloat_t basic_prior(const alignment& A,const Parameters& P) const;
  efloat_t prior(const alignment& A,const Parameters& P) const {return pow(basic_prior(A,P),beta[1]);}

  /// Now we can link moves into probability model?
  efloat_t probability(const alignment& A,const Parameters& P) const {
    return prior(A,P) * likelihood(A,P);
  }

  bool accept_MH(const alignment& A1,const Parameters& P1,
		 const alignment& A2,const Parameters& P2,double rho) const;

  bool accept_MH(const alignment& A,const Parameters& P1,const Parameters& P2,double rho) const;

  /// The Tree
  SequenceTree T;

  /// Alignment constraint
  ublas::matrix<int> alignment_constraint;

  /// Cached Conditional Likelihoods
  mutable Likelihood_Cache LC;

  bool has_IModel() const {return IModel_;}
  /// The IndelModel
  const IndelModel& IModel() const;
  /// The IndelModel
  IndelModel& IModel();

  /// Get the substitution::Model
  const substitution::MultiModel& SModel() const {return *SModel_;}
  /// Get the substitution::Model
  substitution::MultiModel& SModel() {return *SModel_;}

  /// Get the alphabet
  const alphabet& get_alphabet() const {return SModel_->Alphabet();}

  /// Set branch 'b' to have length 'l', and compute the transition matrices
  void setlength(int b,double l); 
  
  /// Recalculate all the cached transition matrices
  void recalc_imodel();
  void recalc_smodel();

  Parameters* clone() const {return new Parameters(*this);}

  string name() const {return "Full Model";}
  string super_parameter_name(int) const;

  const Model& SubModels(int) const;
  Model& SubModels(int);
  int n_submodels() const;

  /// parameter for exponential prior on branch length
  double branch_mean() const; 
  void branch_mean(double);

  Parameters(const substitution::MultiModel&,const SequenceTree&);
  Parameters(const substitution::MultiModel&,const IndelModel&,const SequenceTree&);
};

#endif
