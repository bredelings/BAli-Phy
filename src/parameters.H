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

/// Each data_partition is a model with one parameter: mu (the branch mean)

struct data_partition: public Model
{
  friend class Parameters;

  /// The IndelModel
  OwnedPointer<IndelModel> IModel_;

  /// The substitution::Model
  OwnedPointer<substitution::MultiModel> SModel_;

  string partition_name;

  void recalc(const vector<int>&);

public:

  data_partition* clone() const {return new data_partition(*this);}

  /// The alignment data of this partition
  alignment A;

  /// Tree pushed down from above
  SequenceTree T;

  /// Cached Transition Probabilities
  MatCache MC;
  
  /// Cached Conditional Likelihoods
  mutable Likelihood_Cache LC;

  /// cached branch HMMs
  vector<indel::PairHMM> branch_HMMs;
  vector<int> branch_HMM_type;

  /// Alignment constraint
  ublas::matrix<int> alignment_constraint;

  /// Temperatures -    0:likelihood     1:prior?
  vector<double> beta;

  // get the alphabet for partition i
  const alphabet& get_alphabet() const {return A.get_alphabet();}

  /// Recalculate all the cached transition matrices
  void recalc_imodel();
  void recalc_smodel();

  bool has_IModel() const {return IModel_;}
  /// The IndelModel
  const IndelModel& IModel() const;
  /// The IndelModel
  IndelModel& IModel();

  /// Get the substitution::Model
  const substitution::MultiModel& SModel() const {return *SModel_;}
  /// Get the substitution::Model
  substitution::MultiModel& SModel() {return *SModel_;}

  void setlength(int b, double l);

  /// parameter for exponential prior on branch length
  double branch_mean() const; 
  void branch_mean(double);
  void branch_mean_tricky(double);

  // The prior, likelihood, and probability
  efloat_t prior_no_alignment() const;
  efloat_t prior_alignment() const;
  efloat_t prior() const;
  efloat_t likelihood() const;
  efloat_t probability() const {return prior() * likelihood();}
  efloat_t heated_likelihood() const;
  efloat_t heated_probability() const;

  string name() const;

  data_partition(const string& n, const alignment&, const SequenceTree&,
		 const substitution::MultiModel&);
  data_partition(const string& n, const alignment&, const SequenceTree&,
		 const substitution::MultiModel&, const IndelModel&);
};

/// A class to contain all the MCMC state except the alignment
class Parameters: public SuperModel
{
  /// The IndelModel
  OwnedPointer<IndelModel> IModel_;

  /// The substitution::Model
  OwnedPointer<substitution::MultiModel> SModel_;

  void recalc(const vector<int>&);

public:

  /// The alignments and cached computations
  vector<data_partition> data_partitions;

  int n_data_partitions() const {return data_partitions.size();}

  const data_partition& operator[](int i) const {return data_partitions[i];}
        data_partition& operator[](int i)       {return data_partitions[i];}

  /// The Tree - mean branch lengths = 1
  SequenceTree T;

  /// Topology and Branch-Length Constraint
  SequenceTree TC;

  /// Alignment-constrained branches
  std::vector<int> AC;

  vector<int> branch_HMM_type;

  /// An object for key-value pairs.
  std::map<std::string,double> keys;

  vector<double> beta;
  vector<double> beta_series;

  /// Tree partitions to weight
  vector<Partition> partitions;
  vector<efloat_t> partition_weights;

  /// Is pinning enabled or not?
  vector<double> constants;
  /// Is pinning enabled or not?
  int features;

  // The prior, likelihood, and probability
  efloat_t prior_no_alignment() const;
  efloat_t prior_alignment() const;
  efloat_t prior() const;
  efloat_t likelihood() const;
  efloat_t probability() const { return prior() * likelihood(); }

  efloat_t heated_likelihood() const;
  efloat_t heated_probability() const;

  bool has_IModel() const {return IModel_;}
  /// The IndelModel
  const IndelModel& IModel() const;
  /// The IndelModel
  IndelModel& IModel();

  /// Get the substitution::Model
  const substitution::MultiModel& SModel() const {return *SModel_;}
  /// Get the substitution::Model
  substitution::MultiModel& SModel() {return *SModel_;}


  /// Set branch 'b' to have length 'l', and compute the transition matrices
  void setlength(int b,double l); 
  
  /// Recalculate all the cached transition matrices
  void recalc_imodel();
  void recalc_smodel();
  void tree_propagate();

  void select_root(int b);
  void set_root(int b);
  void invalidate_subA_index_branch(int b);
  void LC_invalidate_branch(int b);

  double branch_mean() const; 

  Parameters* clone() const {return new Parameters(*this);}

  string name() const {return "Full Model";}

  const Model& SubModels(int) const;
  Model& SubModels(int);
  int n_submodels() const;

  Parameters(const vector<alignment>& A, const SequenceTree&, const substitution::MultiModel&);
  Parameters(const vector<alignment>& A, const SequenceTree&, const substitution::MultiModel&,const IndelModel&);
};

bool accept_MH(const Parameters& P1,const Parameters& P2,double rho);


#endif
