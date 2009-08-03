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
#include "cow-ptr.H"
#include "cached_value.H"

//------------------------------- parameter-containing class --------------------------//

/// Each data_partition is a model with one parameter: mu (the branch mean)

struct data_partition: public Model
{
  friend class Parameters;

  /// The IndelModel
  polymorphic_cow_ptr<IndelModel> IModel_;

  /// The Transducer IndelModel
  polymorphic_cow_ptr<TransducerIndelModel> TIModel_;

  /// The substitution::Model
  polymorphic_cow_ptr<substitution::MultiModel> SModel_;

  string partition_name;

  void recalc(const vector<int>&);

  mutable cached_value<efloat_t> cached_alignment_prior;

  mutable vector<cached_value<efloat_t> > cached_alignment_prior_for_branch;

  mutable vector<cached_value<ublas::matrix<int> > > cached_alignment_counts_for_branch;

  mutable vector<cached_value<ublas::matrix<int> > > cached_transducer_counts_for_branch;

  mutable vector<cached_value<int> > cached_sequence_lengths;

  double branch_mean_;

  void branch_mean(double);
  void branch_mean_tricky(double);

public:

  bool smodel_full_tree;

  data_partition* clone() const {return new data_partition(*this);}

  /// The alignment data of this partition
  cow_ptr<alignment> A;

  /// Tree pushed down from above
  cow_ptr<SequenceTree> T;

  /// Cached Transition Probabilities
  MatCache MC;
  
  /// Cached Conditional Likelihoods
  mutable Likelihood_Cache LC;

  /// cached branch HMMs
  vector<indel::PairHMM> branch_HMMs;
  vector<indel::PairTransducer> branch_PTMs;
  vector<int> branch_HMM_type;

  /// Alignment constraint
  ublas::matrix<int> alignment_constraint;

  /// Temperatures -    0:likelihood     1:prior?
  vector<double> beta;

  // get the alphabet for partition i
  const alphabet& get_alphabet() const {return A->get_alphabet();}

  /// Recalculate all the cached transition matrices
  void recalc_imodel();
  void recalc_timodel();
  void recalc_smodel();
 
  bool has_IModel() const {return IModel_;}
  /// The IndelModel
  const IndelModel& IModel() const;
  /// The IndelModel
  IndelModel& IModel();

  bool has_TIModel() const {return TIModel_;}
  /// The IndelModel
  const TransducerIndelModel& TIModel() const;
  /// The IndelModel
  TransducerIndelModel& TIModel();

 /// Get the substitution::Model
  const substitution::MultiModel& SModel() const {return *SModel_;}
  /// Get the substitution::Model
  substitution::MultiModel& SModel() {return *SModel_;}

  void setlength(int b, double l);

  int seqlength(int n) const;

  void note_alignment_changed_on_branch(int b);

  void note_sequence_length_changed(int n);

  void note_column_label_changed();

  /// parameter for exponential prior on branch length
  double branch_mean() const; 

  // The prior, likelihood, and probability
  efloat_t prior_no_alignment() const;
  efloat_t prior_alignment() const;
  efloat_t prior() const;
  efloat_t likelihood() const;
  efloat_t probability() const {return prior() * likelihood();}
  efloat_t heated_prior() const;
  efloat_t heated_likelihood() const;
  efloat_t heated_probability() const;

  string name() const;

  data_partition(const string& n, const alignment&, const SequenceTree&,
		 const substitution::MultiModel&);
  data_partition(const string& n, const alignment&, const SequenceTree&,
		 const substitution::MultiModel&, const IndelModel&);
  data_partition(const string& n, const alignment&, const SequenceTree&,
		 const substitution::MultiModel&, const TransducerIndelModel&);
};

/// A class to contain all the MCMC state except the alignment
class Parameters: public SuperModel
{
  /// The substitution::Model
  vector<polymorphic_cow_ptr<substitution::MultiModel> > SModels;
  vector<int> smodel_for_partition;

  /// The IndelModel
  vector<polymorphic_cow_ptr<IndelModel> > IModels;
  vector<int> imodel_for_partition;

  /// The IndelModel
  vector<polymorphic_cow_ptr<TransducerIndelModel> > TIModels;
  vector<int> timodel_for_partition;

  // Scale for partition
  vector<int> scale_for_partition;
  int n_scales;

  void recalc(const vector<int>&);

public:

  bool smodel_full_tree;

  /// The alignments and cached computations
  vector<cow_ptr<data_partition> > data_partitions;

  int n_data_partitions() const {return data_partitions.size();}

  const data_partition& operator[](int i) const {return *data_partitions[i];}
        data_partition& operator[](int i)       {return *data_partitions[i];}

  int get_smodel_index_for_partition(int i) const {return smodel_for_partition[i];}

  int get_imodel_index_for_partition(int i) const {return imodel_for_partition[i];}

  int get_timodel_index_for_partition(int i) const {return timodel_for_partition[i];}

  /// The Tree - mean branch lengths = 1
  cow_ptr<SequenceTree> T;

  /// Topology and Branch-Length Constraint
  cow_ptr<SequenceTree> TC;

  /// Alignment-constrained branches
  std::vector<int> AC;

  vector<int> branch_HMM_type;

  /// An object for key-value pairs.
  std::map<std::string,double> keys;

  vector<double> beta;
  vector<double> beta_series;

  /// Did we most recently hit beta==1 (1) or beta=0 (0)
  int updown;

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

  efloat_t heated_prior() const;
  efloat_t heated_likelihood() const;
  efloat_t heated_probability() const;

  /// How many substitution models?
  int n_smodels() const {return SModels.size();}
  /// Get the substitution::Model
  const substitution::MultiModel& SModel(int i) const {return *SModels[i];}
  /// Get the substitution::Model
        substitution::MultiModel& SModel(int i)       {return *SModels[i];}
  
  /// How many substitution models?
  int n_imodels() const {return IModels.size();}
  /// Get the substitution::Model
  const IndelModel& IModel(int i) const {return *IModels[i];}
  /// Get the substitution::Model
        IndelModel& IModel(int i)       {return *IModels[i];}

  /// How many substitution models?
  int n_timodels() const {return TIModels.size();}
  /// Get the substitution::Model
  const TransducerIndelModel& TIModel(int i) const {return *TIModels[i];}
  /// Get the substitution::Model
        TransducerIndelModel& TIModel(int i)       {return *TIModels[i];}

  /// Set branch 'b' to have length 'l', and compute the transition matrices
  void setlength(int b,double l); 
  
  /// Recalculate all the cached transition matrices
  void recalc_imodels();
  void recalc_imodel(int i);
  void recalc_timodels();
  void recalc_timodel(int i);
  void recalc_smodels();
  void recalc_smodel(int i);
  void tree_propagate();

  void select_root(int b);
  void set_root(int b);

  // invalidate likelihoood caches on b and b* and all DIRECTED branches after them
  void LC_invalidate_branch(int b);

  // invalidate subA indices on b and b* and all DIRECTED branches after them
  void invalidate_subA_index_branch(int b);

  void note_alignment_changed_on_branch(int b);

  void note_sequence_length_changed(int n);

  int n_branch_means() const;

  double branch_mean() const;

  double branch_mean(int i) const;

  void branch_mean_tricky(int i,double); 

  Parameters* clone() const {return new Parameters(*this);}

  string name() const {return "Full Model";}

  const Model& SubModels(int) const;
  Model& SubModels(int);

  Parameters(const vector<alignment>& A, const SequenceTree&, 
	     const vector<polymorphic_cow_ptr<substitution::MultiModel> >&,
	     const vector<int>&,
	     const vector<int>&);

  Parameters(const vector<alignment>& A, const SequenceTree&, 
	     const vector<polymorphic_cow_ptr<substitution::MultiModel> >&,
	     const vector<int>&,
	     const vector<polymorphic_cow_ptr<IndelModel> >&,
	     const vector<int>&,
	     const vector<int>&);

  Parameters(const vector<alignment>& A, const SequenceTree&, 
	     const vector<polymorphic_cow_ptr<substitution::MultiModel> >&,
	     const vector<int>&,
	     const vector<polymorphic_cow_ptr<TransducerIndelModel> >&,
	     const vector<int>&,
	     const vector<int>&);
};

bool accept_MH(const Parameters& P1,const Parameters& P2,double rho);


#endif
