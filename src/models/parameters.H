/*
   Copyright (C) 2004-2012 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file parameters.H
///
/// \brief This file defines the Parameters class which holds the model and state
///        for the MCMC.
///

#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <string>
#include <valarray>
#include "sequence/alphabet.H"
#include "mytypes.H"
#include "tree/sequencetree.H"
#include "models/probability-model.H"
#include "imodel/imodel.H"
#include "alignment/alignment.H"
#include "substitution/matcache.H"
#include "substitution/substitution-cache.H"
#include "substitution/substitution-index.H"

#include "tools/partition.H"
#include "cow-ptr.H"
#include "cached_value.H"
#include "dp/2way.H"

//------------------------------- parameter-containing class --------------------------//

extern bool use_internal_index;

class Parameters;

/// Each data_partition is a model with one parameter: mu (the branch mean)
class data_partition: public Mat_Cache
{
  friend class Parameters;

  const Parameters* P;

  int partition_index;

  /// The pairwise alignment for each branch
  std::vector<int> pairwise_alignment_for_branch;

  /// The pairwise alignment probability for each branch
  std::vector<int> alignment_prior_for_branch;

  void invalidate_pairwise_alignment_for_branch(int b) const;

  void set_pairwise_alignment_(int b, const pairwise_alignment_t&,bool=true) const;

  std::vector<int> sequence_length_indices;

  std::vector<int> transition_p_method_indices;

  std::vector<int> branch_HMM_indices;

  int alignment_prior_index = -1;

  std::vector<int> frequencies_indices;

  ublas::matrix<int> base_model_indices;

  void branch_mean_changed();

  bool variable_alignment_;

public:

  /// Is the alignment allowed to vary?
  bool variable_alignment() const {return variable_alignment_;}
  
  /// Set alignment variability
  void variable_alignment(bool b);

  data_partition* clone() const {return new data_partition(*this);}

  /// The leaf sequence data of this partition
  cow_ptr< std::vector< std::vector<int> > > sequences;

  /// The alignment data of this partition
  cow_ptr<alignment> A;

  const SequenceTree& T() const;

  /// Cached Transition Probabilities
  const std::vector<Matrix>& transition_P(int b) const;
  
  int n_base_models() const;

  // Perhaps this should really be per-mixture-component.
  int n_states() const;

  // Perhaps this should also be per-mixture-component.
  std::vector<unsigned> state_letters() const;
  
  std::vector<double> distribution() const;

  object_ptr<const Object> base_model(int m, int b) const;

  std::vector<double> frequencies(int m) const;

  /// Cached Conditional Likelihoods
  mutable Likelihood_Cache LC;

  /// sub-alignment indices;
  mutable object_ptr<subA_index_t> subA;

  /// cached branch HMMs
  const indel::PairHMM& get_branch_HMM(int b) const;
  std::vector<indel::PairHMM> get_branch_HMMs(const std::vector<int>&) const;

  double sequence_length_pr(int l) const;

  std::vector<int> branch_HMM_type;

  /// Alignment constraint
  ublas::matrix<int> alignment_constraint;

  double get_beta() const;

  // get the alphabet for partition i
  const alphabet& get_alphabet() const {return A->get_alphabet();}

  /// Recalculate all the cached transition matrices
  void recalc_smodel();

  void uniquify_subA_index();

  bool has_IModel() const;

  void setlength(int b);

  int seqlength(int n) const;

  // invalidate subA indices on b and b* and all DIRECTED branches after them
  void invalidate_subA_index_branch(int b);
  // invalidate subA indices on b and b*
  void invalidate_subA_index_one_branch(int b);
  // invalidate subA indices for all branches
  void invalidate_subA_index_all();

  object_ptr<const Object> get_pairwise_alignment_(int b) const;

  const pairwise_alignment_t& get_pairwise_alignment(int b,bool=true) const;

  void set_pairwise_alignment(int b, const pairwise_alignment_t&, bool=true);

  void recompute_pairwise_alignment(int b, bool=true);

  void subA_index_allow_invalid_branches(bool);

  void note_alignment_changed_on_branch(int b);

  void note_alignment_changed();

  // The prior, likelihood, and probability
  efloat_t prior_no_alignment() const;
  efloat_t prior_alignment() const;

  efloat_t prior() const;
  efloat_t likelihood() const;

  efloat_t heated_likelihood() const;

  efloat_t heated_prior() const {return prior();}
  efloat_t heated_probability() const {return heated_prior() * heated_likelihood();}

  data_partition(Parameters* P, int i, const alignment&);
};

struct imodel_methods
{
  int length_arg_param_index;
  int length_p;
  std::vector<int> parameters;
};

struct smodel_methods
{
  int main;
  int n_base_models;
  int n_states;
  int state_letters;
  int distribution;
  int get_alphabet;
  int rate;

  // but these ones take an argument!
  int transition_p; // int branch
  int base_model; // int index
  int frequencies; // int 
  
  // also:
  // Q1: can we use evaluate_expression( ) to pass in arguments?  what are the pitfalls?
  // Q2: how could we control the peeling from haskell?
  // 
  smodel_methods(const expression_ref&, context&);
};

/// A class to contain all the MCMC state except the alignment
class Parameters: public Probability_Model
{
  friend class data_partition;

  /// The substitution::Model
  std::vector<smodel_methods> SModels;
  std::vector<int> smodel_for_partition;

  /// The IndelModel
  std::vector<imodel_methods> IModel_methods;
  std::vector<int> imodel_for_partition;

  // Scale for partition
  std::vector<int> scale_for_partition;
  int n_scales;

  void recalc();

  std::vector< std::vector<int> > branch_length_indices;

  ublas::matrix<int> branch_transition_p_indices;

  /// The alignments and cached computations
  mutable std::vector<data_partition> data_partitions;

  /// The Tree - mean branch lengths = 1
  cow_ptr<SequenceTree> T_;

  std::vector<int> parameter_for_tree_node;
  std::vector<int> parameter_for_tree_branch;

public:

  /// Do we have an Exponential (0) or Gamma-0.5 (1) prior on branch lengths?
  int branch_prior_type;

  bool variable_alignment() const;

  void variable_alignment(bool b);

  const data_partition& get_data_partition(int i) const;
        data_partition& get_data_partition(int i);

  int n_data_partitions() const {return data_partitions.size();}

  const data_partition& operator[](int i) const {return get_data_partition(i);}
        data_partition& operator[](int i)       {return get_data_partition(i);}

  int get_smodel_index_for_partition(int i) const {return smodel_for_partition[i];}

  int get_imodel_index_for_partition(int i) const {return imodel_for_partition[i];}

  void set_beta(double b);
  double get_beta() const;

  const SequenceTree& T() const;

  void reconnect_branch(int s1, int t1, int t2);
  void set_tree(const SequenceTree& T);
  void exchange_subtrees(int b1, int b2);
  int SPR(int b1, int b2, int bm=-1);
  void read_h_tree();
  void check_h_tree() const;

  /// Topology and Branch-Length Constraint
  cow_ptr<SequenceTree> TC;

  /// Alignment-constrained branches
  std::vector<int> AC;

  std::vector<int> branch_HMM_type;

  std::vector<double> beta_series;

  std::vector<double> all_betas;
  int beta_index;

  /// Did we most recently hit beta==1 (1) or beta=0 (0)
  int updown;

  /// Tree partitions to weight
  std::vector<Partition> partitions;
  std::vector<efloat_t> partition_weights;

  /// Is pinning enabled or not?
  std::vector<double> constants;
  /// Is pinning enabled or not?
  int features;

  double branch_length_max;

  // The prior, likelihood, and probability
  efloat_t prior_no_alignment() const;
  efloat_t prior_alignment() const;
  efloat_t prior() const;
  efloat_t likelihood() const;
  efloat_t probability() const { return prior() * likelihood(); }

  efloat_t heated_likelihood() const;

  /// How many substitution models?
  int n_smodels() const {return SModels.size();}
  
  /// How many substitution models?
  int n_imodels() const {return IModel_methods.size();}

  object_ptr<const alphabet> get_alphabet_for_smodel(int i) const;

  /// Set branch 'b' to have length 'l', and compute the transition matrices
  void setlength_unsafe(int b, double l);
  void setlength(int b,double l); 
  void setlength_no_invalidate_LC(int b,double l); 
  
  /// Recalculate all the cached transition matrices
  void recalc_smodels();
  void recalc_smodel(int i);

  void select_root(int b);
  void set_root(int b);

  // invalidate likelihoood caches on b and b* and all DIRECTED branches after them
  void LC_invalidate_branch(int b);

  // invalidate likelihoood caches on b and b* and all DIRECTED branches after them
  void LC_invalidate_one_branch(int b);

  // invalidate likelihoood caches on b and b* and all DIRECTED branches after them
  void LC_invalidate_all();

  // make sure we aren't sharing our subA index with any other model.
  void uniquify_subA_indices();
  // invalidate subA indices on b and b* and all DIRECTED branches after them
  void invalidate_subA_index_branch(int b);
  // invalidate subA indices on b and b*
  void invalidate_subA_index_one_branch(int b);
  // invalidate subA indices for all branches
  void invalidate_subA_index_all();

  void subA_index_allow_invalid_branches(bool);

  void note_alignment_changed_on_branch(int b);

  void recompute_pairwise_alignment(int b, bool=true);

  void note_alignment_changed();

  int n_branch_means() const;

  double branch_mean() const;

  int branch_mean_index(int i) const;

  void branch_mean(int i,double); 

  void branch_mean_tricky(int i,double); 

  double get_branch_subst_rate(int p, int b) const;

  Parameters* clone() const {return new Parameters(*this);}

  std::string name() const {return "Full Model";}

  Parameters(const module_loader& L,
	     const std::vector<alignment>& A, const SequenceTree&, 
	     const std::vector<formula_expression_ref>&,
	     const std::vector<int>&,
	     const std::vector<int>&);

  Parameters(const module_loader& L,
	     const std::vector<alignment>& A, const SequenceTree&, 
	     const std::vector<formula_expression_ref>&,
	     const std::vector<int>&,
	     const std::vector<formula_expression_ref>&,
	     const std::vector<int>&,
	     const std::vector<int>&);
};

bool accept_MH(const Probability_Model& P1,const Probability_Model& P2,double rho);


#endif
