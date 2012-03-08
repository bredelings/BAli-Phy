/*
   Copyright (C) 2004-2011 Benjamin Redelings

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
/// \file parameters.C
///
/// \brief This file implements the Parameters class which holds the model and state
///        for the MCMC.
///

#include "parameters.H"
#include "smodel/smodel.H"
#include "rng.H"
#include "substitution.H"
#include "substitution-index.H"
#include "alignment-util.H"
#include "likelihood.H"
#include "util.H"
#include "proposals.H"
#include "probability.H"
#include "timer_stack.H"

using std::vector;
using std::string;
using std::cerr;
using std::endl;

bool use_internal_index = true;

void data_partition::set_beta(double b)
{
  beta[0] = b;
  recalc_imodel();
}

const SequenceTree& data_partition::T() const
{
  return *P->T;
}


double data_partition::get_beta() const
{
  return beta[0];
}

void data_partition::variable_alignment(bool b)
{
  variable_alignment_ = b;

  // Ignore requests to turn on alignment variation when there is no imodel or internal nodes
  if (not has_IModel() or A->n_sequences() != T().n_nodes())
    variable_alignment_ = false;

  // turning OFF alignment variation
  if (not variable_alignment()) 
  {
    subA = subA_index_leaf(A->length()+1, T().n_branches()*2);

    // We just changed the subA index type
    LC.invalidate_all();

    if (A->n_sequences() == T().n_nodes())
      if (not check_leaf_characters_minimally_connected(*A,T()))
	throw myexception()<<"Failing to turn off alignment variability: non-default internal node states";
  }
  // turning ON alignment variation
  else 
  {
    if (use_internal_index)
      subA = subA_index_internal(A->length()+1, T().n_branches()*2);
    else
      subA = subA_index_leaf(A->length()+1, T().n_branches()*2);

    assert(has_IModel() and A->n_sequences() == T().n_nodes());
    minimally_connect_leaf_characters(*A,T());
    note_alignment_changed();

    // we need to calculate the branch_HMMs
    recalc_imodel();

    // Minimally connecting leaf characters may remove empty columns, in theory.
    // And we just changed the subA index type
    LC.invalidate_all();
  }
}

const IndelModel& data_partition::IModel() const
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

IndelModel& data_partition::IModel()
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

const std::vector<Matrix>& data_partition::transition_P(int b) const
{
  b = T().directed_branch(b).undirected_name();
  assert(b >= 0 and b < T().n_branches());
  
  if (not cached_transition_P[b].is_valid())
  {
    double l = P->get_branch_subst_length(partition_index,b) / SModel().rate();
    assert(l >= 0);

    vector< Matrix >& TP = cached_transition_P[b].modify_value();
    const int n_models = SModel().n_base_models();
    for(int m=0;m<n_models;m++)
    {
      TP[m] = SModel().transition_p(l,m);
    }
    cached_transition_P[b].validate();
  }
  return cached_transition_P[b];
}

const indel::PairHMM& data_partition::get_branch_HMM(int b) const
{
  assert(variable_alignment());

  b = T().directed_branch(b).undirected_name();

  cached_value<indel::PairHMM>& HMM = cached_branch_HMMs[b];

  if (not HMM.is_valid())
  {
    // use the length, unless we are unaligned
    double D = P->get_branch_indel_length(partition_index, b);

    double indel_scale_by = 1.0;
    int indel_scale_branch = -1;
    if (get_parameter_value_as<Bool>(1))
    {
      indel_scale_by = exp( get_parameter_value_as<Double>(0) );
      indel_scale_branch = get_parameter_value_as<Int>(2);
    }

    // compute and cache the branch HMM
    if (branch_HMM_type[b] == 1)
      HMM = IModel_->get_branch_HMM(-1);
    else if (b == indel_scale_branch)
      HMM = IModel_->get_branch_HMM(D*indel_scale_by);
    else
      HMM = IModel_->get_branch_HMM(D);
  }

  return HMM;
}

vector<indel::PairHMM> data_partition::get_branch_HMMs(const vector<int>& br) const
{
  vector<indel::PairHMM> HMMs(br.size());

  for(int i=0;i<HMMs.size();i++)
    HMMs[i] = get_branch_HMM(br[i]);

  return HMMs;
}

void data_partition::recalc_imodel_for_branch(int b)
{
  if (not variable_alignment()) return;

  // FIXME #1 - this used to go along with computation of the branch_HMMs[].
  // Is it OK to move it here?
  //
  // FIXME #2 - IModel_ should be branch-specific.
  IModel_->set_heat( get_beta() );

  b = T().directed_branch(b).undirected_name();

  cached_branch_HMMs[b].invalidate();
  cached_alignment_prior.invalidate();
  cached_alignment_prior_for_branch[b].invalidate();
}

void data_partition::recalc_imodel() 
{
  for(int b=0;b<cached_branch_HMMs.size();b++) 
    recalc_imodel_for_branch(b);
}

/// \brief Recalculate cached values relating to the substitution model.
///
/// Specifically, we invalidate:
///  - cached conditional likelihoods
///  - cached transition matrices
/// We also rescale the substitution model to use branch_mean() as its
/// rate, which effectively rescales the tree to have mean branch
/// length \a branch_mean() instead of 1. 
///
void data_partition::recalc_smodel() 
{
  default_timer_stack.push_timer("recalc_smodel( )");

  //invalidate cached conditional likelihoods in case the model has changed
  LC.invalidate_all();

  //invalidate the cached transition probabilities in case the model has changed
  for(int i=0;i<cached_transition_P.size();i++)
    cached_transition_P[i].invalidate();
  default_timer_stack.pop_timer();
}

void data_partition::setlength_no_invalidate_LC(int b)
{
  default_timer_stack.push_timer("setlength_no_invalidate_LC( )");
  b = T().directed_branch(b).undirected_name();

  cached_transition_P[b].invalidate();

  recalc_imodel_for_branch(b);

  default_timer_stack.pop_timer();
}

void data_partition::setlength(int b)
{
  setlength_no_invalidate_LC(b);
  LC.invalidate_branch(T(),b);
}

int data_partition::seqlength(int n) const
{
  if (not cached_sequence_lengths[n].is_valid())
    cached_sequence_lengths[n] = A->seqlength(n);

  assert(cached_sequence_lengths[n] == A->seqlength(n));

  return cached_sequence_lengths[n];
}

void data_partition::invalidate_subA_index_branch(int b)
{
  // propagates outward in both directions
  subA->invalidate_branch(T(),b);
}

void data_partition::invalidate_subA_index_one_branch(int b)
{
  int b2 = T().directed_branch(b).reverse();
  subA->invalidate_one_branch(b);
  subA->invalidate_one_branch(b2);
}

void data_partition::invalidate_subA_index_all()
{
  subA->invalidate_all_branches();
}

void data_partition::subA_index_allow_invalid_branches(bool b)
{
#ifndef NDEBUG
  if (subA->may_have_invalid_branches())
  {
    subA->check_footprint(*A, T());
    check_regenerate(*subA, *A, T());
  }  
#endif

  subA->allow_invalid_branches(b);

#ifndef NDEBUG
  if (not subA->may_have_invalid_branches())
  {
    subA->check_footprint(*A, T());
    check_regenerate(*subA, *A, T());
  }  
#endif
}

void data_partition::set_pairwise_alignment_(int b, const pairwise_alignment_t& pi) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  int B = T().directed_branch(b).reverse();

  if (pairwise_alignment_for_branch[b].is_valid())
  {
    assert(pi == pairwise_alignment_for_branch[b]);
    assert(pairwise_alignment_for_branch[B].is_valid());
    assert(pairwise_alignment_for_branch[B] == pi.flipped());
  }
  else
  {
    assert(not pairwise_alignment_for_branch[B].is_valid());
  }


  pairwise_alignment_for_branch[b] = pi;
  pairwise_alignment_for_branch[B] = pi.flipped();
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: what pairwise alignment are you referring to?";

  int B = T().directed_branch(b).reverse();

  if (pairwise_alignment_for_branch[b].is_valid())
  {
    int n1 = T().directed_branch(b).source();
    int n2 = T().directed_branch(b).target();
    assert(pairwise_alignment_for_branch[b] == A2::get_pairwise_alignment(*A,n1,n2));
    assert(pairwise_alignment_for_branch[B].is_valid());
    assert(pairwise_alignment_for_branch[B] == A2::get_pairwise_alignment(*A,n2,n1));
  }
  else
  {
    assert(not pairwise_alignment_for_branch[B].is_valid());
    int n1 = T().directed_branch(b).source();
    int n2 = T().directed_branch(b).target();
    set_pairwise_alignment_(b, A2::get_pairwise_alignment(*A,n1,n2));
  }

  return pairwise_alignment_for_branch[b];
}

void data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi)
{
  note_alignment_changed_on_branch(b);
  set_pairwise_alignment_(b,pi);
}

void data_partition::note_sequence_length_changed(int n)
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the sequence length change?";

  cached_sequence_lengths[n].invalidate();
}

void data_partition::note_alignment_changed_on_branch(int b)
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  b = T().directed_branch(b).undirected_name();

  cached_alignment_prior.invalidate();
  cached_alignment_prior_for_branch[b].invalidate();
  cached_alignment_counts_for_branch[b].invalidate();

  int B = T().directed_branch(b).reverse();
  pairwise_alignment_for_branch[b].invalidate();
  pairwise_alignment_for_branch[B].invalidate();

  const Tree& TT = T();
  int target = TT.branch(b).target();
  int source = TT.branch(b).source();

  if (target >= TT.n_leaves())
    note_sequence_length_changed(target);
  if (source >= TT.n_leaves())
    note_sequence_length_changed(source);

  // If the alignment changes AT ALL, then the mapping from subA columns to alignment columns is broken.
  // Therefore we always mark it as out-of-date and needing to be recomputed.
  invalidate_subA_index_all();

  // However, LC depends only on the alignment of subA indices from different branches.
  // 
  // If the projected leaf alignment remains unchanged, then the subA columns
  // projected to the leaves remain unchanged.  If we only index these columns, then the
  // get_subA_index( ) will not change if we are using subA_index_leaf.
  //
  if (subA.as<subA_index_internal>())
    LC.invalidate_branch_alignment(T(),b);
}

void data_partition::note_alignment_changed()
{
  for(int b=0;b<T().n_branches();b++)
    note_alignment_changed_on_branch(b);

  // this automatically marks all non-leaf sequence lengths for recomputation.
}

void data_partition::recalc(const vector<int>& indices)
{
  // If there is no indel model, we have no parameters
  if (indices.size() and not has_IModel()) throw myexception()<<"What parameter is this???";

  for(int i=0;i<indices.size();i++)
  {
    int index = indices[i];
    if (index > 2) throw myexception()<<"What parameter is this???";
    
    // invalidate cached imodels and cached Pr(alignment_for_branch[b]) for each branch b.
    recalc_imodel();
  }
}

/// Get the mean branch length
double data_partition::branch_mean() const 
{
  return branch_mean_;
}

/// Set the mean branch length to \a mu
void data_partition::branch_mean(double mu)
{
  // unsafe!  Must then read this value into parent.
  branch_mean_ = mu;

  // the scale of the substitution tree changed
  recalc_smodel();

  // the scale of the indel tree changed also
  recalc_imodel();
}

/// \brief Set the mean branch length to \a mu without invalidating cached values
///
/// This model rescales the substitution model, but does not
/// invalidate any cached values.  This is because we assume branch
/// lengths have changed so that mu*T remains constant.
///
void data_partition::branch_mean_tricky(double mu)
{
  branch_mean_ = mu;
}

string data_partition::name() const 
{
  return partition_name;
}

efloat_t data_partition::prior_no_alignment() const 
{
  efloat_t Pr = 1.0;

  if (has_IModel())
  {
    double indel_scale_by = get_parameter_value_as<Double>(0);
    Pr *= laplace_pdf(indel_scale_by, 0, 1);

    //    bool indel_scale_on = get_parameter_value_as<Bool>(1);
    Pr *= 0.5;

    //    int indel_scale_branch = get_parameter_value_as<Int>(2);
    Pr *= 1.0/(T().n_branches());
  }

  return Pr;
}

// We want to decrease 
// (a) the number of times get_counts( ) is called
// (b) the number of times seqlength( ) is called
// (c) the number of times log( ) is called
// This should give a further 6% speedup.

efloat_t data_partition::prior_alignment() const 
{
  if (not variable_alignment()) return 1;

  if (not cached_alignment_prior.is_valid()) 
  {
    const alignment& AA = *A;
    const SequenceTree& TT = T();

    for(int b=0;b<TT.n_branches();b++) {
      if (not cached_alignment_counts_for_branch[b].is_valid()) {
	int target = TT.branch(b).target();
	int source  = TT.branch(b).source();
	cached_alignment_counts_for_branch[b] = get_path_counts(AA,target,source);
      }
#ifndef NDEBUG
      int target = TT.branch(b).target();
      int source  = TT.branch(b).source();
      ublas::matrix<int> counts = get_path_counts(AA,target,source);
      for(int i=0;i<counts.size1();i++)
	for(int j=0;j<counts.size2();j++)
	  assert(cached_alignment_counts_for_branch[b].value()(i,j) == counts(i,j));
#endif
    }

    for(int b=0;b<TT.n_branches();b++) {
      if (not cached_alignment_prior_for_branch[b].is_valid())
      {
	const ublas::matrix<int>& counts = cached_alignment_counts_for_branch[b];
	cached_alignment_prior_for_branch[b] = prior_branch_from_counts(counts, get_branch_HMM(b));
      }
#ifndef NDEBUG      
      int target = TT.branch(b).target();
      int source  = TT.branch(b).source();
      //efloat_t p1 = cached_alignment_prior_for_branch[b];
      //efloat_t p2 = prior_branch(AA, get_branch_HMM(b), target, source);
      //double error = log(p1) - log(p2);
      assert(not different(cached_alignment_prior_for_branch[b], prior_branch(AA, get_branch_HMM(b), target, source)));
#endif
    }

    efloat_t Pr = 1;
    for(int b=0;b<TT.n_branches();b++)
      Pr *= cached_alignment_prior_for_branch[b];

    cached_alignment_prior = Pr * prior_HMM_rootless_scale(*this);
  }

  
  assert(not different(cached_alignment_prior, ::prior_HMM(*this)));

  return cached_alignment_prior;
}

efloat_t data_partition::prior() const 
{
  
  return prior_alignment() * prior_no_alignment();
}


efloat_t data_partition::likelihood() const 
{
    return substitution::Pr(*this);
}

efloat_t data_partition::heated_likelihood() const 
{
  // Don't waste time calculating likelihood if we're sampling from the prior.
  if (get_beta() == 0)
    return 1;
  else
    return pow(likelihood(),get_beta());
}

/// Get the substitution::Model
const substitution::MultiModelObject& data_partition::SModel() const 
{
  int m = P->smodel_for_partition[partition_index];
  return *(P->SModels[m]->result_as<substitution::MultiModelObject>());
}

data_partition::data_partition(const string& n, Parameters* p, int i, const alignment& a,const SequenceTree& t,
			       const substitution::MultiModelObject&,const IndelModel& IM)
  :P(p),
   partition_index(i),
   IModel_(IM),
   partition_name(n),
   cached_alignment_prior_for_branch(t.n_branches()),
   pairwise_alignment_for_branch(2*t.n_branches()),
   cached_alignment_counts_for_branch(t.n_branches(),ublas::matrix<int>(5,5)),
   cached_sequence_lengths(a.n_sequences()),
   cached_branch_HMMs(t.n_branches()),
   cached_transition_P(t.n_branches()),
   branch_mean_(1.0),
   variable_alignment_(true),
   sequences( alignment_letters(a,t.n_leaves()) ),
   A(a),
   LC(t,SModel()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0)
{
  if (variable_alignment() and use_internal_index)
    subA = subA_index_internal(a.length()+1, t.n_branches()*2);
  else
    subA = subA_index_leaf(a.length()+1, t.n_branches()*2);

  for(int b=0;b<cached_alignment_counts_for_branch.size();b++)
    cached_alignment_counts_for_branch[b].invalidate();

  const int n_models = SModel().n_base_models();
  const int n_states = SModel().state_letters().size();
  for(int b=0;b<cached_transition_P.size();b++)
    cached_transition_P[b].modify_value() = vector<Matrix>(n_models,
							   Matrix(n_states, n_states));

  if (has_IModel())
  {
    add_parameter(Parameter("lambda_scale", Double(0.0)));
    add_parameter(Parameter("lambda_scale_on", Bool(false)));
    add_parameter(Parameter("lambda_scale_branch", Int(-1), between(0,T().n_branches()-1)));
  }
}

data_partition::data_partition(const string& n, Parameters* p, int i, const alignment& a,const SequenceTree& t,
			       const substitution::MultiModelObject&)
  :P(p),
   partition_index(i),
   partition_name(n),
   cached_alignment_prior_for_branch(t.n_branches()),
   cached_alignment_counts_for_branch(t.n_branches(),ublas::matrix<int>(5,5)),
   cached_sequence_lengths(a.n_sequences()),
   cached_branch_HMMs(t.n_branches()),
   cached_transition_P(t.n_branches()),
   branch_mean_(1.0),
   variable_alignment_(false),
   sequences( alignment_letters(a,t.n_leaves()) ),
   A(a),
   LC(t,SModel()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0)
{
  if (variable_alignment() and use_internal_index)
    subA = subA_index_internal(a.length()+1, t.n_branches()*2);
  else
    subA = subA_index_leaf(a.length()+1, t.n_branches()*2);

  for(int b=0;b<cached_alignment_counts_for_branch.size();b++)
    cached_alignment_counts_for_branch[b].invalidate();

  const int n_models = SModel().n_base_models();
  const int n_states = SModel().state_letters().size();
  for(int b=0;b<cached_transition_P.size();b++)
    cached_transition_P[b].modify_value() = vector<Matrix>(n_models,
							   Matrix(n_states, n_states));
}

//-----------------------------------------------------------------------------//

void Parameters::set_beta(double b)
{
  set_parameter_value(0,b);
}

double Parameters::get_beta() const
{
  return get_parameter_value_as<Double>(0);
}

efloat_t Parameters::prior_no_alignment() const 
{
  efloat_t Pr = 1.0;

  // prior on the topology and branch lengths
  Pr *= ::prior(*this, *T, 1.0);

  if (branch_length_max > 0)
    for(int i=0; i<T->n_branches(); i++)
    {
      if (T->branch(i).length() > branch_length_max)
	return 0;
    }

  // prior on mu[i], the mean branch length for scale i
  for(int i=0;i<n_branch_means();i++) {
    //  return pow(efloat_t(branch_mean()),-1.0);
    Pr *= gamma_pdf(get_parameter_value_as<Double>(branch_mean_index(i)), 0.5, 2.0);
  }
    
  // prior on the substitution model
  for(int i=0;i<SModels.size();i++)
    Pr *= SModel(i).prior();

  // prior on the insertion/deletion model
  for(int i=0;i<IModels.size();i++)
    Pr *= IModel(i).prior();

  // prior for each branch being aligned/unaliged
  if (variable_alignment()) 
  {
    const double p_unaligned = loadvalue(keys,"P_aligned",0.0);

    efloat_t pNA = p_unaligned;

    efloat_t pA = (1.0 - p_unaligned);

    for(int b=0;b<T->n_branches();b++)
      if (not branch_HMM_type[b])
	Pr *= pA;
      else
	Pr *= pNA;
  }

  // prior on parameters in data partitions
  for(int i=0;i<data_partitions.size();i++)
    Pr *= data_partitions[i]->prior_no_alignment();

  return Pr;
}

efloat_t Parameters::prior_alignment() const 
{
  efloat_t Pr = 1;

  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i]->prior_alignment();

  return Pr;
}

efloat_t Parameters::prior() const 
{
  return prior_no_alignment() * prior_alignment();
}

efloat_t Parameters::likelihood() const 
{
  efloat_t Pr = 1;
  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i]->likelihood();
  return Pr;
}

efloat_t Parameters::heated_likelihood() const 
{
  efloat_t Pr = 1;

  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i]->heated_likelihood();

  return Pr;
}

  /// Get the substitution::Model
const Model& Parameters::SModel(int i) const 
{
  return *SModels[i];
}
  /// Get the substitution::Model
      Model& Parameters::SModel(int i)
{
  return *SModels[i];
}


void Parameters::recalc_imodels() 
{
  for(int i=0;i<IModels.size();i++)
    recalc_imodel(i);
}

void Parameters::recalc_imodel(int m) 
{
  for(int i=0;i<data_partitions.size();i++) 
  {
    if (imodel_for_partition[i] == m) {
      // copy our IModel down into the data partition
      data_partitions[i]->IModel_ = IModels[m];

      // recompute cached computations
      data_partitions[i]->recalc_imodel();
    }
  }
}

void Parameters::recalc_smodels() 
{
  for(int i=0;i<SModels.size();i++)
    recalc_smodel(i);
}

void Parameters::recalc_smodel(int m) 
{
  for(int i=0;i<data_partitions.size();i++) 
  {
    if (smodel_for_partition[i] == m) 
    {
      // recompute cached computations
      data_partitions[i]->recalc_smodel();
    }
  }
}

void Parameters::select_root(int b)
{
  for(int i=0;i<data_partitions.size();i++)
    ::select_root(*T, b, data_partitions[i]->LC);
}

void Parameters::set_root(int node)
{
  for(int i=0;i<data_partitions.size();i++)
    data_partitions[i]->LC.root = node;
}

void Parameters::LC_invalidate_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->LC.invalidate_branch(*T,b);
}

void Parameters::LC_invalidate_one_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->LC.invalidate_one_branch(b);
}

void Parameters::LC_invalidate_all()
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->LC.invalidate_all();
}

void Parameters::invalidate_subA_index_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->invalidate_subA_index_branch(b);
}

void Parameters::invalidate_subA_index_one_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->invalidate_subA_index_one_branch(b);
}

void Parameters::invalidate_subA_index_all()
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->invalidate_subA_index_all();
}

void Parameters::subA_index_allow_invalid_branches(bool b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->subA_index_allow_invalid_branches(b);
}

void Parameters::note_alignment_changed_on_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    if (data_partitions[i]->variable_alignment())
      data_partitions[i]->note_alignment_changed_on_branch(b);
}

void Parameters::note_alignment_changed()
{
  for(int i=0;i<n_data_partitions();i++)
    if (data_partitions[i]->variable_alignment())
      data_partitions[i]->note_alignment_changed();
}

void Parameters::note_sequence_length_changed(int n)
{
  for(int i=0;i<n_data_partitions();i++)
    if (data_partitions[i]->variable_alignment())
      data_partitions[i]->note_sequence_length_changed(n);
}

void Parameters::recalc(const vector<int>& indices)
{
  vector<bool> submodel_changed(n_submodels(),false);

  // Check for beta (0) or mu[i] (i+1)
  for(int i=0;i<indices.size();i++)
  {
    int index = indices[i];
    if (not is_super_parameter(index)) continue;

    if (index == 0) // beta
      for(int j=0;j<n_data_partitions();j++)
	data_partitions[j]->set_beta(get_beta());    
    else
    {
      double mu = get_parameter_value_as<Double>(index);
      
      int p = index - 1;
      
      assert(0 <= p and p < n_scales);
      
      for(int j=0;j<scale_for_partition.size();j++)
	if (scale_for_partition[j] == p)
	  data_partitions[j]->branch_mean(mu);
    }
  }

  // Check if any submodels are affected.
  for(int m=0;m<n_submodels();m++) 
    for(int i=0;i<indices.size() and not submodel_changed[m];i++)
      if (parameter_is_used_by_model(indices[i],m))
	submodel_changed[m] = true;

  // Recalculate smodels or imodels if they changed.
  for(int m=0;m<n_submodels();m++) 
  {
    if (not submodel_changed[m]) continue;

    int M = m;
    if (M < n_smodels())
      recalc_smodel(M);
    else
      M -= n_smodels();

    if (M < n_imodels())
      recalc_imodel(M);
    else
      M -= n_imodels();

    // I don't think we need to update any locally cached values if things cached inside
    // a data_partition change.

    // Note that a set_parameter_value( ) leads to 
    // (a) first setting our own value, then setting submodels (via 'write'). (pre-order)
    // (b) first recalcing submodels, then recalcing ourselves (via 'update'). (post-order)
    // So, we don't need to involve recalc on submodels from here.
  }
}

Model& Parameters::SubModels(int i)
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i<SModels.size()) 
    return SModel(i);
  else
    i -= SModels.size();

  if (i<IModels.size()) 
    return IModel(i);
  else
    i -= IModels.size();

  return *data_partitions[i];
}

const Model& Parameters::SubModels(int i) const
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i<SModels.size()) 
    return SModel(i);
  else
    i -= SModels.size();

  if (i<IModels.size()) 
    return IModel(i);
  else
    i -= IModels.size();

  return *data_partitions[i];
}

bool Parameters::variable_alignment() const
{
  for(int i=0;i<n_data_partitions();i++)
    if (data_partitions[i]->variable_alignment())
      return true;
  return false;
}

void Parameters::variable_alignment(bool b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->variable_alignment(b);
}

void Parameters::setlength_no_invalidate_LC(int b,double l) 
{
  for(int i=0;i<data_partitions.size();i++) 
    data_partitions[i]->setlength_no_invalidate_LC(b);
  T->directed_branch(b).set_length(l);
}

void Parameters::setlength(int b,double l) 
{
  for(int i=0;i<data_partitions.size();i++) 
    data_partitions[i]->setlength(b);
  T->directed_branch(b).set_length(l);
}

double Parameters::branch_mean() const 
{
  return 1.0;
}


int Parameters::n_branch_means() const
{
  return n_scales;
}

int Parameters::branch_mean_index(int i) const 
{
  assert(0 <= i and i < n_branch_means());

  return 1+i;
}


void Parameters::branch_mean(int i, double x)
{
  set_parameter_value(branch_mean_index(i),x);
}

void Parameters::branch_mean_tricky(int i,double x)
{
  C.set_parameter_value(branch_mean_index(i), Double(x) );
  
  for(int j=0;j<scale_for_partition.size();j++)
    if (scale_for_partition[j] == i)
      data_partitions[j]->branch_mean_tricky(x);
}

double Parameters::get_branch_duration(int b) const
{
  b = T->directed_branch(b).undirected_name();

  return T->branch(b).length();
}

double Parameters::get_branch_duration(int p, int b) const
{
  return get_branch_duration(b);
}

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
  int s = scale_for_partition[p];
  return get_parameter_value_as<Double>(branch_mean_index(s));
}

double Parameters::get_branch_subst_length(int p, int b) const
{
  double length1 = get_branch_duration(p,b) * get_branch_subst_rate(p,b);

  b = T->directed_branch(b).undirected_name();
  //  int s = scale_for_partition[p];
  //  double length2 = get_parameter_value_as<Double>(branch_length_indices[s][b]);
  //  assert(std::abs(length1 - length2) < 1.0e-8);

  return length1;
}

double Parameters::get_branch_indel_rate(int p, int /* b */) const
{
  int s = scale_for_partition[p];
  return get_parameter_value_as<Double>(branch_mean_index(s));
}

double Parameters::get_branch_indel_length(int p, int b) const
{
  double length1 = get_branch_duration(p,b) * get_branch_subst_rate(p,b);

  b = T->directed_branch(b).undirected_name();
  //  int s = scale_for_partition[p];
  //  double length2 = get_parameter_value_as<Double>(branch_length_indices[s][b]);
  //  assert(std::abs(length1 - length2) < 1.0e-8);

  return length1;
}

Parameters& Parameters::operator=(const Parameters& P)
{
  Model::operator=(P);

  SuperModel::operator=(P);

  Probability_Model::operator=(P);

  SModels = P.SModels;
  smodel_for_partition = P.smodel_for_partition;

  IModels = P.IModels;
  imodel_for_partition = P.imodel_for_partition;

  scale_for_partition = P.scale_for_partition;
  n_scales = P.n_scales;

  branch_prior_type = P.branch_prior_type;

  branch_length_indices = P.branch_length_indices;

  data_partitions = P.data_partitions;

  T = P.T;

  TC = P.TC;

  AC = P.AC;

  branch_HMM_type = P.branch_HMM_type;

  beta_series = P.beta_series;

  all_betas = P.all_betas;
  beta_index = P.beta_index;

  updown = P.updown;

  partitions = P.partitions;
  partition_weights = P.partition_weights;

  constants = P.constants;
  features = P.features;

  branch_length_max = P.branch_length_max;

  for(int i=0;i<data_partitions.size();i++)
    data_partitions[i]->P = this;

  return *this;
}

Parameters::Parameters(const Parameters& P)
  :Model(P),SuperModel(P), Probability_Model(P)
{
  operator=(P);
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<polymorphic_cow_ptr<Model> >& SMs,
		       const vector<int>& s_mapping,
		       const vector<polymorphic_cow_ptr<IndelModel> >& IMs,
		       const vector<int>& i_mapping,
		       const vector<int>& scale_mapping)
  :SModels(SMs),
   smodel_for_partition(s_mapping),
   IModels(IMs),
   imodel_for_partition(i_mapping),
   scale_for_partition(scale_mapping),
   n_scales(max(scale_mapping)+1),
   branch_prior_type(0),
   T(t),
   TC(star_tree(t.get_leaf_labels())),
   branch_HMM_type(t.n_branches(),0),
   updown(-1),
   features(0),
   branch_length_max(-1)
{
  constants.push_back(-1);

  add_super_parameter(Parameter("Heat:beta", Double(1.0), between(0,1)));

  for(int i=0;i<n_scales;i++)
    add_super_parameter(Parameter("mu"+convertToString(i+1), Double(0.25), lower_bound(0)));

  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // register the substitution models as sub-models
  for(int i=0;i<SModels.size();i++) {
    string name = "S" + convertToString(i+1);
    register_submodel(name);
  }

  // register the indel models as sub-models
  for(int i=0;i<IModels.size();i++) {
    string name = "I" + convertToString(i+1);
    register_submodel(name);
  }

  // check that we only map existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SModels.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SModels.size()<<" smodels.";
  }

  // load values from sub-models (smodels/imodel)
  check();

  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC->branch(b).set_length(-1);

  // create data partitions and register as sub-models
  for(int i=0;i<A.size();i++) 
  {
    // compute name for data-partition
    string name = string("part") + convertToString(i+1);

    // get reference to smodel for data-partition
    const Model& SM = SModel(smodel_for_partition[i]);

    // create a data partition
    cow_ptr<data_partition> dp;
    if (imodel_for_partition[i] != -1) {
      const IndelModel& IM = IModel(imodel_for_partition[i]);
      dp = cow_ptr<data_partition>(data_partition(name, this, i, A[i], *T, *SM.result_as<substitution::MultiModelObject>(), IM));
    }
    else 
      dp = cow_ptr<data_partition>(data_partition(name, this, i, A[i], *T, *SM.result_as<substitution::MultiModelObject>()));

    // add the data partition
    data_partitions.push_back(dp);

    // register data partition as sub-model
    register_submodel(name);
  }

  for(int i=0;i<n_scales;i++)
  {
    string prefix= "scale" + convertToString(i+1);
    branch_length_indices.push_back(vector<int>());
    for(int j=0;j<T->n_branches();j++)
    {
      string name = "D" + convertToString(j+1);
      int index = add_parameter(Parameter(prefix+"::"+name, Double(0.0)));
      branch_length_indices[i].push_back(index);
    }
  }
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<polymorphic_cow_ptr<Model> >& SMs,
		       const vector<int>& s_mapping,
		       const vector<int>& scale_mapping)
  :SModels(SMs),
   smodel_for_partition(s_mapping),
   scale_for_partition(scale_mapping),
   branch_prior_type(0),
   T(t),
   TC(star_tree(t.get_leaf_labels())),
   branch_HMM_type(t.n_branches(),0),
   updown(-1),
   features(0),
   branch_length_max(-1)
{
  constants.push_back(-1);

  add_super_parameter(Parameter("Heat:beta", Double(1.0), between(0,1)));

  for(int i=0;i<n_scales;i++)
    add_super_parameter(Parameter("mu"+convertToString(i+1), Double(1.0), lower_bound(0.0)));

  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // register the substitution models as sub-models
  for(int i=0;i<SModels.size();i++) {
    string name = "S" + convertToString(i+1);
    register_submodel(name);
  }

  // NO indel model (in this constructor)

  // check that we only mapping existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SModels.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SModels.size()<<" smodels.";
  }

  // load values from sub-models (smodels/imodel)
  check();

  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC->branch(b).set_length(-1);

  // create data partitions and register as sub-models
  for(int i=0;i<A.size();i++) 
  {
    // compute name for data-partition
    string name = string("part") + convertToString(i+1);

    // get reference to smodel for data-partition
    const Model& SM = SModel(smodel_for_partition[i]);

    // create data partition
    data_partitions.push_back(cow_ptr<data_partition>(data_partition(name, this, i, A[i],*T,*SM.result_as<substitution::MultiModelObject>())));

    // register data partition as sub-model
    register_submodel(name);
  }

  for(int i=0;i<n_scales;i++)
  {
    string prefix= "scale" + convertToString(i+1);
    branch_length_indices.push_back(vector<int>());
    for(int j=0;j<T->n_branches();j++)
    {
      string name = "D" + convertToString(j+1);
      int index = add_parameter(Parameter(prefix+"::"+name, Double(0.0)));
      branch_length_indices[i].push_back(index);
    }
  }
}

bool accept_MH(const Probability_Model& P1,const Probability_Model& P2,double rho)
{
  efloat_t p1 = P1.heated_probability();
  efloat_t p2 = P2.heated_probability();

  efloat_t ratio = efloat_t(rho)*(p2/p1);

  if (ratio >= 1.0 or myrandomf() < ratio) 
    return true;
  else
    return false;
}

