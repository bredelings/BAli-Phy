/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

void data_partition::variable_alignment(bool b)
{
  variable_alignment_ = b;

  // Ignore requests to turn on alignment variation when there is no imodel or internal nodes
  if (not has_IModel() or A->n_sequences() != T->n_nodes())
    variable_alignment_ = false;

  // turning OFF alignment variation
  if (not variable_alignment()) 
  {
    if (A->n_sequences() == T->n_nodes())
      if (not check_leaf_characters_minimally_connected(*A,*T))
	throw myexception()<<"Failing to turn off alignment variability: non-default internal node states";
  }
  // turning ON alignment variation
  else 
  {
    assert(has_IModel() and A->n_sequences() == T->n_nodes());
    minimally_connect_leaf_characters(*A,*T);
    note_alignment_changed();

    // we need to calculate the branch_HMMs
    recalc_imodel();

    // Minimally connecting leaf characters may remove empty columns, in theory.
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

void data_partition::recalc_imodel() 
{
  if (not variable_alignment()) return;

  cached_alignment_prior.invalidate();

  for(int b=0;b<cached_alignment_prior_for_branch.size();b++)
    cached_alignment_prior_for_branch[b].invalidate();

  for(int b=0;b<branch_HMMs.size();b++) 
  {
    // use the length, unless we are unaligned
    double t = T->branch(b).length();
    
    // compute and cache the branch HMM
    if (branch_HMM_type[b] == 1)
      branch_HMMs[b] = IModel_->get_branch_HMM(-1);
    else
      branch_HMMs[b] = IModel_->get_branch_HMM(t*branch_mean());
  }
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
  // set the rate to one
  // FIXME - we COPY the smodel here!
  SModel_->set_rate(branch_mean());

  //invalidate cached conditional likelihoods in case the model has changed
  LC.invalidate_all();

  //invalidate the cached transition probabilities in case the model has changed
  MC.recalc(*T,*SModel_);
  default_timer_stack.pop_timer();
}

void data_partition::setlength_no_invalidate_LC(int b, double l)
{
  default_timer_stack.push_timer("setlength_no_invalidate_LC( )");
  b = T->directed_branch(b).undirected_name();

  MC.setlength(b,l,*T,*SModel_); 

  if (variable_alignment())
  {
    // use the length, unless we are unaligned
    double t = T->branch(b).length();

    if (branch_HMM_type[b] == 1)
      branch_HMMs[b] = IModel_->get_branch_HMM(-1);
    else
      branch_HMMs[b] = IModel_->get_branch_HMM(t*branch_mean());

    cached_alignment_prior.invalidate();
    cached_alignment_prior_for_branch[b].invalidate();
  }
  default_timer_stack.pop_timer();
}

void data_partition::setlength(int b, double l)
{
  setlength_no_invalidate_LC(b,l);
  LC.invalidate_branch(*T,b);
}

int data_partition::seqlength(int n) const
{
  if (not cached_sequence_lengths[n].is_valid())
    cached_sequence_lengths[n] = A->seqlength(n);

  assert(cached_sequence_lengths[n] == A->seqlength(n));

  return cached_sequence_lengths[n];
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

  b = T->directed_branch(b).undirected_name();

  cached_alignment_prior.invalidate();
  cached_alignment_prior_for_branch[b].invalidate();
  cached_alignment_counts_for_branch[b].invalidate();

  const Tree& TT = *T;
  int target = TT.branch(b).target();
  int source = TT.branch(b).source();

  if (target >= TT.n_leaves())
    note_sequence_length_changed(target);
  if (source >= TT.n_leaves())
    note_sequence_length_changed(source);
}

void data_partition::note_alignment_changed()
{
  for(int b=0;b<T->n_branches();b++)
    note_alignment_changed_on_branch(b);

  // this automatically marks all non-leaf sequence lengths for recomputation.
}

void data_partition::recalc(const vector<int>& indices)
{
  if (indices.size())
    throw myexception()<<"What parameter is this???";
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
  // scale the substitution rate
  // FIXME - we COPY the smodel here!
  SModel_->set_rate(branch_mean());
}

string data_partition::name() const 
{
  return partition_name;
}

efloat_t data_partition::prior_no_alignment() const 
{
  return 1.0;
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
    const SequenceTree& TT = *T;

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
	ublas::matrix<int>& counts = cached_alignment_counts_for_branch[b];
	cached_alignment_prior_for_branch[b] = prior_branch_from_counts(counts, branch_HMMs[b]);
      }
#ifndef NDEBUG      
      int target = TT.branch(b).target();
      int source  = TT.branch(b).source();
      efloat_t p1 = cached_alignment_prior_for_branch[b];
      efloat_t p2 = prior_branch(AA, branch_HMMs[b], target, source);
      double error = log(p1) - log(p2);
      assert(not different(cached_alignment_prior_for_branch[b], prior_branch(AA, branch_HMMs[b], target, source)));
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
  if (smodel_full_tree)
    return substitution::Pr(*this);
  else
    return substitution::Pr_star(*this);
}

efloat_t data_partition::heated_likelihood() const 
{
  return pow(likelihood(),beta[0]);
}

data_partition::data_partition(const string& n, const alignment& a,const SequenceTree& t,
			       const substitution::MultiModel& SM,const IndelModel& IM)
  :IModel_(IM),
   SModel_(SM),
   partition_name(n),
   cached_alignment_prior_for_branch(t.n_branches()),
   cached_alignment_counts_for_branch(t.n_branches(),ublas::matrix<int>(5,5)),
   cached_sequence_lengths(a.n_sequences()),
   branch_mean_(1.0),
   variable_alignment_(true),
   smodel_full_tree(true),
   A(a),
   T(t),
   MC(t,SM),
   LC(t,SModel()),
   branch_HMMs(t.n_branches()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0)
{
  for(int b=0;b<cached_alignment_counts_for_branch.size();b++)
    cached_alignment_counts_for_branch[b].invalidate();
}

data_partition::data_partition(const string& n, const alignment& a,const SequenceTree& t,
			       const substitution::MultiModel& SM)
  :SModel_(SM),
   partition_name(n),
   cached_alignment_prior_for_branch(t.n_branches()),
   cached_alignment_counts_for_branch(t.n_branches(),ublas::matrix<int>(5,5)),
   cached_sequence_lengths(a.n_sequences()),
   branch_mean_(1.0),
   variable_alignment_(false),
   smodel_full_tree(true),
   A(a),
   T(t),
   MC(t,SM),
   LC(t,SModel()),
   branch_HMMs(t.n_branches()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0)
{
  for(int b=0;b<cached_alignment_counts_for_branch.size();b++)
    cached_alignment_counts_for_branch[b].invalidate();
}

//-----------------------------------------------------------------------------//


efloat_t Parameters::prior_no_alignment() const 
{
  efloat_t Pr = 1.0;

  // prior on the topology and branch lengths
  Pr *= ::prior(*this, *T, 1.0);

  // prior on mu[i], the mean branch length for scale i
  for(int i=0;i<n_branch_means();i++) {
    //  return pow(efloat_t(branch_mean()),-1.0);
    Pr *= gamma_pdf(branch_mean(i), 0.5, 2.0);
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
  // set the rate to one
  SModels[m]->set_rate(1);
  read();

  for(int i=0;i<data_partitions.size();i++) 
  {
    if (smodel_for_partition[i] == m) {
      // copy our IModel down into the data partition
      data_partitions[i]->SModel_ = SModels[m];

      // recompute cached computations
      data_partitions[i]->recalc_smodel();
    }
  }
}

void Parameters::select_root(int b)
{
  for(int i=0;i<data_partitions.size();i++)
    ::select_root(*data_partitions[i]->T, b, data_partitions[i]->LC);
}

void Parameters::set_root(int node)
{
  for(int i=0;i<data_partitions.size();i++)
    data_partitions[i]->LC.root = node;
}

void Parameters::tree_propagate()
{
  for(int i=0;i<n_data_partitions();i++) 
    data_partitions[i]->T = T;
}

void Parameters::LC_invalidate_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->LC.invalidate_branch(*data_partitions[i]->T,b);
}

void Parameters::LC_invalidate_one_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i]->LC.invalidate_one_branch(b);
}

void Parameters::invalidate_subA_index_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    ::invalidate_subA_index_branch(*data_partitions[i]->A,*data_partitions[i]->T,b);
}

void Parameters::invalidate_subA_index_one_branch(int b)
{
  int b2 = T->directed_branch(b).reverse();
  for(int i=0;i<n_data_partitions();i++) {
    ::invalidate_subA_index_one(*data_partitions[i]->A,b);
    ::invalidate_subA_index_one(*data_partitions[i]->A,b2);
  }
}

void Parameters::invalidate_subA_index_all()
{
  for(int i=0;i<n_data_partitions();i++)
    ::invalidate_subA_index_all(*data_partitions[i]->A);
}

void Parameters::subA_index_allow_invalid_branches(bool b)
{
#ifndef NDEBUG
  if (subA_index_may_have_invalid_branches())
  {
    for(int i=0;i<n_data_partitions();i++) {
      subA_index_check_footprint(*data_partitions[i]->A, *T);
      subA_index_check_regenerate(*data_partitions[i]->A, *T);
    }
  }
#endif
  ::subA_index_allow_invalid_branches(b);

#ifndef NDEBUG
  if (not subA_index_may_have_invalid_branches())
  {
    for(int i=0;i<n_data_partitions();i++) {
      subA_index_check_footprint(*data_partitions[i]->A, *T);
      subA_index_check_regenerate(*data_partitions[i]->A, *T);
    }
  }
#endif
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

  for(int i=0;i<indices.size();i++) 
  {
    int m = model_of_index[indices[i]];

    if (m == -1) {
      int s = indices[i];
      assert(0 <= s and s < n_scales);

      double mu = parameter(s);

      for(int j=0;j<scale_for_partition.size();j++)
	if (scale_for_partition[j] == s)
	  data_partitions[j]->branch_mean(mu);
    }
    else
      submodel_changed[m] = true;
  }

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

    // no need to call recalc for change in data-partition parameters? 
    // (just part?::mu, I think, for now)  
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
  T->directed_branch(b).set_length(l);
  for(int i=0;i<data_partitions.size();i++) 
    data_partitions[i]->setlength_no_invalidate_LC(b,l);
}

void Parameters::setlength(int b,double l) 
{
  T->directed_branch(b).set_length(l);
  for(int i=0;i<data_partitions.size();i++) 
    data_partitions[i]->setlength(b,l);
}

double Parameters::branch_mean() const 
{
  return 1.0;
}


int Parameters::n_branch_means() const
{
  return n_scales;
}

double Parameters::branch_mean(int i) const 
{
  assert(0 <= i and i < n_branch_means());

  return parameter(i);
}


void Parameters::branch_mean_tricky(int i,double x)
{
  assert(0 <= i and i < n_branch_means());

  parameters_[i] = x;
  
  for(int j=0;j<scale_for_partition.size();j++)
    if (scale_for_partition[j] == i)
      data_partitions[j]->branch_mean_tricky(x);
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<polymorphic_cow_ptr<substitution::MultiModel> >& SMs,
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
   smodel_full_tree(true),
   T(t),
   TC(star_tree(t.get_sequences())),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0),
   updown(-1),
   features(0) 
{
  constants.push_back(-1);

  for(int i=0;i<n_scales;i++)
    add_super_parameter("mu"+convertToString(i+1),1.0);

  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // register the substitution models as sub-models
  for(int i=0;i<SModels.size();i++) {
    string name = "S" + convertToString(i+1);
    add_submodel(name, *SModels[i]);
  }

  // register the indel models as sub-models
  for(int i=0;i<IModels.size();i++) {
    string name = "I" + convertToString(i+1);
    add_submodel(name, *IModels[i]);
  }

  // check that we only map existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SModels.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SModels.size()<<" smodels.";
  }

  // load values from sub-models (smodels/imodel)
  read();

  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC->branch(b).set_length(-1);

  // create data partitions and register as sub-models
  for(int i=0;i<A.size();i++) 
  {
    // compute name for data-partition
    string name = string("part") + convertToString(i+1);

    // get reference to smodel for data-partition
    const substitution::MultiModel& SM = SModel(smodel_for_partition[i]);

    // create a data partition
    cow_ptr<data_partition> dp;
    if (imodel_for_partition[i] != -1) {
      const IndelModel& IM = IModel(imodel_for_partition[i]);
      dp = cow_ptr<data_partition>(data_partition(name,A[i],*T,SM,IM));
    }
    else 
      dp = cow_ptr<data_partition>(data_partition(name,A[i],*T,SM));

    // add the data partition
    data_partitions.push_back(dp);

    // register data partition as sub-model
    add_submodel(name,*data_partitions[i]);
  }
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<polymorphic_cow_ptr<substitution::MultiModel> >& SMs,
		       const vector<int>& s_mapping,
		       const vector<int>& scale_mapping)
  :SModels(SMs),
   smodel_for_partition(s_mapping),
   scale_for_partition(scale_mapping),
   branch_prior_type(0),
   smodel_full_tree(true),
   T(t),
   TC(star_tree(t.get_sequences())),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0),
   updown(-1),
   features(0)
{
  constants.push_back(-1);

  for(int i=0;i<n_scales;i++)
    add_super_parameter("mu"+convertToString(i+1),1.0);

  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // register the substitution models as sub-models
  for(int i=0;i<SModels.size();i++) {
    string name = "S" + convertToString(i+1);
    add_submodel(name, *SModels[i]);
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
  read();

  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC->branch(b).set_length(-1);

  // create data partitions and register as sub-models
  for(int i=0;i<A.size();i++) 
  {
    // compute name for data-partition
    string name = string("part") + convertToString(i+1);

    // get reference to smodel for data-partition
    const substitution::MultiModel& SM = SModel(smodel_for_partition[i]);

    // create data partition
    data_partitions.push_back(cow_ptr<data_partition>(data_partition(name,A[i],*T,SM)));

    // register data partition as sub-model
    add_submodel(name,*data_partitions[i]);
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

