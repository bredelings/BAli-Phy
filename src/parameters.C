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
/// \file parameters.C
///
/// \brief This file implements the Parameters class which holds the model and state
///        for the MCMC.
///

#include "parameters.H"
#include "smodel/smodel.H"
#include "rng.H"
#include "substitution/substitution.H"
#include "substitution/substitution-index.H"
#include "alignment/alignment-util.H"
#include "likelihood.H"
#include "util.H"
#include "mcmc/proposals.H"
#include "probability.H"
#include "computation/formula_expression.H"
#include "smodel/operations.H"
#include "computation/prelude.H"
#include "computation/program.H"
#include "computation/operations.H"
#include "exponential.H"
#include "smodel/functions.H"
#include "distribution-operations.H"

using std::vector;
using std::string;
using std::cerr;
using std::endl;
using std::ostream;

/*
 * Goal: Construct a complete tree-based imodel along the lines of
 *       SingleRate[RS07] or BranchwiseRate[RS07]
 * 
 * [DONE]. Make get_branch_hmm properly dependent on the scale!
 * 1. Export names like substitutionBranchLengths, branchDuration
 * 2. Give the setup routine for imodels the number of branches
 *    (\b,h,t -> RS07 e lambda*sustitutionBranchLengths!b h t, \e,l -> RS07_lengthp e l)
 * 3. Allow defining things in a formula_expression
 *    (f_e is becoming more like a program!)
 * 4. Remove code for caching and updating branch_hmms
 * 5. Remove imodels as sub-models.
 * 6. Remove class SuperModel?
 * 7. Remove imodel_methods.{lambda,epsilon}
 *
 *    indelRates = listArray B [lambda1, lambda2, lambda3 ... lambdaB]
 *    
 *    traditionally, we could have indelRates = mkArray B \b -> lambda
 * 8. Do all the names we add make it into the program??
 *   - Yes, when we do Model::add_parameter( ), or Context::add_parameter( ), something is added to the program.
 * 9. Move the mapping from identifiers to locations from Context to reg_heap.
 *   - No, wait, these locations may indeed change, if they depend on parameters, right?
 *   - Thus, we could do this, but the locations would NOT be allowed to depend on parameters!
 *   - Perhaps we could separate them into (a) dependent and (b) non-dependent vars?
 * 10. Move the Program from Context to reg_heap.
 */

bool use_internal_index = true;

const SequenceTree& data_partition::T() const
{
  return *P->T;
}


double data_partition::get_beta() const
{
  return P->get_beta();
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
    minimally_connect_leaf_characters(*A.modify(), T());
    note_alignment_changed();

    // we need to calculate the branch_HMMs
    recalc_imodel();

    // Minimally connecting leaf characters may remove empty columns, in theory.
    // And we just changed the subA index type
    LC.invalidate_all();
  }
}

bool data_partition::has_IModel() const
{
  int m = P->imodel_for_partition[partition_index];
  return (m != -1);
}

const IndelModel& data_partition::IModel() const
{
  int m = P->imodel_for_partition[partition_index];
  if (m == -1)
    std::abort();
  else
    return P->IModel(m);
}

const std::vector<Matrix>& data_partition::transition_P(int b) const
{
  b = T().directed_branch(b).undirected_name();
  assert(b >= 0 and b < T().n_branches());

  return *P->C.evaluate_as<Box<vector<Matrix>>>( transition_p_method_indices[b] );
}

int data_partition::n_base_models() const
{
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->C.evaluate(P->SModels[s].n_base_models);
  return *convert<const Int>(O);
}

int data_partition::n_states() const
{
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->C.evaluate(P->SModels[s].n_states);
  return *convert<const Int>(O);
}

vector<double> data_partition::distribution() const
{
  // Add Op to convert list to vector<Double>
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->C.evaluate(P->SModels[s].distribution);
  return *convert<const Box<vector<double>>>(O);
}

vector<unsigned> data_partition::state_letters() const
{
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->C.evaluate(P->SModels[s].state_letters);
  return *convert<const Box<vector<unsigned> > >(O);
}

vector<double> data_partition::frequencies(int m) const
{
  return *P->C.evaluate_as<Vector<double>>( frequencies_indices[m] );
}

object_ptr<const Object> data_partition::base_model(int m, int b) const
{
  b = T().directed_branch(b).undirected_name();

  return P->C.evaluate( base_model_indices(m,b) );
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

    int m = P->imodel_for_partition[partition_index];

    // compute and cache the branch HMM
    if (branch_HMM_type[b] == 1)
      HMM = IModel().get_branch_HMM(-1);
    else
    {
      indel::PairHMM other = IModel().get_branch_HMM(D);
      HMM = *P->C.evaluate_as<indel::PairHMM>( branch_HMM_indices[b] );
      for(int i=0;i<other.size1();i++)
	for(int j=0;j<other.size2();j++)
	  assert(std::abs(other(i,j) - HMM.value()(i,j)) < 0.00001);
    }
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

double data_partition::sequence_length_pr(int l) const
{
  int m = P->imodel_for_partition[partition_index];

  int arg_param_index = P->IModel_methods[m].length_arg_param_index;

  const_cast<Parameters*>(P)->set_parameter_value(arg_param_index, new Int(l) );

  double pr1 = *P->C.evaluate_as<Double>( P->IModel_methods[m].length_p );
  double pr2 = IModel().lengthp(l);
  assert( std::abs( log(pr2) - log(pr1)) < 0.00000001 );

  return pr1;
}

void data_partition::recalc_imodel_for_branch(int b)
{
  if (not variable_alignment()) return;

  // FIXME #1 - this used to go along with computation of the branch_HMMs[].
  // Is it OK to move it here?
  //
  // FIXME #2 - IModel_ should be branch-specific.
  //  IModel_.modify()->set_heat( get_beta() );

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
  //invalidate cached conditional likelihoods in case the model has changed
  LC.invalidate_all();
}

void data_partition::setlength_no_invalidate_LC(int b)
{
  b = T().directed_branch(b).undirected_name();

  recalc_imodel_for_branch(b);
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

void data_partition::set_pairwise_alignment_(int b, const pairwise_alignment_t& pi,bool require_match_A) const
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

  if (require_match_A)
  {
    int n1 = T().directed_branch(b).source();
    int n2 = T().directed_branch(b).target();
    assert(pairwise_alignment_for_branch[b] == A2::get_pairwise_alignment(*A,n1,n2));
    assert(pairwise_alignment_for_branch[B] == A2::get_pairwise_alignment(*A,n2,n1));
  }
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b, bool require_match_A) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: what pairwise alignment are you referring to?";

#ifndef NDEBUG
  int B = T().directed_branch(b).reverse();
#endif

  if (pairwise_alignment_for_branch[b].is_valid())
  {
#ifndef NDEBUG
    if (require_match_A)
    {
      int n1 = T().directed_branch(b).source();
      int n2 = T().directed_branch(b).target();
      assert(pairwise_alignment_for_branch[b] == A2::get_pairwise_alignment(*A,n1,n2));
      assert(pairwise_alignment_for_branch[B].is_valid());
      assert(pairwise_alignment_for_branch[B] == A2::get_pairwise_alignment(*A,n2,n1));
    }
#endif
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

void data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi, bool require_match_A)
{
  note_alignment_changed_on_branch(b);
  set_pairwise_alignment_(b,pi,require_match_A);
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

/// Set the mean branch length to \a mu
void data_partition::branch_mean_changed()
{
  // the scale of the substitution tree changed
  recalc_smodel();

  // the scale of the indel tree changed also
  recalc_imodel();
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

data_partition::data_partition(const string& n, Parameters* p, int i, const alignment& a)
  :P(p),
   partition_index(i),
   partition_name(n),
   cached_alignment_prior_for_branch(T().n_branches()),
   pairwise_alignment_for_branch(2*T().n_branches()),
   cached_alignment_counts_for_branch(T().n_branches(),ublas::matrix<int>(5,5)),
   cached_sequence_lengths(a.n_sequences()),
   cached_branch_HMMs(T().n_branches()),
   transition_p_method_indices(T().n_branches(),-1),
   variable_alignment_( has_IModel() ),
   sequences( alignment_letters(a,T().n_leaves()) ),
   A(a),
   LC(T(), *this),
   branch_HMM_type(T().n_branches(),0)
{
  int B = T().n_branches();

  if (variable_alignment() and use_internal_index)
    subA = subA_index_internal(a.length()+1, B*2);
  else
    subA = subA_index_leaf(a.length()+1, B*2);

  for(int b=0;b<cached_alignment_counts_for_branch.size();b++)
    cached_alignment_counts_for_branch[b].invalidate();

  // Add method indices for calculating transition matrices.
  const int n_models = n_base_models();
  //  const int n_states = state_letters().size();
  for(int b=0;b<B;b++)
  {
    int s = P->scale_for_partition[partition_index];
    int m = P->smodel_for_partition[partition_index];

    expression_ref E = P->C.get_expression(P->branch_transition_p_indices(s,m));
    E = (getIndex, E, b);

    transition_p_method_indices[b] = p->C.add_compute_expression(E);
  }

  // Add method indices for calculating base models and frequencies
  base_model_indices.resize(n_models, B);
  for(int m=0;m<n_models;m++)
  {
    int s = P->smodel_for_partition[partition_index];
    expression_ref F = P->C.get_expression(P->SModels[s].frequencies);
    frequencies_indices.push_back( p->C.add_compute_expression( (F,m) ) );

    expression_ref BM = P->C.get_expression(P->SModels[s].base_model);
    for(int b=0;b<B;b++)
      base_model_indices(m,b) = p->C.add_compute_expression((BM,m,b));
  }

  // Add method indices for calculating branch HMMs
  int i_index = P->imodel_for_partition[partition_index];
  const imodel_methods& I = P->IModel_methods[i_index];

  expression_ref RS07BranchHMM = lambda_expression( RS07_branch_HMM() );
  expression_ref epsilon = (var("exp"), parameter(I.epsilon));
  expression_ref lambda = (var("exp"), parameter(I.lambda));

  expression_ref heat = parameter("Heat.beta");
  expression_ref training = parameter("IModels.training");
  
  int scale_index = P->scale_for_partition[partition_index];

  for(int b=0;b<B;b++)
  {
    expression_ref Db = parameter("Scale" + convertToString(scale_index+1) + ".d" + convertToString(b+1));
    branch_HMM_indices.push_back(  p->C.add_compute_expression( (RS07BranchHMM, epsilon, lambda * Db, heat, training) ) );
  }
}

//-----------------------------------------------------------------------------//
smodel_methods::smodel_methods(const expression_ref& E, context& C)
{
  expression_ref V = listToVectorDouble;

  main = C.add_compute_expression( E );
  expression_ref S = C.get_expression(main);

  n_base_models = C.add_compute_expression((::n_base_models, S));
  n_states =  C.add_compute_expression((::n_states, S));
  distribution =  C.add_compute_expression((V,(::distribution, S)));
  get_alphabet = C.add_compute_expression((::get_alphabet, S));
  state_letters = C.add_compute_expression((::state_letters, S));
  n_states = C.add_compute_expression((::n_states, S));
  rate = C.add_compute_expression((::rate, S));

  base_model = C.add_compute_expression( v1^(v2^(::base_model, (get_nth_mixture,S,v2), v1) ) );
  frequencies = C.add_compute_expression((::get_component_frequencies, S));
  transition_p = C.add_compute_expression((::branch_transition_p, S));
}

const data_partition& Parameters::get_data_partition(int i) const
{
  data_partitions[i].P = this;
  return data_partitions[i];
}

data_partition& Parameters::get_data_partition(int i)
{
  data_partitions[i].P = this;
  return data_partitions[i];
}

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
  efloat_t Pr = Model::prior();

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
  for(int i=0;i<n_data_partitions();i++)
    Pr *= get_data_partition(i).prior_no_alignment();

  return Pr;
}

efloat_t Parameters::prior_alignment() const 
{
  efloat_t Pr = 1;

  for(int i=0;i<n_data_partitions();i++) 
    Pr *= get_data_partition(i).prior_alignment();

  return Pr;
}

efloat_t Parameters::prior() const 
{
  return prior_no_alignment() * prior_alignment();
}

efloat_t Parameters::likelihood() const 
{
  efloat_t Pr = 1;
  for(int i=0;i<n_data_partitions();i++) 
    Pr *= get_data_partition(i).likelihood();
  return Pr;
}

efloat_t Parameters::heated_likelihood() const 
{
  efloat_t Pr = 1;

  for(int i=0;i<n_data_partitions();i++) 
    Pr *= get_data_partition(i).heated_likelihood();

  return Pr;
}

void Parameters::recalc_imodels() 
{
  for(int i=0;i<IModels.size();i++)
    recalc_imodel(i);
}

void Parameters::recalc_imodel(int m) 
{
  for(int i=0;i<n_data_partitions();i++) 
  {
    if (imodel_for_partition[i] == m) {
      // recompute cached computations
      get_data_partition(i).recalc_imodel();
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
  for(int i=0;i<n_data_partitions();i++) 
  {
    if (smodel_for_partition[i] == m) 
    {
      // recompute cached computations
      get_data_partition(i).recalc_smodel();
    }
  }
}

void Parameters::select_root(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    ::select_root(*T, b, get_data_partition(i).LC);
}

void Parameters::set_root(int node)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.root = node;
}

void Parameters::LC_invalidate_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.invalidate_branch(*T,b);
}

void Parameters::LC_invalidate_one_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.invalidate_one_branch(b);
}

void Parameters::LC_invalidate_all()
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.invalidate_all();
}

void Parameters::invalidate_subA_index_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).invalidate_subA_index_branch(b);
}

void Parameters::invalidate_subA_index_one_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).invalidate_subA_index_one_branch(b);
}

void Parameters::invalidate_subA_index_all()
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).invalidate_subA_index_all();
}

void Parameters::subA_index_allow_invalid_branches(bool b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).subA_index_allow_invalid_branches(b);
}

void Parameters::note_alignment_changed_on_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      get_data_partition(i).note_alignment_changed_on_branch(b);
}

void Parameters::note_alignment_changed()
{
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      get_data_partition(i).note_alignment_changed();
}

void Parameters::note_sequence_length_changed(int n)
{
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      get_data_partition(i).note_sequence_length_changed(n);
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
    {
      for(int m=0;m<n_imodels();m++)
	IModel(m).set_heat( get_beta() );
      for(int p=0;p<n_data_partitions();p++)
	get_data_partition(p).recalc_imodel();
    }
    else if (index < n_scales+1)
    {
      int s = index - 1;
      
      assert(0 <= s and s < n_scales);
      
      // Change branch lengths for the s-th scale
      assert(branch_length_indices[s].size() == T->n_branches());
      for(int b=0;b<T->n_branches();b++)
      {
	double rate = *convert<const Double>(C.get_parameter_value(branch_mean_index(s)));
	double delta_t = T->branch(b).length();

	C.set_parameter_value(branch_length_indices[s][b], Double(rate*delta_t));
      }

      // notify partitions with scale 'p' that their branch mean changed
      for(int p=0;p<scale_for_partition.size();p++)
      {
	if (scale_for_partition[p] == s)
	  get_data_partition(p).branch_mean_changed();
      }
    }
    else if (n_imodels() and index < n_scales+4)
      for(int m=0;m<n_imodels();m++) 
	recalc_imodel(m);
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

  // Check if any substitution models have changed.
  for(int s=0;s<n_smodels();s++)
    if (not C.compute_expression_is_up_to_date(SModels[s].main))
      recalc_smodel(s);
}

object_ptr<const alphabet> Parameters::get_alphabet_for_smodel(int s) const
{
  return convert<const alphabet>(C.evaluate(SModels[s].get_alphabet));
}

Model& Parameters::SubModels(int i)
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i<IModels.size()) 
    return IModel(i);

  std::abort();
}

const Model& Parameters::SubModels(int i) const
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i<IModels.size()) 
    return IModel(i);

  std::abort();
}

bool Parameters::variable_alignment() const
{
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      return true;
  return false;
}

void Parameters::variable_alignment(bool b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).variable_alignment(b);
}

void Parameters::setlength_no_invalidate_LC(int b,double l) 
{
  b = T->directed_branch(b).undirected_name();
  T.modify()->directed_branch(b).set_length(l);

  // Update D parameters
  for(int s=0; s<n_scales; s++) 
  {
    double rate = *convert<const Double>(C.get_parameter_value(branch_mean_index(s)));
    double delta_t = T->branch(b).length();
    
    C.set_parameter_value(branch_length_indices[s][b], rate * delta_t);
  }

  for(int i=0;i<n_data_partitions();i++) 
    get_data_partition(i).setlength_no_invalidate_LC(b);
}

void Parameters::setlength(int b,double l) 
{
  b = T->directed_branch(b).undirected_name();
  T.modify()->directed_branch(b).set_length(l);

  // Update D parameters
  for(int s=0; s<n_scales; s++) 
  {
    double rate = *convert<const Double>(C.get_parameter_value(branch_mean_index(s)));
    double delta_t = T->branch(b).length();
    
    C.set_parameter_value(branch_length_indices[s][b], rate * delta_t);
  }

  for(int p=0;p<n_data_partitions();p++) 
    get_data_partition(p).setlength(b);
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

// Change the branch_mean for the i-th scale (and all partitions using it) without
// invalidating anything. How do we do this?
// I think we do it by not going through set_parameter_value( ) as above.
void Parameters::branch_mean_tricky(int i,double x)
{
  C.set_parameter_value(branch_mean_index(i), Double(x) );
}

double Parameters::get_branch_duration(int b) const
{
  b = T->directed_branch(b).undirected_name();

  return T->branch(b).length();
}

double Parameters::get_branch_duration(int /* p */, int b) const
{
  // This would only depend on p if we allowed (say) different branch lengths in different partitions.
  // Which we do not.
  return get_branch_duration(b);
}

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
  int s = scale_for_partition[p];
  return get_parameter_value_as<Double>(branch_mean_index(s));
}

double Parameters::get_branch_subst_length(int p, int b) const
{
  int s = scale_for_partition[p];
  b = T->directed_branch(b).undirected_name();
  double length2 = get_parameter_value_as<Double>(branch_length_indices[s][b]);

#ifndef NDEBUG
  double length1 = get_branch_duration(p,b) * get_branch_subst_rate(p,b);
  assert(std::abs(length1 - length2) < 1.0e-10);
#endif

  return length2;
}

double Parameters::get_branch_indel_rate(int p, int b) const
{
  assert(n_imodels() > 0);

  double r = get_branch_subst_rate(p, b);

  double indel_scale_by = 1.0;

  // determine scaling factor.
  int offset = n_scales+1;

  bool indel_scale_on = get_parameter_value_as<Bool>(offset+1);
  int indel_scale_branch = get_parameter_value_as<Int>(offset+2);
  if (indel_scale_on and indel_scale_branch == b)
    indel_scale_by = exp( get_parameter_value_as<Double>(offset+0) );

  return r * indel_scale_by;
}

double Parameters::get_branch_indel_length(int p, int b) const
{
  double length1 = get_branch_duration(p,b) * get_branch_indel_rate(p,b);

  b = T->directed_branch(b).undirected_name();

  //  int s = scale_for_partition[p];
  //  double length2 = get_parameter_value_as<Double>(branch_length_indices[s][b]);
  //  assert(std::abs(length1 - length2) < 1.0e-8);

  return length1;
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<formula_expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<polymorphic_cow_ptr<IndelModel> >& IMs,
		       const vector<int>& i_mapping,
		       const vector<int>& scale_mapping)
  :smodel_for_partition(s_mapping),
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
  C += SModel_Functions();
  C += Distribution_Functions();
  // Don't call set_parameter_value here, because recalc( ) depends on branch_length_indices, which is not ready.

  constants.push_back(-1);

  add_super_parameter(Parameter("Heat.beta", Double(1.0), between(0,1)));

  for(int i=0;i<n_scales;i++)
    add_super_parameter(Parameter("mu"+convertToString(i+1), Double(0.25), lower_bound(0)));

  // create parameters for scaling indel model on a specific branch
  if (IMs.size())
  {
    add_super_parameter(Parameter("lambdaScale", Double(0.0)));
    // lambda_scale ~ Laplace(0, 1)
    add_note((distributed,parameter("lambdaScale"),Tuple(laplace_dist,Tuple(0.0, 1.0))));

    add_super_parameter(Parameter("lambdaScaleOn", Bool(false)));
    add_note((distributed,parameter("lambdaScaleOn"),Tuple(bernoulli_dist,0.5)));
    // lambda_scale_on ~ Uniform on T,F

    add_super_parameter(Parameter("lambdaScaleBranch", Int(-1), between(0,T->n_branches()-1)));
    //lambda_scale_branch ~ Uniform on 0 .. T.n_branches()-1
  }

  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // register the substitution models as sub-models
  for(int i=0;i<SMs.size();i++) {
    string name = "S" + convertToString(i+1);
    formula_expression_ref S = prefix_formula(name,SMs[i]);

    std::set<string> names = find_named_parameters(S.get_notes_plus_exp());
    for(const auto& name: names)
      if (find_parameter(name) == -1)
	add_super_parameter(name);

    for(int j=0;j<S.n_notes();j++)
      add_note(S.get_note(j));

    // Set default values.
    for(const auto& name: names)
    {
      int index = find_parameter(name);
      if (not C.parameter_is_set(index))
	C.set_parameter_value(index, C.default_parameter_value(index));
    }

    SModels.push_back( smodel_methods(S.exp(), C) );
  }

  // register the indel models as sub-models
  for(int i=0;i<IModels.size();i++) 
  {
    string name = "I" + convertToString(i+1);
    register_submodel(name);

    imodel_methods I;
    I.lambda = name+".lambda";
    I.epsilon = name+".epsilon";
    IModel_methods.push_back(I);
  }

  // check that we only map existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SModels.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SModels.size()<<" smodels.";
  }

  // Add commands to log everything.
  expression_ref make_logger = constructor("make_logger",1);
  for(int i=0;i<n_parameters();i++)
    add_note( (make_logger,parameter_name(i)) );

  // load values from sub-models (smodels/imodel)
  check();

  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC.modify()->branch(b).set_length(-1);

  // Add and initialize variables for branch *lengths*: scale<s>::D<b>
  for(int s=0;s<n_scales;s++)
  {
    string prefix= "Scale" + convertToString(s+1);
    branch_length_indices.push_back(vector<int>());
    for(int b=0;b<T->n_branches();b++)
    {
      double rate = *convert<const Double>(C.get_parameter_value(branch_mean_index(s)));
      double delta_t = T->branch(b).length();

      string name = "d" + convertToString(b+1);
      int index = add_parameter(Parameter(prefix+"."+name, Double(rate * delta_t)));
      branch_length_indices[s].push_back(index);
    }
  }

  // Add and initialize variables for branch *categories*: branch_cat<b>
  vector<expression_ref> branch_categories;
  for(int b=0;b<T->n_branches();b++)
  {
    string name = "branchCat" + convertToString(b+1);
    add_parameter(Parameter(name, Int(0)));
    branch_categories.push_back(parameter(name));
  }
  expression_ref branch_cat_list = C.get_expression( C.add_compute_expression( (get_list(branch_categories) ) ) );
  
  // register the cached transition_p indices
  branch_transition_p_indices.resize(n_branch_means(), n_smodels());
  for(int s=0;s < n_branch_means(); s++)
  {
    string prefix= "Scale" + convertToString(s+1);
    // Get a list of the branch LENGTH (not time) parameters
    vector<expression_ref> D;
    for(int b=0;b<T->n_branches();b++)
    {
      string name = "d" + convertToString(b+1);
      D.push_back(parameter(prefix+"."+name));
    }

    // FIXME - give this a usable name!!
    // Better yet, make a substitutionBranchLengths!scale!branch that can be referenced elsewhere.
    expression_ref DL = get_list(D);

    // Here, for each (scale,model) pair we're construction a function from branches -> Vector<transition matrix>
    for(int m=0;m < n_smodels(); m++)
    {
      expression_ref S = C.get_expression(SModels[m].main);
      //expression_ref V = listToVectorMatrix;
      expression_ref V = Vector_From_List<Matrix,MatrixObject>();
      //expression_ref I = 0;
      expression_ref I = (get_list_index,branch_cat_list,v1);
      expression_ref E = (mkArray, T->n_branches(), v1^(V,(branch_transition_p, (get_nth_mixture,S,I), (get_list_index, DL, v1) ) ) );
      branch_transition_p_indices(s,m) = C.add_compute_expression(E);
    }
  }

  add_parameter(Parameter("IModels.training", Bool(true)));
  // create data partitions
  for(int i=0;i<A.size();i++) 
  {
    // compute name for data-partition
    string name = string("part") + convertToString(i+1);

    // add the data partition
    data_partitions.push_back( data_partition(name, this, i, A[i]) );
  }

  // Register compute expressions for branch HMMs and sequence length distributions
  for(int i=0;i<IModels.size();i++) 
  {
    imodel_methods& I = IModel_methods[i];
    string prefix = "I" + convertToString(i+1);

    I.length_arg_param_index = add_parameter(Parameter(prefix+".lengthpArg", Int(1)));
    expression_ref lengthp = lambda_expression( RS07_lengthp() );
    expression_ref epsilon = (var("exp"), parameter(I.epsilon));
    expression_ref lengthp_arg = parameter(prefix+".lengthpArg");
    I.length_p = C.add_compute_expression( (lengthp, epsilon, lengthp_arg) );

    // Note that branch_HMM's are per scale and per-imodel.  Construct them in the data_partition.
  }

}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<formula_expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<int>& scale_mapping)
  :Parameters(A, t, SMs, s_mapping, {}, {}, scale_mapping)
{ }

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

