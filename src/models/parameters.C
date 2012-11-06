#undef NDEBUG
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

#include "models/parameters.H"
#include "rng.H"
#include "substitution/substitution.H"
#include "substitution/substitution-index.H"
#include "alignment/alignment-util.H"
#include "likelihood.H"
#include "util.H"
#include "mcmc/proposals.H"
#include "probability/probability.H"
#include "computation/formula_expression.H"
#include "smodel/operations.H"
#include "computation/prelude.H"
#include "computation/program.H"
#include "computation/operations.H"
#include "math/exponential.H"
#include "smodel/functions.H"
#include "probability/distribution-operations.H"

using std::vector;
using std::string;
using std::cerr;
using std::endl;
using std::ostream;

/*
 * Goal: Construct a complete tree-based imodel along the lines of
 *       SingleRate[RS07] or BranchwiseRate[RS07]
 * 
 * 1. Move calculation of alignment prior dependencies to the machine!
 *    - Otherwise changing ANY imodel parameter with invalidate ALL dependencies!
 *    
 * 2. Allow defining things in a formula_expression
 *    (f_e is becoming more like a program!)
 *    - Could we use let-expressions instead?
 *    - We COULD, but this wouldn't allow the names to be published!
 *
 * 3. Eliminate any remaining cached_value< > in calculation of alignment prior.
 *    - Eliminate cached_alignment_prior
 *    - Eliminate cached_sequence_lengths
 *
 * 4. Move unchangeable name mappings out of context and into reg_heap.
 *   - That is, separate name bindings into (a) dependent and (b) non-dependent vars?
 *
 * 5. Move the Program from Context to reg_heap.
 *
 * 6. Allow calculating location of unnamed parameters hidden in structures.
 *    - Problem! Things move around.
 *    - Calculation of locations might work.  But they would change as soon as we change
 *      their value.
 *    - Also, the location of an anonymous parameter depends on the context.
 *    - Still, it would be nice to be able to change only a single FIELD or PART of a
 *      parameter's value.
 *    - We want to be able to refer to things like "the 6th element in the array that parameter
 *      "x" evaluates to.
 *
 *    - Solution?
 *      + Make another evaluation context, in which parameters are objects that evaluate to themselves
 *        instead of to their values.
 *      + In this context, we can e.g. evaluate list elements until a parameter is found.
 *      + What *type* do we give structures containing parameters, though?  Is [1, parameter("x"), 3] allowed?
 *        - Perhaps it is if we always evaluate parameters to their VALUES.  It would be:
 *              [1, (value parameter("x")), 3]
 *          which is OK.
 *
 *        - However, if we sometimes do not, then we have the issue that everything would need to have a consistent type.
 *
 * 7. ... OR arrays!
 *
 * 8. Define a haskell tree class.
 *
 * 9. Define moves on the tree based on the idea that some entries are parameters.
 *
 * 10. Compare the monadic interface with Acar's interface.
 *
 * 11. Remove class Parameter that is solely used in the old way of defining parameters.
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

    // reset the pairwise alignments.
    for(int b=0;b<T().n_branches();b++)
    {
      int n1 = T().directed_branch(b).source();
      int n2 = T().directed_branch(b).target();
      set_pairwise_alignment(b, A2::get_pairwise_alignment(*A,n1,n2));
    }

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

  return *P->C.evaluate_as<indel::PairHMM>( branch_HMM_indices[b] );
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

  return *P->C.evaluate_as<Double>( P->IModel_methods[m].length_p );
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

  cached_alignment_prior.invalidate();
}

void data_partition::recalc_imodel() 
{
  for(int b=0;b<T().n_branches();b++) 
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

/// Set the pairwise alignment value, but don't mark the alignment & sequence lengths as changed.
void data_partition::set_pairwise_alignment_(int b, const pairwise_alignment_t& pi,bool require_match_A) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  int B = T().directed_branch(b).reverse();

  if (P->get_parameter_value(pairwise_alignment_for_branch[b]))
  {
    assert(pi == get_pairwise_alignment(b,false));
    assert(pi.flipped() == get_pairwise_alignment(B,false));
  }
  else
  {
    assert(not P->get_parameter_value(pairwise_alignment_for_branch[B]));
  }

  const_cast<Parameters*>(P)->set_parameter_value(pairwise_alignment_for_branch[b], new pairwise_alignment_t(pi));
  const_cast<Parameters*>(P)->set_parameter_value(pairwise_alignment_for_branch[B], new pairwise_alignment_t(pi.flipped()));

  if (require_match_A)
  {
    int n1 = T().directed_branch(b).source();
    int n2 = T().directed_branch(b).target();
    assert(get_pairwise_alignment(b,false) == A2::get_pairwise_alignment(*A,n1,n2));
    assert(get_pairwise_alignment(B,false) == A2::get_pairwise_alignment(*A,n2,n1));
  }
}

object_ptr<const Object> data_partition::get_pairwise_alignment_(int b) const
{
  return P->get_parameter_value(pairwise_alignment_for_branch[b]);
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b, bool require_match_A) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: what pairwise alignment are you referring to?";

  if (not P->get_parameter_value(pairwise_alignment_for_branch[b]))
    std::abort();
  //  assert(P->get_parameter_value(pairwise_alignment_for_branch[b]));

#ifndef NDEBUG
  int B = T().directed_branch(b).reverse();
  if (require_match_A)
  {
    int n1 = T().directed_branch(b).source();
    int n2 = T().directed_branch(b).target();
    assert(get_pairwise_alignment(b,false) == A2::get_pairwise_alignment(*A,n1,n2));
    assert(get_pairwise_alignment(B,false) == A2::get_pairwise_alignment(*A,n2,n1));
  }
#endif

  return P->get_parameter_value_as<pairwise_alignment_t>(pairwise_alignment_for_branch[b]);
}

void data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi, bool require_match_A)
{
  note_alignment_changed_on_branch(b);
  set_pairwise_alignment_(b,pi,require_match_A);
}

void data_partition::recompute_pairwise_alignment(int b, bool require_match_A)
{
  int n1 = T().directed_branch(b).source();
  int n2 = T().directed_branch(b).target();
  set_pairwise_alignment(b, A2::get_pairwise_alignment(*A,n1,n2), require_match_A);
}

void data_partition::note_sequence_length_changed(int n)
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the sequence length change?";

  cached_sequence_lengths[n].invalidate();
}

void data_partition::invalidate_pairwise_alignment_for_branch(int b) const
{
  const_cast<Parameters*>(P)->set_parameter_value(pairwise_alignment_for_branch[b], object_ref());
}

void data_partition::note_alignment_changed_on_branch(int b)
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  b = T().directed_branch(b).undirected_name();

  cached_alignment_prior.invalidate();

  int B = T().directed_branch(b).reverse();
  invalidate_pairwise_alignment_for_branch(b);
  invalidate_pairwise_alignment_for_branch(B);

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

  for(int i=0;i<T().n_branches()*2;i++)
    assert(P->get_parameter_value(pairwise_alignment_for_branch[i]));

  if (not cached_alignment_prior.is_valid()) 
  {
    const alignment& AA = *A;
    const SequenceTree& TT = T();

    efloat_t Pr = 1;
    for(int b=0;b<TT.n_branches();b++)
    {
      efloat_t prior_for_branch_b = *P->C.evaluate_as<Log_Double>(alignment_prior_for_branch[b]);
      Pr *= prior_for_branch_b;

#ifndef NDEBUG      
      int target = TT.branch(b).target();
      int source  = TT.branch(b).source();
      //efloat_t p1 = cached_alignment_prior_for_branch[b];
      //efloat_t p2 = prior_branch(AA, get_branch_HMM(b), target, source);
      //double error = log(p1) - log(p2);
      assert(not different(prior_for_branch_b, prior_branch(AA, get_branch_HMM(b), target, source)));
#endif
    }

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

data_partition::data_partition(Parameters* p, int i, const alignment& a)
  :P(p),
   partition_index(i),
   pairwise_alignment_for_branch(2*T().n_branches()),
   alignment_prior_for_branch(T().n_branches()),
   cached_sequence_lengths(a.n_sequences()),
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

  string prefix = "P"+convertToString(i+1)+".";
  for(int b=0;b<pairwise_alignment_for_branch.size();b++)
    pairwise_alignment_for_branch[b] = p->add_parameter(Parameter(prefix+"a"+convertToString(b)));

  if (variable_alignment())
    for(int b=0;b<T().n_branches();b++)
    {
      int n1 = T().directed_branch(b).source();
      int n2 = T().directed_branch(b).target();
      set_pairwise_alignment(b, A2::get_pairwise_alignment(*A,n1,n2));
    }

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
  int scale_index = P->scale_for_partition[partition_index];

  if (i_index != -1)
    for(int b=0;b<B;b++)
    {
      // D = Parameters.substitutionBranchLengths!scale_index
      expression_ref D = (var("!"),var("Parameters.substitutionBranchLengths"),scale_index);
      // (fst IModels.models!i_index) D b
      int index = p->C.add_compute_expression( ((fst,(var("!"),var("IModels.models"),i_index)),D,b) );
      branch_HMM_indices.push_back(  index );
      expression_ref hmm = P->C.get_expression(index);

      expression_ref getTransitionCounts = lambda_expression( get_transition_counts() );
      expression_ref getPairwiseAlignmentProbabilityFromCounts = lambda_expression( pairwise_alignment_probability_from_counts() );
      expression_ref a = parameter( P->parameter_name(pairwise_alignment_for_branch[b]) );
      alignment_prior_for_branch[b] = p->C.add_compute_expression( (getPairwiseAlignmentProbabilityFromCounts,(getTransitionCounts,a),hmm) );
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
  for(int i=0;i<n_imodels();i++)
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

void Parameters::recompute_pairwise_alignment(int b, bool check_A)
{
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      get_data_partition(i).recompute_pairwise_alignment(b, check_A);
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
  // Check for beta (0) or mu[i] (i+1)
  for(int index: indices)
  {
    if (index == 0) // beta
    {
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
      for(int p=0;p<n_data_partitions() and p<scale_for_partition.size();p++)
      {
	if (scale_for_partition[p] == s)
	  get_data_partition(p).branch_mean_changed();
      }
    }

    // If any of the imodel parameters have changed, invalidate all branch HMMs
    for(int i=0;i<n_imodels();i++)
      if (includes(IModel_methods[i].parameters,index))
	recalc_imodel(i);

    if (parameter_name(index) == "IModels.training")
      recalc_imodels();
  }

  // Check if any substitution models have changed.
  // WHY does this work?  Shouldn't this be a structure that never gets out of date?
  for(int s=0;s<n_smodels();s++)
    if (not C.compute_expression_is_up_to_date(SModels[s].main))
      recalc_smodel(s);
}

object_ptr<const alphabet> Parameters::get_alphabet_for_smodel(int s) const
{
  return convert<const alphabet>(C.evaluate(SModels[s].get_alphabet));
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

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
  int s = scale_for_partition[p];
  return get_parameter_value_as<Double>(branch_mean_index(s));
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const vector<formula_expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<formula_expression_ref>& IMs,
		       const vector<int>& i_mapping,
		       const vector<int>& scale_mapping)
  :smodel_for_partition(s_mapping),
   IModel_methods(IMs.size()),
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
  // FIXME: add C += IModel_Functions() instead of referencing the operations directly.  Then we could parse a text file.
  
  // Don't call set_parameter_value here, because recalc( ) depends on branch_length_indices, which is not ready.

  constants.push_back(-1);

  add_parameter(Parameter("Heat.beta", Double(1.0), between(0,1)));

  for(int i=0;i<n_scales;i++)
    add_parameter(Parameter("mu"+convertToString(i+1), Double(0.25), lower_bound(0)));

  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // register the substitution models as sub-models
  for(int i=0;i<SMs.size();i++) 
  {
    string prefix = "S" + convertToString(i+1);

    formula_expression_ref smodel = prefix_formula(prefix, SMs[i]);

    add_submodel(smodel);

    SModels.push_back( smodel_methods( smodel.exp(), C) );
  }

  add_parameter(Parameter("IModels.training", Bool(true)));
  // register the indel models as sub-models
  vector<formula_expression_ref> imodels_;
  for(int i=0;i<n_imodels();i++) 
  {
    string prefix = "I" + convertToString(i+1);

    formula_expression_ref imodel = prefix_formula(prefix, IMs[i]);

    imodels_.push_back(imodel);

    IModel_methods[i].parameters = add_submodel(imodel);
  }
  Program imodels_program("IModels");
  imodels_program.def_function("models", 0, (listArray_, get_list(imodels_).exp()));
  C += imodels_program;
  
  // check that we only map existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SModels.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SModels.size()<<" smodels.";
  }

  /*------------------------- Add commands to log all parameters created before this point. ------------------------*/
  expression_ref make_logger = constructor("make_logger",1);
  for(int i=0;i<n_parameters();i++)
    add_note( (make_logger,parameter_name(i)) );

  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC.modify()->branch(b).set_length(-1);

  // Add and initialize variables for branch *lengths*: scale<s>.D<b>
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
  
  expression_ref substitutionBranchLengthsList;
  {
    vector<expression_ref> SBLL;
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
      SBLL.push_back(get_list(D));
    }
    substitutionBranchLengthsList = get_list(SBLL);
  }

  Program parameter_program("Parameters");
  parameter_program.def_function("substitutionBranchLengths", 0, (listArray_,(fmap,listArray_,substitutionBranchLengthsList)));
  C += parameter_program;

  // register the cached transition_p indices
  branch_transition_p_indices.resize(n_branch_means(), n_smodels());
  for(int s=0;s < n_branch_means(); s++)
  {
    // Better yet, make a substitutionBranchLengths!scale!branch that can be referenced elsewhere.
    expression_ref DL = (var("!"),var("Parameters.substitutionBranchLengths"),s);

    // Here, for each (scale,model) pair we're construction a function from branches -> Vector<transition matrix>
    for(int m=0;m < n_smodels(); m++)
    {
      expression_ref S = C.get_expression(SModels[m].main);
      //expression_ref V = listToVectorMatrix;
      expression_ref V = Vector_From_List<Matrix,MatrixObject>();
      //expression_ref I = 0;
      expression_ref I = (get_list_index,branch_cat_list,v1);
      expression_ref E = (mkArray, T->n_branches(), v1^(V,(branch_transition_p, (get_nth_mixture,S,I), (var("!"), DL, v1) ) ) );
      branch_transition_p_indices(s,m) = C.add_compute_expression(E);
    }
  }

  // create data partitions
  for(int i=0;i<A.size();i++) 
    data_partitions.push_back( data_partition(this, i, A[i]) );

  // Register compute expressions for branch HMMs and sequence length distributions
  for(int i=0;i<n_imodels();i++) 
  {
    imodel_methods& I = IModel_methods[i];
    string prefix = "I" + convertToString(i+1);

    I.length_arg_param_index = add_parameter(Parameter(prefix+".lengthpArg", Int(1)));
    expression_ref lengthp = (snd,(var("!"),var("IModels.models"),i));
    expression_ref lengthp_arg = parameter(prefix+".lengthpArg");
    I.length_p = C.add_compute_expression( (lengthp, lengthp_arg) );

    // Note that branch_HMM's are per scale and per-imodel.  Construct them in the data_partition.
  }

  /*------------------------- Create the tree structure -----------------------*/
  Model_Notes tree;

  vector<expression_ref> node_branches;
  for(int n=0; n < T->n_nodes(); n++)
  {
    expression_ref param = def_parameter(tree, "Tree.nodeBranches"+convertToString(n));
    node_branches.push_back( (var("listFromVectorInt"),param) );
  }
  expression_ref node_branches_array = (listArray_,get_list(node_branches));

  vector<expression_ref> branch_nodes;
  for(int b=0; b < 2*T->n_branches(); b++)
  {
    expression_ref param = def_parameter(tree, "Tree.branchNodes"+convertToString(b));
    branch_nodes.push_back( (var("listFromVectorInt"), param) );
  }
  expression_ref branch_nodes_array = (listArray_,get_list(branch_nodes));

  expression_ref tree_con = lambda_expression( constructor("Tree",4) );

  // FIXME - there should be some way to define this all and then add the prefix "Tree"!

  add_submodel( tree);

  expression_ref _ = dummy(-1);

  Program tree_program("Tree");
  tree_program.def_function("tree", 0, (tree_con, node_branches_array, branch_nodes_array, T->n_nodes(), T->n_branches()));

  // numNodes (Tree _ _ n _) = n
  tree_program += Def( (var("numNodes"), (tree_con, _, _, v1, _)), v1);

  // numBranches (Tree _ _ _ n) = n
  tree_program += Def( (var("numBranches"), (tree_con, _, _, _, v1)), v1);

  // edgesOutOfNode (Tree nodesArray _ _ _) node = nodesArray!node
  tree_program += Def( (var("edgesOutOfNode"), (tree_con,v1,_,_,_), v2), (var("!"),v1,v2));

  // nodesForEdge (Tree _ branchesArray _ _) edgeIndex = branchesArray!edgeIndex
  tree_program += Def( (var("nodesForEdge"), (tree_con,_,v1,_,_), v2), (var("!"),v1,v2));

  // sourceNode t edge = fst (nodesForEdge t edge)
  tree_program += Def( (var("sourceNode"), v1, v2), (fst, (var("nodesForEdge"), v1, v2)));

  // targetNode t edge = snd (nodesForEdge t edge)
  tree_program += Def( (var("targetNode"), v1, v2), (snd, (var("nodesForEdge"), v1, v2)));

  // findFirst f h:t = if (f h) then h else findFirst f t
  tree_program += Def( (var("findFirst"),v1,v2&v3), (If,(v1,v2),v2,(var("findFirst"),v1,v3)) );

  // edgeForNodes t (n1, n2) = [b | b <- (edgesOutOfNode t s), target t b == n2]
  tree_program += Def( (var("edgeForNodes"),v3,Tuple(v1,v2)), (var("findFirst"),(var("edgesOutOfNode"),v3,v1),v4^((var("targetNode"),v3,v4)==v2)));

  // reverseEdge t b = edgeForNodes t (swap (nodesForEdge t b))
  tree_program += Def( (var("reverseEdge"),v1,v2), (var("edgeForNodes"),v1, (var("swap"),(var("nodesForEdge"),v1, v2) ) ) );

  // nodeDegree t n = length (edgesOutOfNode t n)
  tree_program += Def( (var("nodeDegree"),v1,v2), (var("length"),(var("edgesOutOfNode"), v1, v2) ) );

  // neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)
  tree_program += Def( (var("neighbors"),v1,v2), (var("fmap"),(var("targetNode"), v1), (var("edgesOutOfNode"),v1, v2) ) );

  // edgesBeforeEdge t b = let (n1,n2) = nodesForEdge t b in 
  //                            [edgeForNodes (n,n1) | n <- neighbors t n1, n /= n2 ]


  C += tree_program;

  for(int n=0; n < T->n_nodes(); n++)
  {
    vector<const_branchview> branch_list;
    append(T->node(n).branches_out(),branch_list);
  }

  for(int b=0; b < 2*T->n_branches(); b++)
  {
    int source = T->directed_branch(b).source();
    int target = T->directed_branch(b).source();
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

