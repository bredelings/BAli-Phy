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
#include "computation/module.H"
#include "computation/operations.H" // for VectorFromList<>
#include "math/exponential.H"

using std::vector;
using std::string;
using std::cerr;
using std::endl;
using std::ostream;

/*
 * 1. [DONE] Allow calculating the location of unnamed parameters hidden in structures & arrays.
 *
 * 2. [DONE] Remove class Parameter that is solely used in the old way of defining parameters.
 *
 *
 * \todo Goal: Construct a complete tree-based imodel along the lines of
 *
 *       SingleRate[RS07] or BranchwiseRate[RS07]
 * 
 * 1. Move calculation of alignment prior dependencies to the machine!
 *    - Otherwise changing ANY imodel parameter with invalidate ALL dependencies!
 *
 * 2. Improve quality of code fragments here by switching from integer names (e.g. "a12") to subscripts (e.g. "a!12").
 *    
 * 3. [Done] Eliminate any remaining cached_value< > in calculation of alignment prior or sequence lengths.
 *
 * 4. Move unchangeable name mappings out of context and into reg_heap.
 *   - That is, separate name bindings into (a) dependent and (b) non-dependent vars?
 *
 * 5. Translate code from calls to parser?
 *
 * 6. Now that we can stop triggering on imodel parameters, can we somehow attach objects to Changeable nodes
 *    as triggers?  Perhaps each changeable node needs a list of triggers that "fire" when it is invalidated?
 *
 * 7. Allow addressing anonymous parameters by setting structures containing parameters to things.  For example,
 *    setting [x,y] to [1,2] would set x to 1 and y to 2.
 *    - But sometimes we do want to change z=[x,y] to (say) [].
 *    - And how about setting [x=[a,b],y] to [[1,2],[3,4]]. Do we change x here, or not?
 *    - Will we only try to set parameter values for leaves?
 *    - Perhaps we need to be more explicit about the procedure for setting a list of parameters to a list of values.
 *      + Perhaps we could say to walk the list and find the parameters (which works, because it evaluates each list entry
 *        until it becomes a parameter!) and to then set each entry to the corresonding list entry.
 *
 * 8. Define a haskell tree class.
 *
 * 9. Define moves on the tree based on the idea that some entries are parameters.
 *
 * 10. Compare the monadic interface with Acar's interface.
 *
 * 12. Make loading of data easier
 *     - Handle sequences with lengths not divisible by 3.
 *     - Handle loading alignments with codons not together.
 *     - Handle guessing of alphabets based on frequencies.
 *     - Handle loading of letters like K for DNA -- change to N.
 *     - Could we actually handle all SEEN codon triplets?
 *
 * 13. 
 */

bool use_internal_index = true;

const SequenceTree& data_partition::T() const
{
  return P->T();
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
    subA = new subA_index_leaf(A->length()+1, T().n_branches()*2);

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
      subA = new subA_index_internal(A->length()+1, T().n_branches()*2);
    else
      subA = new subA_index_leaf(A->length()+1, T().n_branches()*2);

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

  return *P->evaluate_as<Box<vector<Matrix>>>( transition_p_method_indices[b] );
}

int data_partition::n_base_models() const
{
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->evaluate(P->SModels[s].n_base_models);
  return *convert<const Int>(O);
}

int data_partition::n_states() const
{
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->evaluate(P->SModels[s].n_states);
  return *convert<const Int>(O);
}

vector<double> data_partition::distribution() const
{
  // Add Op to convert list to vector<Double>
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->evaluate(P->SModels[s].distribution);
  return *convert<const Box<vector<double>>>(O);
}

vector<unsigned> data_partition::state_letters() const
{
  int s = P->smodel_for_partition[partition_index];
  object_ref O = P->evaluate(P->SModels[s].state_letters);
  return *convert<const Box<vector<unsigned> > >(O);
}

vector<double> data_partition::frequencies(int m) const
{
  return *P->evaluate_as<Vector<double>>( frequencies_indices[m] );
}

object_ptr<const Object> data_partition::base_model(int m, int b) const
{
  b = T().directed_branch(b).undirected_name();

  return P->evaluate( base_model_indices(m,b) );
}

const indel::PairHMM& data_partition::get_branch_HMM(int b) const
{
  assert(variable_alignment());

  b = T().directed_branch(b).undirected_name();

  return *P->evaluate_as<indel::PairHMM>( branch_HMM_indices[b] );
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

  return *P->evaluate_as<Double>( P->IModel_methods[m].length_p );
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

void data_partition::setlength(int b)
{
  LC.invalidate_branch(T(),b);
}

int data_partition::seqlength(int n) const
{
  int l = *P->evaluate_as<Int>(sequence_length_indices[n]);

  assert(l == A->seqlength(n));

  return l;
}

void data_partition::uniquify_subA_index()
{
  if (subA->ref_count() > 1)
    subA = subA->clone();

  assert(subA->ref_count() == 1);
}

void data_partition::invalidate_subA_index_branch(int b)
{
  uniquify_subA_index();

  // propagates outward in both directions
  subA->invalidate_branch(T(),b);
}

void data_partition::invalidate_subA_index_one_branch(int b)
{
  uniquify_subA_index();

  int b2 = T().directed_branch(b).reverse();
  subA->invalidate_one_branch(b);
  subA->invalidate_one_branch(b2);
}

void data_partition::invalidate_subA_index_all()
{
  uniquify_subA_index();

  subA->invalidate_all_branches();
}

void data_partition::subA_index_allow_invalid_branches(bool b)
{
  uniquify_subA_index();

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

void data_partition::invalidate_pairwise_alignment_for_branch(int b) const
{
  const_cast<Parameters*>(P)->set_parameter_value(pairwise_alignment_for_branch[b], object_ref());
}

void data_partition::note_alignment_changed_on_branch(int b)
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  b = T().directed_branch(b).undirected_name();

  int B = T().directed_branch(b).reverse();
  invalidate_pairwise_alignment_for_branch(b);
  invalidate_pairwise_alignment_for_branch(B);

  const Tree& TT = T();
  int target = TT.branch(b).target();
  int source = TT.branch(b).source();

  // If the alignment changes AT ALL, then the mapping from subA columns to alignment columns is broken.
  // Therefore we always mark it as out-of-date and needing to be recomputed.
  invalidate_subA_index_all();

  // However, LC depends only on the alignment of subA indices from different branches.
  // 
  // If the projected leaf alignment remains unchanged, then the subA columns
  // projected to the leaves remain unchanged.  If we only index these columns, then the
  // get_subA_index( ) will not change if we are using subA_index_leaf.
  //
  if (dynamic_pointer_cast<subA_index_internal>(subA))
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

  efloat_t Pr = *P->evaluate_as<Log_Double>(alignment_prior_index);

  assert(not different(Pr, ::prior_HMM(*this)));

  return Pr;
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
   sequence_length_indices(a.n_sequences(),-1),
   transition_p_method_indices(T().n_branches(),-1),
   variable_alignment_( has_IModel() ),
   sequences( alignment_letters(a,T().n_leaves()) ),
   A(a),
   LC(T(), *this),
   branch_HMM_type(T().n_branches(),0)
{
  int B = T().n_branches();

  if (variable_alignment() and use_internal_index)
    subA = new subA_index_internal(a.length()+1, B*2);
  else
    subA = new subA_index_leaf(a.length()+1, B*2);

  string prefix = "P"+convertToString(i+1)+".";
  for(int b=0;b<pairwise_alignment_for_branch.size();b++)
    pairwise_alignment_for_branch[b] = p->add_parameter(prefix+"a"+convertToString(b));

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

    expression_ref E = P->get_expression(P->branch_transition_p_indices(s,m));
    E = (identifier("!"), E, b);

    transition_p_method_indices[b] = p->add_compute_expression(E);
  }

  // Add method indices for calculating base models and frequencies
  base_model_indices.resize(n_models, B);
  for(int m=0;m<n_models;m++)
  {
    int s = P->smodel_for_partition[partition_index];
    expression_ref F = P->get_expression(P->SModels[s].frequencies);
    frequencies_indices.push_back( p->add_compute_expression( (F,m) ) );

    expression_ref BM = P->get_expression(P->SModels[s].base_model);
    for(int b=0;b<B;b++)
      base_model_indices(m,b) = p->add_compute_expression((BM,m,b));
  }

  // Add method indices for calculating branch HMMs
  int i_index = P->imodel_for_partition[partition_index];
  int scale_index = P->scale_for_partition[partition_index];

  if (i_index != -1)
  {
    // D = Params.substitutionBranchLengths!scale_index
    expression_ref D = (identifier("!"),identifier("Params.substitutionBranchLengths"),scale_index);
    expression_ref heat = parameter("Heat.beta");
    expression_ref training = parameter("IModels.training");
    expression_ref model = (identifier("!"),identifier("IModels.models"),i_index);

    vector<expression_ref> as_;
    for(int b=0;b<2*B;b++)
    {
      expression_ref a = parameter( P->parameter_name(pairwise_alignment_for_branch[b]) );
      as_.push_back(a);
    }
    expression_ref as = P->get_expression( p->add_compute_expression((identifier("listArray'"),get_list(as_))) );

    expression_ref hmms = (identifier("branch_hmms"), model, D, heat, training, B);
    hmms = P->get_expression( p->add_compute_expression(hmms) );

    for(int b=0;b<B;b++)
    {
      // (fst IModels.models!i_index) D b heat training
      int index = p->add_compute_expression( (identifier("!"), hmms, b) );
      branch_HMM_indices.push_back( index );
      expression_ref hmm = P->get_expression(index);

      alignment_prior_for_branch[b] = p->add_compute_expression( (identifier("alignment_branch_pr"),as,hmms,b) );
    }

    expression_ref tree = identifier("MyTree.tree");

    alignment_prior_index = p->add_compute_expression( (identifier("alignment_pr"), as, tree, hmms, model) );

    for(int n=0;n<T().n_nodes();n++)
    {
      expression_ref L = A->seqlength(n);
      if (variable_alignment())
	L = (identifier("seqlength"),as,tree,n);
      sequence_length_indices[n] = p->add_compute_expression( L );
    }
  }
}

//-----------------------------------------------------------------------------//
smodel_methods::smodel_methods(const expression_ref& E, context& C)
{
  expression_ref V = identifier("listToVectorDouble");

  main = C.add_compute_expression( E );
  expression_ref S = C.get_expression(main);

  n_base_models = C.add_compute_expression((identifier("nBaseModels"), S));
  n_states =  C.add_compute_expression((identifier("nStates"), S));
  distribution =  C.add_compute_expression((V,(identifier("distribution"), S)));
  get_alphabet = C.add_compute_expression((identifier("getAlphabet"), S));
  state_letters = C.add_compute_expression((identifier("stateLetters"), S));
  n_states = C.add_compute_expression((identifier("nStates"), S));
  rate = C.add_compute_expression((identifier("rate"), S));

  base_model = C.add_compute_expression( v1^(v2^(identifier("baseModel"), (identifier("getNthMixture"),S,v2), v1) ) );
  frequencies = C.add_compute_expression((identifier("componentFrequencies"), S));
  transition_p = C.add_compute_expression((identifier("branchTransitionP"), S));
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

const SequenceTree& Parameters::T() const
{
  return *T_;
}

OVector edges_connecting_to_node(const Tree& T, int n)
{
  vector<const_branchview> branch_list;
  append(T.node(n).branches_out(),branch_list);
  
  OVector branch_list_;
  for(auto b: branch_list)
    branch_list_.push_back(Int(int(b)));

  return branch_list_;
}

void Parameters::read_h_tree()
{
  for(int n=0; n < T().n_nodes(); n++)
    context::set_parameter_value(parameter_for_tree_node[n], edges_connecting_to_node(T(),n));

  for(int b=0; b < 2*T().n_branches(); b++)
    context::set_parameter_value(parameter_for_tree_branch[b], OPair(Opair{Int(T().directed_branch(b).source()), Int(T().directed_branch(b).target())}) );
}
void Parameters::set_tree(const SequenceTree& T2)
{
  check_h_tree();

  *T_.modify() = T2;

  read_h_tree();

  check_h_tree();
}

void Parameters::reconnect_branch(int s1, int t1, int t2)
{
  uniquify_subA_indices();

  check_h_tree();

  int b1 = T().directed_branch(s1,t1);
  int b2 = T().directed_branch(t1,s1);

  T_.modify()->reconnect_branch(s1,t1,t2);

  context::set_parameter_value(parameter_for_tree_branch[b1], OPair(Opair{Int(T().directed_branch(b1).source()), Int(T().directed_branch(b1).target())}) );
  context::set_parameter_value(parameter_for_tree_branch[b2], OPair(Opair{Int(T().directed_branch(b2).source()), Int(T().directed_branch(b2).target())}) );
  context::set_parameter_value(parameter_for_tree_node[t1], edges_connecting_to_node(T(),t1));
  context::set_parameter_value(parameter_for_tree_node[t2], edges_connecting_to_node(T(),t2));

  check_h_tree();
}

// This could create loops it we don't check that the subtrees are disjoint.
// br{1,2} point into the subtrees.  b{1,2} point out of the subtrees, towards the other subtree.
void Parameters::exchange_subtrees(int br1, int br2)
{
  const_branchview b1 = T().directed_branch(br1).reverse();
  const_branchview b2 = T().directed_branch(br2).reverse();

  int s1 = b1.source();
  int t1 = b1.target();

  int s2 = b2.source();
  int t2 = b2.target();

  assert(not T().subtree_contains(br1,s2));
  assert(not T().subtree_contains(br2,s1));

  reconnect_branch(s1,t1,t2);
  reconnect_branch(s2,t2,t1);
}

/// SPR: move the subtree b1 into branch b2
///
/// When two branches are merged into one as the pruned subtree is removed,
/// one branch name remains in place, and one is moved.  If branch_to_move is
/// -1, then the branch with the higher undirected name is the one that moves.
/// If branch_to_move is not -1 (default) then it specifies the one to move.
///
/// The direction of the branches that do not move remains unchanged, but
/// for the attachment branch, it may be pointing either towards or away
/// from the attachment point.
///
/// Got m1<--->x<--->m2 and n1<--->n2, and trying to move x onto (n1,n2)
int Parameters::SPR(int br1, int br2, int branch_to_move)
{
  check_h_tree();

  int x1 = T().directed_branch(br1).source();
  int x2 = T().directed_branch(br1).target();

  std::vector<const_branchview> m_branches;
  append(T().directed_branch(x2,x1).branches_after(), m_branches);
  assert(m_branches.size() == 2);
  int m1 = m_branches[0].target();
  int m2 = m_branches[1].target();

  int n1 = T().directed_branch(br2).source();
  int n2 = T().directed_branch(br2).target();

  //-------------------- Correctly order m1 and m2 ----------------------//
  // Preserve the name of the branch with the smaller name (to avoid renaming leaf branches!)
  // (The name of the x<--->n1 branch gets preserved)
  if (branch_to_move == -1)
  {
    if (T().directed_branch(m1,x1).undirected_name() > T().directed_branch(m2,x1).undirected_name() )
      std::swap(m1,m2);
  }
  // ensure that (x,m2) is the branch to move
  else
  {
    if (T().directed_branch(m1,x1).name() == branch_to_move or T().directed_branch(x1,m1).name() == branch_to_move)
      std::swap(m1,m2);
    else if (T().directed_branch(m2,x1).name() == branch_to_move or T().directed_branch(x1,m2).name() == branch_to_move)
      ;
    else
      std::abort(); // we couldn't find the branch to move!
  }

  //-------------------- Correctly order n1 and n2 ----------------------//
  // choose sub-branch to give the new name to. (It will go to the one pointed to by b2)
  if (n1 > n2)
    std::swap(n1,n2);

  //------ Merge the branches (m1,x1) and (x1,m2) -------//
  int dead_branch = T().directed_branch(m2,x1).undirected_name();

  setlength_unsafe( T().directed_branch(m1,x1), T().directed_branch(m1,x1).length() + T().directed_branch(m2,x1).length() );
  setlength_unsafe( T().directed_branch(m2,x1), 0.0);

  //------------ Reconnect the branches ---------------//

  // Reconnect (m1,x) to m2, making x a degree-2 node
  // This leaves m1 connected to its branch, so m1 can be a leaf.
  assert(not T().node(m2).is_leaf_node());
  reconnect_branch(m1, x1, m2);

  // Reconnect (x,m2) to n2, leaving x a degree-2 node
  assert(not T().node(m2).is_leaf_node());
  reconnect_branch(x1, m2, n2);

  // Reconnect (n1,n2) to x, making x a degree-3 node again.
  // This leaves n1 connected to its branch, so n1 can be a leaf.
  assert(not T().node(n2).is_leaf_node());
  reconnect_branch(n1, n2, x1);

  return dead_branch;
}

void Parameters::check_h_tree() const
{
#ifndef NDEBUG
  for(int b=0; b < 2*T().n_branches(); b++)
  {
    object_ref p = get_parameter_value(parameter_for_tree_branch[b]);
    object_ref s = convert<const OPair>(p)->first;
    object_ref t = convert<const OPair>(p)->second;
    assert(T().directed_branch(b).source() == *convert<const Int>(s));
    assert(T().directed_branch(b).target() == *convert<const Int>(t));
  }

  for(int n=0; n < n*T().n_nodes(); n++)
  {
    object_ptr<const OVector> V = convert<const OVector>(get_parameter_value(parameter_for_tree_node[n]));
    vector<int> VV;
    for(const auto& elem: *V)
      VV.push_back(*convert<const Int>(elem));

    vector<const_branchview> v = branches_from_node(T(), n);
    vector<int> vv;
    for(const auto& bv: v)
      vv.push_back(bv);

    assert(V->size() == v.size());
    for(int elem: v)
      assert(includes(VV,elem));
    for(int elem: VV)
      assert(includes(vv,elem));
  }
#endif
}

efloat_t Parameters::prior_no_alignment() const 
{
  efloat_t Pr = Model::prior();

  // prior on the topology and branch lengths
  Pr *= ::prior(*this, T(), 1.0);

  if (branch_length_max > 0)
    for(int i=0; i<T().n_branches(); i++)
    {
      if (T().branch(i).length() > branch_length_max)
	return 0;
    }

  // prior for each branch being aligned/unaliged
  if (variable_alignment()) 
  {
    const double p_unaligned = load_value("P_aligned",0.0);

    efloat_t pNA = p_unaligned;

    efloat_t pA = (1.0 - p_unaligned);

    for(int b=0;b<T().n_branches();b++)
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
    ::select_root(T(), b, get_data_partition(i).LC);
}

void Parameters::set_root(int node)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.root = node;
}

void Parameters::LC_invalidate_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.invalidate_branch(T(),b);
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

void Parameters::uniquify_subA_indices()
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).uniquify_subA_index();
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

void Parameters::recalc()
{
  // Check for beta (0) or mu[i] (i+1)
  for(int index: triggers())
  {
    if (0 <= index and index < n_scales)
    {
      int s = index;

      assert(includes(triggers(),s));
      assert(0 <= s and s < n_scales);
      
      // Change branch lengths for the s-th scale
      assert(branch_length_indices[s].size() == T().n_branches());
      for(int b=0;b<T().n_branches();b++)
      {
	double rate = *convert<const Double>(get_parameter_value(branch_mean_index(s)));
	double delta_t = T().branch(b).length();

	context::set_parameter_value(branch_length_indices[s][b], Double(rate*delta_t));
      }

      // notify partitions with scale 'p' that their branch mean changed
      for(int p=0;p<n_data_partitions() and p<scale_for_partition.size();p++)
      {
	if (scale_for_partition[p] == s)
	  get_data_partition(p).branch_mean_changed();
      }
    }
  }
  triggers().clear();

  // Check if any substitution models have changed.
  // This (probably?) works because it recursively check the up-to-date-ness of the entire structure.
  for(int s=0;s<n_smodels();s++)
    if (not compute_expression_is_up_to_date(SModels[s].main))
      recalc_smodel(s);
}

object_ptr<const alphabet> Parameters::get_alphabet_for_smodel(int s) const
{
  return convert<const alphabet>(evaluate(SModels[s].get_alphabet));
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
  b = T().directed_branch(b).undirected_name();
  T_.modify()->directed_branch(b).set_length(l);

  // Update D parameters
  for(int s=0; s<n_scales; s++) 
  {
    double rate = *convert<const Double>(get_parameter_value(branch_mean_index(s)));
    double delta_t = T().branch(b).length();
    
    context::set_parameter_value(branch_length_indices[s][b], rate * delta_t);
  }
}

void Parameters::setlength_unsafe(int b,double l) 
{
  b = T().directed_branch(b).undirected_name();
  T_.modify()->directed_branch(b).set_length(l);
}

void Parameters::setlength(int b,double l) 
{
  b = T().directed_branch(b).undirected_name();
  T_.modify()->directed_branch(b).set_length(l);

  // Update D parameters
  for(int s=0; s<n_scales; s++) 
  {
    double rate = *convert<const Double>(get_parameter_value(branch_mean_index(s)));
    double delta_t = T().branch(b).length();
    
    context::set_parameter_value(branch_length_indices[s][b], rate * delta_t);
  }

  // Invalidates conditional likelihoods
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
  context::set_parameter_value(branch_mean_index(i), Double(x) );
}

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
  int s = scale_for_partition[p];
  return get_parameter_value_as<Double>(branch_mean_index(s));
}

Parameters::Parameters(const module_loader& L,
		       const vector<alignment>& A, const SequenceTree& t,
		       const vector<formula_expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<formula_expression_ref>& IMs,
		       const vector<int>& i_mapping,
		       const vector<int>& scale_mapping)
  :Probability_Model(L),
   smodel_for_partition(s_mapping),
   IModel_methods(IMs.size()),
   imodel_for_partition(i_mapping),
   scale_for_partition(scale_mapping),
   n_scales(max(scale_mapping)+1),
   T_(t),
   branch_prior_type(0),
   TC(star_tree(t.get_leaf_labels())),
   branch_HMM_type(t.n_branches(),0),
   updown(-1),
   features(0),
   branch_length_max(-1)
{
  // \todo FIXME:cleanup|fragile - Don't touch C here directly!
  *this += { "SModel","Distributions","Range","PopGen","Alignment" };
  
  // Don't call set_parameter_value here, because recalc( ) depends on branch_length_indices, which is not ready.

  constants.push_back(-1);

  add_parameter("Heat.beta", Double(1.0));

  Model_Notes Main;
  for(int i=0;i<n_scales;i++)
  {
    string mu_name = "Main.mu"+convertToString(i+1);
    def_parameter(Main, mu_name, Double(0.25), lower_bound(0), (identifier("gamma"), 0.5, 2.0));
  }
  add_submodel(Main);

  for(int i=0;i<n_scales;i++)
  {
    string mu_name = "Main.mu"+convertToString(i+1);
    int trigger = add_compute_expression( (identifier("trigger_on"),parameter(mu_name),i) );
    set_re_evaluate(trigger, true);
  }

  /*------------------------- Create the tree structure -----------------------*/
  Module tree_module("MyTree");

  vector<expression_ref> node_branches;
  for(int n=0; n < T().n_nodes(); n++)
  {
    string name = "nodeBranches"+convertToString(n);
    tree_module.declare_parameter("nodeBranches"+convertToString(n));
    expression_ref param = parameter("MyTree." + name);
    node_branches.push_back( (identifier("list_from_vector"),param) );
  }
  expression_ref node_branches_array = (identifier("listArray'"),get_list(node_branches));

  vector<expression_ref> branch_nodes;
  for(int b=0; b < 2*T().n_branches(); b++)
  {
    string name = "branchNodes"+convertToString(b); 
    tree_module.declare_parameter(name);
    branch_nodes.push_back( (identifier("pair_from_c"), parameter("MyTree."+name)) );
  }
  expression_ref branch_nodes_array = (identifier("listArray'"),get_list(branch_nodes));

  expression_ref tree_con = lambda_expression( constructor("Tree.Tree",4) );

  tree_module.add_import(false, "Tree");
  tree_module.def_function("tree", (tree_con, node_branches_array, branch_nodes_array, T().n_nodes(), T().n_branches()));

  add_submodel( tree_module );

  // Determine the parameter index for branches adjoining a tree node
  for(int n=0; n < T().n_nodes(); n++)
  {
    string parameter_name = "MyTree.nodeBranches"+convertToString(n);
    parameter_for_tree_node.push_back ( find_parameter(parameter_name) );
    assert( parameter_for_tree_node.back() != -1);
  }
  // Determine the parameter index for nodes at the endpoint of a branch
  for(int b=0; b < 2*T().n_branches(); b++)
  {
    string parameter_name = "MyTree.branchNodes"+convertToString(b);
    parameter_for_tree_branch.push_back( find_parameter(parameter_name) );
    assert( parameter_for_tree_branch.back() != -1);
  }

  read_h_tree();

  check_h_tree();

  evaluate_expression( (identifier("numNodes"), identifier("MyTree.tree")));
  evaluate_expression( (identifier("numBranches"), identifier("MyTree.tree")));
  evaluate_expression( (identifier("edgesOutOfNode"), identifier("MyTree.tree"), 0));
  evaluate_expression( (identifier("neighbors"), identifier("MyTree.tree"), 0));
  evaluate_expression( (identifier("nodesForEdge"),identifier("MyTree.tree"), 0));
  int nn = *convert<const Int>(evaluate_expression( (identifier("edgeForNodes"), identifier("MyTree.tree"), (identifier("nodesForEdge"),identifier("MyTree.tree"), 0))));
  for(int b=0; b < 2*T().n_branches(); b++)
  {
    vector<const_branchview> branch_list;
    append(T().directed_branch(b).branches_before(),branch_list);
    vector<int> branch_list_;
    for(auto b: branch_list)
      branch_list_.push_back(b);

    vector<int> b2 = *convert<const Vector<int>>(evaluate_expression( (identifier("listToVectorInt"),((identifier("edgesBeforeEdge"),identifier("MyTree.tree"),b)))));
    assert(b2.size() == branch_list_.size());
    for( int i: branch_list_)
      assert(includes(b2,i));
  }

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

    SModels.push_back( smodel_methods( smodel.perform_exp(), *this) );
  }

  // register the indel models as sub-models
  vector<expression_ref> imodels_;
  for(int i=0;i<n_imodels();i++) 
  {
    string prefix = "I" + convertToString(i+1);

    formula_expression_ref imodel = prefix_formula(prefix, IMs[i]);

    imodels_.push_back(imodel.perform_exp());

    IModel_methods[i].parameters = add_submodel(imodel);
  }
  Module imodels_program("IModels");
  imodels_program.def_function("models", (identifier("listArray'"), get_list(imodels_)));
  imodels_program.declare_parameter("training");
  add_submodel(imodels_program);
  context::set_parameter_value(find_parameter("IModels.training"), false);
  
  // check that we only map existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SModels.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SModels.size()<<" smodels.";
  }

  /*------------------------- Add commands to log all parameters created before this point. ------------------------*/
  // don't constrain any branch lengths
  for(int b=0;b<TC->n_branches();b++)
    TC.modify()->branch(b).set_length(-1);

  // Add and initialize variables for branch *lengths*: scale<s>.D<b>
  for(int s=0;s<n_scales;s++)
  {
    string prefix= "Scale" + convertToString(s+1);
    branch_length_indices.push_back(vector<int>());
    for(int b=0;b<T().n_branches();b++)
    {
      double rate = *convert<const Double>(get_parameter_value(branch_mean_index(s)));
      double delta_t = T().branch(b).length();

      string name = "d" + convertToString(b+1);
      int index = add_parameter(prefix+"."+name, Double(rate * delta_t));
      branch_length_indices[s].push_back(index);
    }
  }

  // Add and initialize variables for branch *categories*: branch_cat<b>
  vector<expression_ref> branch_categories;
  for(int b=0;b<T().n_branches();b++)
  {
    string name = "Main.branchCat" + convertToString(b+1);
    add_parameter(name, Int(0));
    branch_categories.push_back(parameter(name));
  }
  expression_ref branch_cat_list = get_expression( add_compute_expression( (get_list(branch_categories) ) ) );

  expression_ref substitutionBranchLengthsList;
  {
    vector<expression_ref> SBLL;
    for(int s=0;s < n_branch_means(); s++)
    {
      string prefix= "Scale" + convertToString(s+1);
      // Get a list of the branch LENGTH (not time) parameters
      vector<expression_ref> D;
      for(int b=0;b<T().n_branches();b++)
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

  Module parameter_program("Params");
  parameter_program.def_function("substitutionBranchLengths", (identifier("listArray'"),(identifier("fmap"),identifier("listArray'"),substitutionBranchLengthsList)));
  add_submodel( parameter_program );

  // register the cached transition_p indices
  branch_transition_p_indices.resize(n_branch_means(), n_smodels());
  for(int s=0;s < n_branch_means(); s++)
  {
    // Better yet, make a substitutionBranchLengths!scale!branch that can be referenced elsewhere.
    expression_ref DL = (identifier("!"),identifier("Params.substitutionBranchLengths"),s);

    // Here, for each (scale,model) pair we're construction a function from branches -> Vector<transition matrix>
    for(int m=0;m < n_smodels(); m++)
    {
      expression_ref S = get_expression(SModels[m].main);
      //expression_ref V = identifier("listToVectorMatrix");
      expression_ref V = Vector_From_List<Matrix,MatrixObject>();
      //expression_ref I = 0;
      expression_ref I = (identifier("!!"),branch_cat_list,v1);
      expression_ref E = (identifier("mkArray"), T().n_branches(), v1^(V,(identifier("branchTransitionP"), (identifier("getNthMixture"),S,I), (identifier("!"), DL, v1) ) ) );
      branch_transition_p_indices(s,m) = add_compute_expression(E);
    }
  }

  // Register compute expressions for branch HMMs and sequence length distributions
  for(int i=0;i<n_imodels();i++) 
  {
    imodel_methods& I = IModel_methods[i];
    string prefix = "I" + convertToString(i+1);

    I.length_arg_param_index = add_parameter(prefix+".lengthpArg", Int(1));
    expression_ref lengthp = (identifier("snd"),(identifier("!"),identifier("IModels.models"),i));
    expression_ref lengthp_arg = parameter(prefix+".lengthpArg");
    I.length_p = add_compute_expression( (lengthp, lengthp_arg) );

    // Note that branch_HMM's are per scale and per-imodel.  Construct them in the data_partition.
  }

  // create data partitions
  for(int i=0;i<A.size();i++) 
    data_partitions.push_back( data_partition(this, i, A[i]) );
}

Parameters::Parameters(const module_loader& L,
		       const vector<alignment>& A, const SequenceTree& t,
		       const vector<formula_expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<int>& scale_mapping)
  :Parameters(L, A, t, SMs, s_mapping, vector<formula_expression_ref>{}, vector<int>{}, scale_mapping)
{ }

bool accept_MH(const Probability_Model& P1,const Probability_Model& P2,double rho)
{
  efloat_t p1 = P1.heated_probability();
  efloat_t p2 = P2.heated_probability();

  efloat_t ratio = efloat_t(rho)*(p2/p1);

  if (ratio >= 1.0 or uniform() < ratio) 
    return true;
  else
    return false;
}

