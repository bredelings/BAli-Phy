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
#include "alignment/alignment-util2.H"
#include "likelihood.H"
#include "util.H"
#include "mcmc/proposals.H"
#include "probability/probability.H"
#include "computation/formula_expression.H"
#include "computation/module.H"
#include "computation/operations.H" // for VectorFromList<>
#include "math/exponential.H"

using std::vector;
using std::string;
using std::cerr;
using std::endl;
using std::ostream;

/*
 * DONE:
 *
 * 1. [DONE] Allow defining constructors in files.
 * 2. [DONE] Convert strings to [Char]
 * 3. [DONE] Update probability functions to separate the family from the probability object.
 *    3a. [DONE] Construct the ExpOf transform to make logNormal, logGamma, etc.
 *    3b. [DONE] Choose kernels based on the range, not based on the distribution name.
 * 4. [DONE] Convert Defs to use the machine.
 * 5. [DONE] SYNTAX: replace a ~ b ( c ) with a ~ b
 * 6. [DONE] SYNTAX: external a ~ b [To not declare all parameters]
 *      6a. [DONE] SYNTAX: data a ~ b [Don't treat a as a parameter at all!]
 * 7. [DONE] Allow defs in BUGS files.
 * 8. [DONE] Rationalize C++ operations on expression_ref and formula_expression_ref
 *    - [DONE] Eliminate C++ operators on formula_expression_ref -> use parser instead.
 *    - [DONE] Eliminate C++ operators on expression_ref -> use parser instead.
 *    - [DONE] Make (f,g) only create an apply expression, but NOT substitute.
 *    - [DONE] Make f+g simply append g to f->sub.
 *    - [DONE] Make f*g substitute into f.
 * 9. [DONE] Convert all of distribution-operations.H to the parser.
 * 10. [DONE] Remove arity argument to def_function.
 * 11. [DONE] Process imports
 *     + [DONE] 11a. Process mutually dependent modules.
 *     + [DONE] 11b. Note that clashing declarations are allowed if the symbol is unreferenced!
 * 12. [DONE] Add function to clean up fully resolved symbols to make things look nicer.
 * 13. [DONE] Replace recalc indices with trigers.
 * 14. [DONE] Allow the creation, destruction, initialization, ranges, and MCMC of unnamed parameters.
 * 15. [DONE] Allow printing ints w/o decimal points.
 *
 * 16. [DONE] Make a model-creation monad.
 * 17. [DONE] Eliminate formula_expression_ref.
 * 18. [DONE] Eliminate all notes.
 * 19. [DONE] Add Lexer => allows comments.
 * 20. [DONE] Eliminate module renaming.
 * 21. [DONE] Allow distributions on structures with variable components.
 * 22. [DONE] Name pieces of structures intelligently -- e.g. piA instead of pi!!0
 * 23. [DONE] Allow model files to create models where dimension of parameter depeonds on argument to model.
 * 24. [DONE] Allow creation of parameters in their own namespace.
 * 25. [DONE] Compute the entire probability expression at once, instead of adding pieces incrementally.
 * 26. [DONE] Move the Program from Context to reg_heap.
 * 27. [DONE] Allow adding transition kernels from haskell
 * 28. [DONE] specifying the sampling rate from haskell
 *
 */

/* \todo: List of things to do to clean up programs.
 *
 * See list in models/parameters.C 
 *
 * 1. Efficiently recalculate the probability when only a few densities change.
 *    - Will this require signals? (Signals might also help us to recalculate mu*t to see if anything changed.)
 *    - This will allow us to avoid maintaining a Markov blanket.
 * 2. Make sure we don't read alignments with ^@ characters in the sequences!
 *
 * 3. Eliminate the need for set_branch_mean_tricky( )
 *    - Implement setting D!b only when the change is large enough.
 * 4. Rewrite multi-case code to take patterns in terms of expression_ref's that might be seen from the parser.
 *     + Allows moving towards 16 incrementally.
 * 5. Handle 'where' clauses (e.g. in "alt")
 * 6. Handle guards clauses (e.g. in gdrhs, and gdpat)
 *     + I *think* that guards cannot fail in a way that makes the rule fail and move to the next rule.
 *       (The 'otherwise' rule might be special, though??)
 *     + If failure of all guards leads to failure, then can the guards can be processed as a special RHS?
 *     6b. Handle as-patterns.
 *        case v of x@pat -> body => case v of pat -> (\x->body) v
 *     6c. Handle irrefutable patterns that use ~.
 *        case v of ~h:t -> body
 *     6d. What does it mean (if its true) that irrefutable bindings are only irrefutable at the top level?
 * 8. Make Context load an entire program, instead of adding pieces incrementally.
 *
 * 10. Allow fixing parameters. (e.g. to test the branch-site model under ML)
 * 11. How to specify default priors if model creation is an IO operation?
 * 12. Optimizations
 *     - Perform applications if expression is used only once?
 *     - Remove let bindings for unused variables?
 *     - Merge let bidings with identical bodies?
 *     - Simplify some case expressions based on knowledge of let-bound variable?
 * 13. Print out simpler names than Test.i for parameter i.
 *     - I think parameters are in a separate namespace?
 *     - Perhaps put a '*' on the beginning of the name when comparing with the Haskell namespace?
 *
 * 18. Add ability to change the prior on variables.
 *
 * 19. Add the ability to store newtype definitions.
 *
 * 20. Move to only loading entire programs into a context, where programs are entire module collections.
 *
 * 21. Compare the monadic interface with Acar's interface.
 *
 * 22.
 *     - Handle sequences with lengths not divisible by 3.
 *     - Handle loading alignments with codons not together.
 *     - Handle guessing of alphabets based on frequencies.
 *     - Handle loading of letters like K for DNA -- change to N.
 *     - Could we actually handle all SEEN codon triplets?
 *
 * 23. Store alignments in a more sparse format?
 *
 * 24. Allow generating an alignment (sparse or dense) only when we need it?
 *
 * 25. Eliminate the need for re-evaluate in the substitution likelihood?
 *
 * 26. We seem to have lost a lot of speed (factor of 2) compared to:
VERSION: 2.3.0-devel  [master commit f4e1bbc3+]  (Jan 21 2014 22:45:49)
 *     Perhaps this is entirely related to module loading time?
 *     A lot of the time seems to come from substituting for global identifiers in Module::resolve_symbols
 *
 * 27. Clean up intermediate representations.
 *      - Speed up get_free_indices.
 *      - Make get_bound_indices simpler.
 *      - Make Case statements different in the different representations.
 *      - Speed up let floating.
 *      - Eliminate dynamic casting.
 *
 * 28. Eliminate named dummies in favor of identifiers.
 *
 * 29. Remove sequence data from the alignment!
 *      - Or, at least the alignment we use in Parameters.
 *      - What INTERFACES does the alignment class need to implement?
 *      - Clearing the sequence data seems to work.
 *
 * 30. When do we need to know the alignment MATRIX to construct alignment indices?
 *
 * 31. Split up graph_register.C
 *
 * 32. Rename reg_heap -> something more descriptive/attractive.
 *
 * 33. Begin adding node-based information to new tree class.
 *
 * 34. Try to eliminate the use of the alignment matrix in more areas.
 *     - Then we won't have to construct it right...
 *
 * 35. We are spending 7% of CPU time in read_h_tree.
 *     - Stop calling set_tree( ) in sample-topology-SPR.C!
 *     - Why can't we just remove the call?
 *
 * 36. We are spending 7.5% of CPU time in construct 
 *     - Perform write_match and write_insertions w/ function calls in construct( )
 *
 * 37. Avoid constructing an alignment for (some?) common index-matrix operations.
 *
 * 38. We still need the alignment -- not just the As -- to compute column likelihoods in sample-tri
 *     when the tree changes.
 *     - Instead of fixing this immediately, first just compute A() from H().
 *     - Use H() in substitution.C and index-matrix.C
 *
 * 42. Different results for 25.fasta versus 2.2.0!
 *     - Really?
 *     - Check when this comment (#42) was added.
 *
 * 43. Not printing RS07 model parameters??
 *     - To force all names to be generated before the layout for C1.p is constructed,
 *       I compute the probability at the end of Parameters::Parameters( ).
 */

bool use_internal_index = true;

subA_index_t& data_partition::subA() const
{
  subA_->P = P;
  return *subA_;
}

void data_partition::set_parameters(const Parameters* p)
{
  P = p;
}

const alignment& data_partition::A() const
{
  return P->get_parameter_value(alignment_matrix_index).as_<alignment>();
}

void data_partition::set_alignment(const expression_ref& A2)
{
  const context* C = P;
  const_cast<context*>(C)->set_parameter_value(alignment_matrix_index, A2 );
  invalidate_subA_index_all();
}

void data_partition::recompute_alignment_matrix_from_pairwise_alignments()
{
  assert( variable_alignment_ );

  vector<pairwise_alignment_t> As;
  for(int b=0;b<2*t().n_branches();b++)
    As.push_back(get_pairwise_alignment(b,false));
  
  set_alignment( get_alignment(get_alphabet(), *seqs, *sequences, construct(t(), As)) );
}

TreeInterface data_partition::t() const
{
  return P->t();
}


double data_partition::get_beta() const
{
  return P->get_beta();
}

bool data_partition::pairwise_alignment_for_branch_is_valid(int b) const
{
  expression_ref E = P->get_parameter_value(pairwise_alignment_for_branch[b]);

  if (E.is_int())
    return false;
  else
    return true;
}

void data_partition::variable_alignment(bool b)
{
  variable_alignment_ = b;

  // Ignore requests to turn on alignment variation when there is no imodel or internal nodes
  if (not has_IModel() or A().n_sequences() != t().n_nodes())
    variable_alignment_ = false;

  // turning OFF alignment variation
  if (not variable_alignment()) 
  {
    subA_ = new subA_index_leaf(P, t().n_branches()*2);

    // We just changed the subA index type
    LC.invalidate_all();

    if (A().n_sequences() == t().n_nodes())
      if (not check_leaf_characters_minimally_connected(A(),t()))
	throw myexception()<<"Failing to turn off alignment variability: non-default internal node states";
  }
  // turning ON alignment variation
  else 
  {
    if (use_internal_index)
      subA_ = new subA_index_internal(P, t().n_branches()*2);
    else
      subA_ = new subA_index_leaf(P, t().n_branches()*2);

    assert(has_IModel() and A().n_sequences() == t().n_nodes());
    {
      alignment* A2 = A().clone();
      minimally_connect_leaf_characters(*A2, t());
      set_alignment(A2);
    }
    note_alignment_changed();

    // reset the pairwise alignments.
    for(int b=0;b<t().n_branches();b++)
    {
      int n1 = t().source(b);
      int n2 = t().target(b);
      set_pairwise_alignment(b, A2::get_pairwise_alignment(A(),n1,n2));
    }

    // Minimally connecting leaf characters may remove empty columns, in theory.
    // And we just changed the subA index type
    LC.invalidate_all();
  }
}

bool data_partition::has_IModel() const
{
  int m = P->imodel_index_for_partition(partition_index);
  return (m != -1);
}

const std::vector<Matrix>& data_partition::transition_P(int b) const
{
  b = t().undirected(b);
  assert(b >= 0 and b < t().n_branches());

  return P->evaluate( transition_p_method_indices[b] ).as_<Vector<Matrix>>();
}

int data_partition::n_base_models() const
{
  int s = P->smodel_index_for_partition(partition_index);
  return P->evaluate(P->PC->SModels[s].n_base_models).as_int();
}

int data_partition::n_states() const
{
  int s = P->smodel_index_for_partition(partition_index);
  return P->evaluate(P->PC->SModels[s].n_states).as_int();
}

vector<double> data_partition::distribution() const
{
  int s = P->smodel_index_for_partition(partition_index);
  return P->evaluate(P->PC->SModels[s].distribution).as_<Vector<double>>();
}

vector<unsigned> data_partition::state_letters() const
{
  int s = P->smodel_index_for_partition(partition_index);
  return P->evaluate(P->PC->SModels[s].state_letters).as_<Vector<unsigned>>();
}

vector<double> data_partition::frequencies(int m) const
{
  return P->evaluate( frequencies_indices[m] ).as_<Vector<double>>();
}

expression_ref data_partition::base_model(int m, int b) const
{
  b = t().undirected(b);

  return P->evaluate( base_model_indices(m,b) );
}

const indel::PairHMM& data_partition::get_branch_HMM(int b) const
{
  assert(variable_alignment());

  b = t().undirected(b);

  return P->evaluate( branch_HMM_indices[b] ).as_<indel::PairHMM>();
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
  int m = P->imodel_index_for_partition(partition_index);

  int arg_param_index = P->PC->IModel_methods[m].length_arg_param_index;

  const context* C = P;
  const_cast<context*>(C)->set_parameter_value(arg_param_index, l );

  return P->evaluate( P->PC->IModel_methods[m].length_p ).as_double();
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
  LC.invalidate_branch(t(),b);
}

int data_partition::seqlength(int n) const
{
  int l = P->evaluate(sequence_length_indices[n]).as_int();

  assert(l == A().seqlength(n));

  return l;
}

void data_partition::uniquify_subA_index()
{
  if (subA_->ref_count() > 1)
    subA_ = subA_->clone();

  assert(subA_->ref_count() == 1);
}

void data_partition::invalidate_subA_index_branch(int b)
{
  uniquify_subA_index();

  // propagates outward in both directions
  subA().invalidate_branch(t(),b);
}

void data_partition::invalidate_subA_index_one_branch(int b)
{
  uniquify_subA_index();

  int b2 = t().reverse(b);
  subA().invalidate_one_branch(b);
  subA().invalidate_one_branch(b2);
}

void data_partition::invalidate_subA_index_all()
{
  uniquify_subA_index();

  subA().invalidate_all_branches();
}

void data_partition::subA_index_allow_invalid_branches(bool b)
{
  uniquify_subA_index();

#ifndef NDEBUG
  if (not subA().may_have_invalid_branches())
    check_regenerate(subA(), A(), t());
  subA().check_footprint(A(), t());
#endif

  subA().allow_invalid_branches(b);

#ifndef NDEBUG
  if (not subA().may_have_invalid_branches())
    check_regenerate(subA(), A(), t());
  subA().check_footprint(A(), t());
#endif
}

/// Set the pairwise alignment value, but don't mark the alignment & sequence lengths as changed.
void data_partition::set_pairwise_alignment_(int b, const pairwise_alignment_t& pi,bool require_match_A) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  int B = t().reverse(b);

#ifndef NDEBUG
  if (pairwise_alignment_for_branch_is_valid(b))
  {
    assert(pi == get_pairwise_alignment(b,false));
    assert(pi.flipped() == get_pairwise_alignment(B,false));
  }
  else
  {
    assert(not pairwise_alignment_for_branch_is_valid(B));
  }
#endif

  const context* C = P;
  const_cast<context*>(C)->set_parameter_value(pairwise_alignment_for_branch[b], new pairwise_alignment_t(pi));
  const_cast<context*>(C)->set_parameter_value(pairwise_alignment_for_branch[B], new pairwise_alignment_t(pi.flipped()));

  if (require_match_A)
  {
#ifndef NDEBUG
    int n1 = t().source(b);
    int n2 = t().target(b);
    assert(get_pairwise_alignment(b,false) == A2::get_pairwise_alignment(A(),n1,n2));
    assert(get_pairwise_alignment(B,false) == A2::get_pairwise_alignment(A(),n2,n1));
#endif
  }
}

expression_ref data_partition::get_pairwise_alignment_(int b) const
{
  return P->get_parameter_value(pairwise_alignment_for_branch[b]);
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b, bool require_match_A) const
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: what pairwise alignment are you referring to?";

  //  if (not pairwise_alignment_for_branch_is_valid(b)) std::abort();
  assert(pairwise_alignment_for_branch_is_valid(b));

  if (require_match_A)
  {
#ifndef NDEBUG
    int B = t().reverse(b);
    int n1 = t().source(b);
    int n2 = t().target(b);
    assert(get_pairwise_alignment(b,false) == A2::get_pairwise_alignment(A(),n1,n2));
    assert(get_pairwise_alignment(B,false) == A2::get_pairwise_alignment(A(),n2,n1));
#endif
  }

  return P->get_parameter_value(pairwise_alignment_for_branch[b]).as_<pairwise_alignment_t>();
}

void data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi, bool require_match_A)
{
  note_alignment_changed_on_branch(b);
  set_pairwise_alignment_(b,pi,require_match_A);
}

void data_partition::recompute_pairwise_alignment(int b, bool require_match_A)
{
  int n1 = t().source(b);
  int n2 = t().target(b);
  set_pairwise_alignment(b, A2::get_pairwise_alignment(A(),n1,n2), require_match_A);
}

void data_partition::invalidate_pairwise_alignment_for_branch(int b) const
{
  const context* C = P;
  const_cast<context*>(C)->set_parameter_value(pairwise_alignment_for_branch[b], 0);
}

void data_partition::note_alignment_changed_on_branch(int b)
{
  if (not variable_alignment())
    throw myexception()<<"Alignment variation is OFF: how can the alignment change?";

  b = t().undirected(b);

  int B = t().reverse(b);
  invalidate_pairwise_alignment_for_branch(b);
  invalidate_pairwise_alignment_for_branch(B);

  // If the alignment changes AT ALL, then the mapping from subA columns to alignment columns is broken.
  // Therefore we always mark it as out-of-date and needing to be recomputed.

  // However, LC depends only on the alignment of subA indices from different branches.
  // 
  // If the projected leaf alignment remains unchanged, then the subA columns
  // projected to the leaves remain unchanged.  If we only index these columns, then the
  // get_subA_index( ) will not change if we are using subA_index_leaf.
  //
  if (subA().kind() == subA_index_t::internal_index)
    LC.invalidate_branch_alignment(t(),b);
}

void data_partition::note_alignment_changed()
{
  for(int b=0;b<t().n_branches();b++)
    note_alignment_changed_on_branch(b);

  // this automatically marks all non-leaf sequence lengths for recomputation.
}

/// Set the mean branch length to \a mu
void data_partition::branch_mean_changed()
{
  // the scale of the substitution tree changed
  recalc_smodel();
}

log_double_t data_partition::prior_no_alignment() const 
{
  return 1.0;
}

// We want to decrease 
// (a) the number of times get_counts( ) is called
// (b) the number of times seqlength( ) is called
// (c) the number of times log( ) is called
// This should give a further 6% speedup.

log_double_t data_partition::prior_alignment() const 
{
  if (not variable_alignment()) return 1;

  for(int i=0;i<t().n_branches()*2;i++)
    assert(pairwise_alignment_for_branch_is_valid(i));

  log_double_t Pr = P->evaluate(alignment_prior_index).as_log_double();

  assert(not different(Pr, ::prior_HMM(*this)));

  return Pr;
}

log_double_t data_partition::prior() const 
{
  return prior_alignment() * prior_no_alignment();
}


log_double_t data_partition::likelihood() const 
{
    return substitution::Pr(*this);
}

log_double_t data_partition::heated_likelihood() const 
{
  // Don't waste time calculating likelihood if we're sampling from the prior.
  if (get_beta() == 0)
    return 1;
  else
    return pow(likelihood(),get_beta());
}

data_partition::data_partition(Parameters* p, int i, const alignment& AA)
  :P(p),
   DPC(new data_partition_constants),
   partition_index(i),
   pairwise_alignment_for_branch(2*t().n_branches()),
   sequence_length_indices(AA.n_sequences(),-1),
   transition_p_method_indices(t().n_branches(),-1),
   variable_alignment_( has_IModel() ),
   seqs(AA.seqs()),
   sequences( alignment_letters(AA, t().n_leaves()) ),
   a(AA.get_alphabet().clone()),
   LC(t(), *this),
   branch_HMM_type(t().n_branches(),0)
{
  int B = t().n_branches();

  if (variable_alignment() and use_internal_index)
    subA_ = new subA_index_internal(P, B*2);
  else
    subA_ = new subA_index_leaf(P, B*2);

  string invisible_prefix = "*P"+convertToString(i+1)+".";
  alignment_matrix_index = p->add_parameter(invisible_prefix + "A", AA);
  if (variable_alignment())
  {
    string prefix = "P"+convertToString(i+1)+".";
    for(int b=0;b<pairwise_alignment_for_branch.size();b++)
      pairwise_alignment_for_branch[b] = p->add_parameter(prefix+"a"+convertToString(b), 0);

    for(int b=0;b<t().n_branches();b++)
    {
      int n1 = t().source(b);
      int n2 = t().target(b);
      set_pairwise_alignment(b, A2::get_pairwise_alignment(A(),n1,n2));
    }
  }

  const int n_base_smodels = n_base_models();
  //  const int n_states = state_letters().size();
  const int scale_index = P->scale_index_for_partition(partition_index);
  const int smodel_index = P->smodel_index_for_partition(partition_index);
  const int imodel_index = P->imodel_index_for_partition(partition_index);

  // Add method indices for calculating transition matrices.
  {
    expression_ref E = P->get_expression(P->PC->branch_transition_p_indices(scale_index, smodel_index));
    for(int b=0;b<B;b++)
      transition_p_method_indices[b] = p->add_compute_expression( (identifier("!"), E, b) );
  }

  // Add method indices for calculating base models and frequencies
  base_model_indices.resize(n_base_smodels, B);
  {
    expression_ref F = P->get_expression(P->PC->SModels[smodel_index].frequencies);
    expression_ref BM = P->get_expression(P->PC->SModels[smodel_index].base_model);
    for(int m=0;m<n_base_smodels;m++)
    {
      frequencies_indices.push_back( p->add_compute_expression( (F,m) ) );
      for(int b=0;b<B;b++)
	base_model_indices(m,b) = p->add_compute_expression((BM,m,b));
    }
  }

  // Add method indices for calculating branch HMMs

  if (imodel_index != -1)
  {
    // D = Params.substitutionBranchLengths!scale_index
    expression_ref D = (identifier("!"),identifier("Params.substitutionBranchLengths"),scale_index);
    expression_ref heat = parameter("Heat.beta");
    expression_ref training = parameter("*IModels.training");
    expression_ref model = (identifier("!"),identifier("IModels.models"),imodel_index);

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
      // (fst IModels.models!imodel_index) D b heat training
      int index = p->add_compute_expression( (identifier("!"), hmms, b) );
      branch_HMM_indices.push_back( index );
    }

    expression_ref tree = p->my_tree();

    alignment_prior_index = p->add_compute_expression( (identifier("alignment_pr"), as, tree, hmms, model) );

    for(int n=0;n<t().n_nodes();n++)
    {
      expression_ref L = A().seqlength(n);
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

vector<int> edges_connecting_to_node(const Tree& T, int n)
{
  vector<const_branchview> branch_list;
  append(T.node(n).branches_out(),branch_list);
  
  vector<int> branch_list_;
  for(auto b: branch_list)
    branch_list_.push_back(int(b));

  return branch_list_;
}

tree_constants::tree_constants(Parameters* p, const SequenceTree& T)
  :n_leaves(T.n_leaves()),
   node_labels(T.get_labels())
{
  /*------------------------- Create the tree structure -----------------------*/
  vector<expression_ref> node_branches;
  for(int n=0; n < T.n_nodes(); n++)
  {
    auto edges = edges_connecting_to_node(T,n);
    vector<int> p_node;
    expression_ref node;
    
    if (T.node(n).is_leaf_node())
    {
      node = List(edges.front());
      p_node = { -1 };
    }
    else
    {
      node = List();
      for(int i=0;i<edges.size();i++)
      {
	const auto& edge = edges[i];
	string name = "*MyTree.nodeBranches"+convertToString(n) + "." + convertToString(i);
	p_node.push_back( p->add_parameter(name,edge) );
	node  = Cons * parameter(name) * node;
      }
    }
    
    parameters_for_tree_node.push_back ( p_node );
    node_branches.push_back( node );
  }
  expression_ref node_branches_array = (identifier("listArray'"),get_list(node_branches));

  vector<expression_ref> branch_nodes;
  for(int b=0; b < 2*T.n_branches(); b++)
  {
    expression_ref source = T.directed_branch(b).source().name();
    int p_source = -1;
    if (not T.directed_branch(b).source().is_leaf_node())
    {
      string name_source = "*MyTree.branch"+convertToString(b)+"source"; 
      p_source = p->add_parameter(name_source,source);
      source = parameter(name_source);
    }

    expression_ref target = T.directed_branch(b).target().name();
    int p_target = -1;
    if (not T.directed_branch(b).target().is_leaf_node())
    {
      string name_target = "*MyTree.branch"+convertToString(b)+"target"; 
      p_target = p->add_parameter(name_target,target);
      target = parameter(name_target);
    }
    int reverse_branch = T.directed_branch(b).reverse();
    parameters_for_tree_branch.push_back( {p_source,p_target} );
    branch_nodes.push_back( Tuple(source, target, reverse_branch) );
  }
  expression_ref branch_nodes_array = (identifier("listArray'"),get_list(branch_nodes));

  expression_ref tree_con = lambda_expression( constructor("Tree.Tree",4) );

  tree_head = p->add_compute_expression( (tree_con, node_branches_array, branch_nodes_array, T.n_nodes(), T.n_branches()));
  
  // Create the parameters that hold branch lengths
  for(int b=0;b<T.n_branches();b++)
  {
    int index = p->add_parameter("*T"+convertToString(b+1), T.branch(b).length());
    branch_length_parameters.push_back(index);
  }
}

const data_partition& Parameters::get_data_partition(int i) const
{
  data_partitions[i].set_parameters(this);
  return data_partitions[i];
}

data_partition& Parameters::get_data_partition(int i)
{
  data_partitions[i].set_parameters(this);
  return data_partitions[i];
}

void Parameters::set_beta(double b)
{
  set_parameter_value(0,b);
}

double Parameters::get_beta() const
{
  return get_parameter_value(0).as_double();
}

TreeInterface Parameters::t() const
{
  return {this};
}

vector<string> Parameters::get_labels() const
{
  return TC->node_labels;
}

void Parameters::reconnect_branch(int s1, int t1, int t2, bool safe)
{
  uniquify_subA_indices();

  int b1 = t().find_branch(s1,t1);
  int b2 = t().reverse(b1);

  if (safe)
  {
    LC_invalidate_branch(b1);
    invalidate_subA_index_branch(b1);
    note_alignment_changed_on_branch(t().find_branch(s1,t1));
  }
  
  t().reconnect_branch(s1, t1, t2);

  if (safe)
  {
    LC_invalidate_branch(b1);
    invalidate_subA_index_branch(b1);
    note_alignment_changed_on_branch(t().find_branch(s1,t2));
  }
  
}

void Parameters::begin_modify_topology()
{
  t().begin_modify_topology();
}

void Parameters::end_modify_topology()
{
  t().end_modify_topology();
}

// This could create loops it we don't check that the subtrees are disjoint.
// br{1,2} point out of the subtrees.  b{1,2} point into the subtrees, towards the other subtree.
void Parameters::exchange_subtrees(int br1, int br2)
{
  int b1 = t().reverse(br1);
  int b2 = t().reverse(br2);

  int s1 = t().source(b1);
  int t1 = t().target(b1);

  int s2 = t().source(b2);
  int t2 = t().target(b2);

  //  assert(not t().subtree_contains(br1,s2));
  //  assert(not t().subtree_contains(br2,s1));

  begin_modify_topology();
  reconnect_branch(s1,t1,t2,true);
  reconnect_branch(s2,t2,t1,true);
  end_modify_topology();
}

#include "dp/hmm.H"
#include "dp/5way.H"

void disconnect(alignment& A, const vector<int>& nodes)
{
  for(int c=0; c<A.length(); c++)
  {
    A.set_value(c,nodes[4],alphabet::gap);
    A.set_value(c,nodes[5],alphabet::gap);
  }
}

void disconnect(vector<HMM::bitmask_t>& a123456)
{
  for(auto& col:a123456)
  {
    col.set(4,false);
    col.set(5,false);
  }
}

void minimally_connect(alignment& A, const vector<int>& nodes)
{
  for(int c=0; c<A.length(); c++)
  {
    bool n0 = A.character(c, nodes[0]);
    bool n1 = A.character(c, nodes[1]);
    bool n2 = A.character(c, nodes[2]);
    bool n3 = A.character(c, nodes[3]);

    if ((n0 or n1) and (n2 or n3))
    {
      A.set_value(c, nodes[4], alphabet::not_gap);
      A.set_value(c, nodes[5], alphabet::not_gap);
    }

    if (n0 and n1)
      A.set_value(c, nodes[4], alphabet::not_gap);

    if (n2 and n3)
      A.set_value(c, nodes[5], alphabet::not_gap);
  }
}

void minimally_connect(vector<HMM::bitmask_t>& a123456)
{
  for(auto& col:a123456)
  {
    if ((col.test(0) or col.test(1)) and (col.test(2) or col.test(3)))
    {
      col.set(4);
      col.set(5);
    }

    if (col.test(0) and col.test(1))
      col.set(4);

    if (col.test(2) and col.test(3))
      col.set(5);
  }
}

void Parameters::NNI(const tree_edge& B1, const tree_edge& B2)
{
  int b1 = t().find_branch(B1);
  int b2 = t().find_branch(B2);
  NNI(b1, b2);
}



// br1/b1 and br2/b2 point outwards, away from the other subtrees.
void Parameters::NNI(int b1, int b2)
{
  int s1 = t().source(b1);
  int t1 = t().target(b1);

  int s2 = t().source(b2);
  int t2 = t().target(b2);

  assert(t().is_connected(s1,s2));
  int b45 = t().find_branch(s1,s2);

  // 1. Get alignments of sequences 123456
  auto order = A5::get_nodes(t(),b45);
  auto& nodes = order.nodes;
  assert(nodes[4] == s1);
  assert(nodes[5] == s2);

  if (nodes[0] != t1) std::swap(nodes[0],nodes[1]);
  assert(nodes[0] == t1);
  
  if (nodes[2] != t2) std::swap(nodes[2],nodes[3]);
  assert(nodes[2] == t2);

  // OK, br1 is nodes[0]<->nodes[4] and br2 is nodes[2]<->nodes[5]
  
  vector<vector<HMM::bitmask_t>> a123456(n_data_partitions());
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      a123456[i] = A5::get_bitpath((*this)[i], order);

  // 3. Perform NNI
  exchange_subtrees(b1, b2);  // alter tree
  std::swap(nodes[0],nodes[2]); // alter nodes
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
      for(auto& col: a123456[i]) // alter matrix
      {
	auto col2 = col;
	col.set(0,col2.test(2));
	col.set(2,col2.test(0));
      }
    
  // 4. Update the alignment matrix (alignment)
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
    {
      alignment* A = get_data_partition(i).A().clone();
      disconnect(*A,nodes);
      minimally_connect(*A,nodes);
      get_data_partition(i).set_alignment(A);
    }
  /*
  // 5. Fix-up the alignment matrix (bits)
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
    {
      disconnect(a123456[i]);
      minimally_connect(a123456[i]);
    }

  // 6. Set the pairwise alignments.
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
    {
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[0],nodes[4]), get_pairwise_alignment_from_bits(a123456[i], 0, 4), true);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[1],nodes[4]), get_pairwise_alignment_from_bits(a123456[i], 1, 4), true);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[2],nodes[5]), get_pairwise_alignment_from_bits(a123456[i], 2, 5), true);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[3],nodes[5]), get_pairwise_alignment_from_bits(a123456[i], 3, 5), true);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[4],nodes[5]), get_pairwise_alignment_from_bits(a123456[i], 4, 5), true);
    }
  */

  // 6. Set the pairwise alignments.
  for(int i=0;i<n_data_partitions();i++)
    if (get_data_partition(i).variable_alignment())
    {
      const alignment& A = get_data_partition(i).A();
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[0],nodes[4]), A2::get_pairwise_alignment(A, nodes[0], nodes[4]), false);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[1],nodes[4]), A2::get_pairwise_alignment(A, nodes[1], nodes[4]), false);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[2],nodes[5]), A2::get_pairwise_alignment(A, nodes[2], nodes[5]), false);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[3],nodes[5]), A2::get_pairwise_alignment(A, nodes[3], nodes[5]), false);
      get_data_partition(i).set_pairwise_alignment(t().find_branch(nodes[4],nodes[5]), A2::get_pairwise_alignment(A, nodes[4], nodes[5]), false);
    }
}

int Parameters::SPR(const tree_edge& B1, const tree_edge& B2, int branch_to_move)
{
  int b1 = t().find_branch(B1);
  int b2 = t().find_branch(B2);
  return SPR(b1, b2, branch_to_move);
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
/// Got m1<--->x1<--->m2 and n1<--->n2, and trying to move x1 onto (n1,n2)
int Parameters::SPR(int br1, int br2, int branch_to_move)
{
  int x1 = t().source(br1);
  int x2 = t().target(br1);

  std::vector<int> m_branches = t().branches_after(t().find_branch(x2,x1));
  assert(m_branches.size() == 2);
  int m1 = t().target(m_branches[0]);
  int m2 = t().target(m_branches[1]);

  int n1 = t().source(br2);
  int n2 = t().target(br2);

  // If we are already attached to br2, then return.
  if (n1 == x1 or n2 == x1) return -1;

  //-------------------- Correctly order m1 and m2 ----------------------//
  // Preserve the name of the branch with the smaller name (to avoid renaming leaf branches!)
  // (The name of the x<--->n1 branch gets preserved)
  if (branch_to_move == -1)
  {
    if (t().undirected(t().find_branch(m1,x1)) > t().undirected(t().find_branch(m2,x1)) )
      std::swap(m1,m2);
  }
  // ensure that (x,m2) is the branch to move
  else
  {
    if (t().find_branch(m1,x1) == branch_to_move or t().find_branch(x1,m1) == branch_to_move)
      std::swap(m1,m2);
    else if (t().find_branch(m2,x1) == branch_to_move or t().find_branch(x1,m2) == branch_to_move)
      ;
    else
      std::abort(); // we couldn't find the branch to move!
  }

  //-------------------- Correctly order n1 and n2 ----------------------//
  // choose sub-branch to give the new name to. (It will go to the one pointed to by b2)
  if (n1 > n2)
    std::swap(n1,n2);

  //------ Merge the branches (m1,x1) and (x1,m2) -------//
  int dead_branch = t().undirected(t().find_branch(m2,x1));

  setlength( t().find_branch(m1,x1), t().branch_length(t().find_branch(m1,x1)) + t().branch_length(t().find_branch(m2,x1)) );

  setlength( t().find_branch(m2,x1), 0.0);

  //------------ Reconnect the branches ---------------//

  begin_modify_topology();

  // Reconnect (m1,x) to m2, making x a degree-2 node
  // This leaves m1 connected to its branch, so m1 can be a leaf.
  assert(not t().is_leaf_node(m2) );
  reconnect_branch(m1, x1, m2, true);

  // Reconnect (x,m2) to n2, leaving x a degree-2 node
  assert(not t().is_leaf_node(m2));
  reconnect_branch(x1, m2, n2, true);

  // Reconnect (n1,n2) to x, making x a degree-3 node again.
  // This leaves n1 connected to its branch, so n1 can be a leaf.
  assert(not t().is_leaf_node(n2));
  reconnect_branch(n1, n2, x1, true);

  end_modify_topology();

  recompute_pairwise_alignment(t().find_branch(m1,m2));

  return dead_branch;
}

void Parameters::show_h_tree() const
{
  for(int b=0; b < 2*t().n_branches(); b++)
  {
    auto source = get_parameter_value(TC->parameters_for_tree_branch[b].first ).as_int();
    auto target = get_parameter_value(TC->parameters_for_tree_branch[b].second).as_int();
    std::cerr<<"branch "<<b<<": ("<<source<<","<<target<<")     "<<t().branch_length(b)<<"\n";
  }
}

log_double_t Parameters::prior_no_alignment() const 
{
  log_double_t Pr = Model::prior();

  // prior on the topology and branch lengths
  Pr *= ::prior(*this, t(), 1.0);

  // prior for each branch being aligned/unaliged
  if (variable_alignment()) 
  {
    const double p_unaligned = load_value("P_aligned",0.0);

    log_double_t pNA = p_unaligned;

    log_double_t pA = (1.0 - p_unaligned);

    for(int b=0;b<t().n_branches();b++)
      if (not branch_HMM_type(b))
	Pr *= pA;
      else
	Pr *= pNA;
  }

  // prior on parameters in data partitions
  for(int i=0;i<n_data_partitions();i++)
    Pr *= get_data_partition(i).prior_no_alignment();

  return Pr;
}

log_double_t Parameters::prior_alignment() const 
{
  log_double_t Pr = 1;

  for(int i=0;i<n_data_partitions();i++) 
    Pr *= get_data_partition(i).prior_alignment();

  return Pr;
}

log_double_t Parameters::prior() const 
{
  return prior_no_alignment() * prior_alignment();
}

log_double_t Parameters::likelihood() const 
{
  log_double_t Pr = 1;
  for(int i=0;i<n_data_partitions();i++) 
    Pr *= get_data_partition(i).likelihood();
  return Pr;
}

log_double_t Parameters::heated_likelihood() const 
{
  log_double_t Pr = 1;

  for(int i=0;i<n_data_partitions();i++) 
    Pr *= get_data_partition(i).heated_likelihood();

  return Pr;
}

void Parameters::recalc_smodels() 
{
  for(int i=0;i<n_smodels();i++)
    recalc_smodel(i);
}

void Parameters::recalc_smodel(int m) 
{
  for(int i=0;i<n_data_partitions();i++) 
  {
    if (smodel_index_for_partition(i) == m) 
    {
      // recompute cached computations
      get_data_partition(i).recalc_smodel();
    }
  }
}

void Parameters::select_root(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    ::select_root(t(), b, get_data_partition(i).LC);
}

void Parameters::set_root(int node)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.root = node;
}

void Parameters::LC_invalidate_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    get_data_partition(i).LC.invalidate_branch(t(),b);
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
    if (0 <= index and index < n_scales())
    {
      int s = index;

      assert(includes(triggers(),s));
      assert(0 <= s and s < n_scales());
      
      // Change branch lengths for the s-th scale
      assert(PC->branch_length_indices[s].size() == t().n_branches());
      for(int b=0;b<t().n_branches();b++)
      {
	double rate = get_parameter_value(branch_mean_index(s)).as_double();;
	double delta_t = t().branch_length(b);

	context::set_parameter_value(PC->branch_length_indices[s][b], rate*delta_t);
      }

      // notify partitions with scale 'p' that their branch mean changed
      for(int p=0;p<n_data_partitions();p++)
	if (scale_index_for_partition(p) == s)
	  get_data_partition(p).branch_mean_changed();
    }
  }
  triggers().clear();

  // Check if any computions that likelihood caches depend on have changed.
  for(int p=0;p<n_data_partitions();p++)
  {
    bool everything_changed = false;
    for(int f_index: get_data_partition(p).frequencies_indices)
      if (not compute_expression_is_up_to_date(f_index))
	everything_changed = true;

    if (everything_changed)
      get_data_partition(p).recalc_smodel();
    else
    {
      const vector<int>& v = get_data_partition(p).transition_p_method_indices;
      for(int b = 0; b < v.size(); b++)
      {
	int tp_index = v[b];
	if (not compute_expression_is_up_to_date(tp_index))
	  get_data_partition(p).setlength(b); // just invalidate caches
      }
    }
  }
}

object_ptr<const alphabet> Parameters::get_alphabet_for_smodel(int s) const
{
  return evaluate(PC->SModels[s].get_alphabet).assert_is_a<alphabet>();
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
  // this is setlength_unsafe( ) .. but computes the undirected name.
  b = t().undirected(b);

  t().set_branch_length(b, l);

  // Update D parameters
  for(int s=0; s<n_scales(); s++) 
  {
    double rate = get_parameter_value(branch_mean_index(s)).as_double();
    double delta_t = t().branch_length(b);
    
    context::set_parameter_value(PC->branch_length_indices[s][b], rate * delta_t);
  }
}

void Parameters::setlength_unsafe(int b,double l) 
{
  t().set_branch_length(b, l);
}

void Parameters::setlength(int b,double l) 
{
  b = t().undirected(b);

  t().set_branch_length(b, l);

  // Update D parameters
  for(int s=0; s<n_scales(); s++) 
  {
    double rate = get_parameter_value(branch_mean_index(s)).as_double();
    double delta_t = t().branch_length(b);
    
    context::set_parameter_value(PC->branch_length_indices[s][b], rate * delta_t);
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
  return n_scales();
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
  context::set_parameter_value(branch_mean_index(i), x );
}

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
  int s = scale_index_for_partition(p);
  return get_parameter_value(branch_mean_index(s)).as_double();
}

expression_ref Parameters::my_tree() const
{
  return get_expression(TC->tree_head);
}

parameters_constants::parameters_constants(const vector<alignment>& A, const SequenceTree& t,
					   const vector<expression_ref>& SMs,
					   const vector<int>& s_mapping,
					   const vector<expression_ref>& IMs,
					   const vector<int>& i_mapping,
					   const vector<int>& scale_mapping)
  :smodel_for_partition(s_mapping),
   IModel_methods(IMs.size()),
   imodel_for_partition(i_mapping),
   scale_for_partition(scale_mapping),
   n_scales(max(scale_mapping)+1),
   TC(star_tree(t.get_leaf_labels())),
   branch_HMM_type(t.n_branches(),0)
{
  // check that smodel mapping has correct size.
  if (smodel_for_partition.size() != A.size())
    throw myexception()<<"There are "<<A.size()
		       <<" data partitions, but you mapped smodels onto "
		       <<smodel_for_partition.size();

  // check that we only map existing smodels to data partitions
  for(int i=0;i<smodel_for_partition.size();i++) {
    int m = smodel_for_partition[i];
    if (m >= SMs.size())
      throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			 <<" because there are only "<<SMs.size()<<" smodels.";
  }
}

Parameters::Parameters(const module_loader& L,
		       const vector<alignment>& A, const SequenceTree& tt,
		       const vector<expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<expression_ref>& IMs,
		       const vector<int>& i_mapping,
		       const vector<int>& scale_mapping)
  :Model(L),
   PC(new parameters_constants(A,tt,SMs,s_mapping,IMs,i_mapping,scale_mapping)),
   updown(-1)
{
  // \todo FIXME:cleanup|fragile - Don't touch C here directly!
  *this += { "SModel","Distributions","Range","PopGen","Alignment","IModel" };
  
  // Don't call set_parameter_value here, because recalc( ) depends on branch_length_indices, which is not ready.

  PC->constants.push_back(-1);

  add_parameter("Heat.beta", 1.0);

  // Add a Main.mu<i> parameter for each scale.
  {
    expression_ref mus = model_expression({identifier("branch_mean_model"), n_scales()});
    evaluate_expression( perform_exp(mus) );
  }

  for(int i=0;i<n_scales();i++)
  {
    string mu_name = "Main.mu"+convertToString(i+1);
    int trigger = add_compute_expression( (identifier("trigger_on"),parameter(mu_name),i) );
    set_re_evaluate(trigger, true);
  }

  branches_from_affected_node.resize(tt.n_nodes());

  TC = new tree_constants(this, tt);
  
  t().read_tree(tt);

#ifndef NDEBUG
  evaluate_expression( (identifier("numNodes"), my_tree()));
  evaluate_expression( (identifier("numBranches"), my_tree()));
  evaluate_expression( (identifier("edgesOutOfNode"), my_tree(), 0));
  evaluate_expression( (identifier("neighbors"), my_tree(), 0));
  evaluate_expression( (identifier("nodesForEdge"),my_tree(), 0));
  //  evaluate_expression( (identifier("edgeForNodes"), my_tree(), (identifier("nodesForEdge"),my_tree(), 0))).as_int();
  for(int b=0; b < 2*tt.n_branches(); b++)
  {
    vector<const_branchview> branch_list;
    append(tt.directed_branch(b).branches_before(),branch_list);
    vector<int> branch_list_;
    for(auto b: branch_list)
      branch_list_.push_back(b);

    vector<int> b2 = evaluate_expression( (identifier("listToVectorInt"),((identifier("edgesBeforeEdge"),my_tree(),b)))).as_<Vector<int>>();
    assert(b2.size() == branch_list_.size());
    for( int i: branch_list_)
      assert(includes(b2,i));
  }
#endif

  // register the substitution models as sub-models
  for(int i=0;i<SMs.size();i++) 
  {
    string prefix = "S" + convertToString(i+1);

    expression_ref smodel = SMs[i];

    PC->SModels.push_back( smodel_methods( perform_exp(smodel,prefix), *this) );
  }

  // register the indel models as sub-models
  vector<expression_ref> imodels_;
  for(int i=0;i<n_imodels();i++) 
  {
    string prefix = "I" + convertToString(i+1);

    imodels_.push_back(perform_exp(IMs[i],prefix));
  }

  add_parameter("*IModels.training", false);

  Module imodels_program("IModels");
  imodels_program.def_function("models", (identifier("listArray'"), get_list(imodels_)));
  (*this) += imodels_program;
  
  /*------------------------- Add commands to log all parameters created before this point. ------------------------*/
  // don't constrain any branch lengths
  for(int b=0;b<PC->TC.n_branches();b++)
    PC->TC.branch(b).set_length(-1);

  // Add and initialize variables for branch *lengths*: scale<s>.D<b>
  for(int s=0;s<n_scales();s++)
  {
    string prefix= "*Scale" + convertToString(s+1);
    PC->branch_length_indices.push_back(vector<int>());
    for(int b=0;b<t().n_branches();b++)
    {
      double rate = get_parameter_value(branch_mean_index(s)).as_double();
      double delta_t = t().branch_length(b);

      string name = "d" + convertToString(b+1);
      int index = add_parameter(prefix+"."+name, rate * delta_t);
      PC->branch_length_indices[s].push_back(index);
    }
  }

  // Add and initialize variables for branch *categories*: branch_cat<b>
  vector<expression_ref> branch_categories;
  for(int b=0;b<t().n_branches();b++)
  {
    string name = "*Main.branchCat" + convertToString(b+1);
    add_parameter(name, 0);
    branch_categories.push_back(parameter(name));
  }
  expression_ref branch_cat_list = get_expression( add_compute_expression( (get_list(branch_categories) ) ) );

  expression_ref substitutionBranchLengthsList;
  {
    vector<expression_ref> SBLL;
    for(int s=0;s < n_branch_means(); s++)
    {
      string prefix= "*Scale" + convertToString(s+1);
      // Get a list of the branch LENGTH (not time) parameters
      vector<expression_ref> D;
      for(int b=0;b<t().n_branches();b++)
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
  (*this) += parameter_program;

  // register the cached transition_p indices
  PC->branch_transition_p_indices.resize(n_branch_means(), n_smodels());
  for(int s=0;s < n_branch_means(); s++)
  {
    // Better yet, make a substitutionBranchLengths!scale!branch that can be referenced elsewhere.
    expression_ref DL = (identifier("!"),identifier("Params.substitutionBranchLengths"),s);

    // Here, for each (scale,model) pair we're construction a function from branches -> Vector<transition matrix>
    for(int m=0;m < n_smodels(); m++)
    {
      expression_ref S = get_expression(PC->SModels[m].main);
      //expression_ref V = identifier("listToVectorMatrix");
      expression_ref V = identifier("vector_Matrix_From_List");
      //expression_ref I = 0;
      expression_ref I = (identifier("!!"),branch_cat_list,v1);
      expression_ref E = (identifier("mkArray"), t().n_branches(), v1^(V,(identifier("branchTransitionP"), (identifier("getNthMixture"),S,I), (identifier("!"), DL, v1) ) ) );
      PC->branch_transition_p_indices(s,m) = add_compute_expression(E);
    }
  }

  // Register compute expressions for branch HMMs and sequence length distributions
  for(int i=0;i<n_imodels();i++) 
  {
    imodel_methods& I = PC->IModel_methods[i];
    string prefix = "*I" + convertToString(i+1);

    I.length_arg_param_index = add_parameter(prefix+".lengthpArg", 1);
    expression_ref lengthp = (identifier("snd"),(identifier("!"),identifier("IModels.models"),i));
    expression_ref lengthp_arg = parameter(prefix+".lengthpArg");
    I.length_p = add_compute_expression( (lengthp, lengthp_arg) );

    // Note that branch_HMM's are per scale and per-imodel.  Construct them in the data_partition.
  }

  // create data partitions
  for(int i=0;i<A.size();i++) 
    data_partitions.push_back( data_partition(this, i, A[i]) );

  // FIXME: We currently need this to make sure all parameters get instantiated before we finish the constructor.
  probability();
}

Parameters::Parameters(const module_loader& L,
		       const vector<alignment>& A, const SequenceTree& t,
		       const vector<expression_ref>& SMs,
		       const vector<int>& s_mapping,
		       const vector<int>& scale_mapping)
  :Parameters(L, A, t, SMs, s_mapping, vector<expression_ref>{}, vector<int>{}, scale_mapping)
{ }

bool accept_MH(const Model& P1,const Model& P2,double rho)
{
  log_double_t p1 = P1.heated_probability();
  log_double_t p2 = P2.heated_probability();

  log_double_t ratio = log_double_t(rho)*(p2/p1);

  if (ratio >= 1.0 or uniform() < ratio) 
    return true;
  else
    return false;
}

