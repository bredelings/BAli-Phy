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
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "prior.H"
#include "util.H"
#include "mcmc/proposals.H"
#include "probability/probability.H"
#include "computation/model_expression.H"
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
 * 31. Split up graph_register.C
 *
 * 32. Rename reg_heap -> something more descriptive/attractive.
 *
 * 33. Begin adding node-based information to new tree class.
 *
 * 35. We are spending 7% of CPU time in read_h_tree.
 *     - Stop calling set_tree( ) in sample-topology-SPR.C!
 *     - Why can't we just remove the call?
 *
 * 36. We are spending 7.5% of CPU time in construct 
 *     - Perform write_match and write_insertions w/ function calls in construct( )
 *
 * 42. Different results for 25.fasta versus 2.2.0!
 *     - Really?
 *     - Check when this comment (#42) was added.
 *
 * 43. Not printing RS07 model parameters??
 *     - To force all names to be generated before the layout for C1.p is constructed,
 *       I compute the probability at the end of Parameters::Parameters( ).
 */

/// Is the alignment allowed to vary?
bool data_partition::variable_alignment() const
{
    return P->variable_alignment() and has_IModel();
}

const data_partition_constants& data_partition::DPC() const
{
    return P->PC->DPC[partition_index];
}

const alphabet& data_partition::get_alphabet() const
{
    return *DPC().a;
}

alignment data_partition::A() const
{
    vector<pairwise_alignment_t> As;
    for(int b=0;b<2*t().n_branches();b++)
	As.push_back(get_pairwise_alignment(b));
  
    return get_alignment(get_alphabet(), DPC().seqs, DPC().sequences, construct(t(), As));
}

TreeInterface data_partition::t() const
{
    return P->t();
}


double data_partition::get_beta() const
{
    return P->get_beta();
}

int data_partition::subst_root() const {
    return P->subst_root();
}

bool data_partition::has_IModel() const
{
    int m = P->imodel_index_for_partition(partition_index);
    return (m != -1);
}

const std::vector<int>& data_partition::get_sequence(int i) const
{
    return P->evaluate( DPC().leaf_sequence_indices[i] ).as_<Vector<int>>();
}

const std::vector<Matrix>& data_partition::transition_P(int b) const
{
    b = t().undirected(b);
    assert(b >= 0 and b < t().n_branches());

    return P->evaluate( DPC().transition_p_method_indices[b] ).as_<Vector<Matrix>>();
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

Matrix data_partition::WeightedFrequencyMatrix() const
{
    int s = P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].weighted_frequency_matrix).as_<Box<Matrix>>();
}

Matrix data_partition::FrequencyMatrix() const
{
    int s = P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].frequency_matrix).as_<Box<Matrix>>();
}

vector<unsigned> data_partition::state_letters() const
{
    int s = P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].state_letters).as_<Vector<unsigned>>();
}

expression_ref data_partition::base_model(int m, int b) const
{
    b = t().undirected(b);

    return P->evaluate( DPC().base_model_indices(m,b) );
}

const indel::PairHMM& data_partition::get_branch_HMM(int b) const
{
    assert(variable_alignment());

    b = t().undirected(b);

    return P->evaluate( DPC().branch_HMM_indices[b] ).as_<indel::PairHMM>();
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

int data_partition::seqlength(int n) const
{
    if (n < DPC().sequences.size())
	return DPC().sequences[n].size();

    int l = P->evaluate(DPC().sequence_length_indices[n]).as_int();

    return l;
}

/// Set the pairwise alignment value, but don't mark the alignment & sequence lengths as changed.
void data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi)
{
    int B = t().reverse(b);
    assert(get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped());
    const context* C = P;
    const_cast<context*>(C)->set_parameter_value(DPC().pairwise_alignment_for_branch[b], new pairwise_alignment_t(pi));
    const_cast<context*>(C)->set_parameter_value(DPC().pairwise_alignment_for_branch[B], new pairwise_alignment_t(pi.flipped()));

    assert(get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped());
}

const matrix<int>& data_partition::alignment_constraint() const
{
    return DPC().alignment_constraint;
}

expression_ref data_partition::get_pairwise_alignment_(int b) const
{
    return P->get_parameter_value(DPC().pairwise_alignment_for_branch[b]);
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b) const
{
    return get_pairwise_alignment_(b).as_<pairwise_alignment_t>();
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

    log_double_t Pr = P->evaluate(DPC().alignment_prior_index).as_log_double();

    return Pr;
}

log_double_t data_partition::prior() const 
{
    return prior_alignment() * prior_no_alignment();
}

const Likelihood_Cache_Branch& data_partition::cache(int b) const
{
    return P->evaluate(DPC().conditional_likelihoods_for_branch[b]).as_<Likelihood_Cache_Branch>();
}

log_double_t data_partition::likelihood() const 
{
    substitution::total_likelihood++;
    return P->evaluate(DPC().likelihood_index).as_log_double();
}

log_double_t data_partition::heated_likelihood() const 
{
    // Don't waste time calculating likelihood if we're sampling from the prior.
    if (get_beta() == 0)
	return 1;
    else
	return pow(likelihood(),get_beta());
}

data_partition::data_partition(const Parameters* p, int i)
    :P(p),partition_index(i)
{ }

data_partition_constants::data_partition_constants(Parameters* p, int i, const alignment& AA)
    :pairwise_alignment_for_branch(2*p->t().n_branches()),
     conditional_likelihoods_for_branch(2*p->t().n_branches()),
     leaf_sequence_indices(p->t().n_leaves(),-1),
     sequence_length_indices(AA.n_sequences(),-1),
     transition_p_method_indices(p->t().n_branches(),-1),
     seqs(AA.seqs()),
     sequences( alignment_letters(AA, p->t().n_leaves()) ),
     a(AA.get_alphabet().clone()),
     branch_HMM_type(p->t().n_branches(),0)
{
    const auto& t = p->t();
    int B = t.n_branches();

    string prefix = "P"+convertToString(i+1)+".";
    string invisible_prefix = "*"+prefix;

    auto AAA = AA;
    minimally_connect_leaf_characters(AAA, t);
  
    // Create and set pairwise alignment parameters.
    for(int b=0;b<pairwise_alignment_for_branch.size();b++)
    {
	int n1 = t.source(b);
	int n2 = t.target(b);
	auto pi = A2::get_pairwise_alignment(AAA,n1,n2);
	pairwise_alignment_for_branch[b] = p->add_parameter(invisible_prefix+"a"+convertToString(b), pi);
    }

    // Create and set conditional likelihoods for each branch
    for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
	conditional_likelihoods_for_branch[b] = p->add_parameter(invisible_prefix+"CL"+convertToString(b), 0);

    //  const int n_states = state_letters().size();
    const int scale_index = p->scale_index_for_partition(i);
    const int smodel_index = p->smodel_index_for_partition(i);
    const int imodel_index = p->imodel_index_for_partition(i);
    const int n_base_smodels = p->evaluate(p->PC->SModels[smodel_index].n_base_models).as_int();

    // Add method indices for calculating transition matrices.
    auto transition_ps = p->get_expression(p->PC->branch_transition_p_indices(scale_index, smodel_index));
    {
	for(int b=0;b<B;b++)
	    transition_p_method_indices[b] = p->add_compute_expression( (identifier("!"), transition_ps, b) );
    }

    // Add method indices for calculating base models and frequencies
    base_model_indices.resize(n_base_smodels, B);
    {
	expression_ref BM = p->get_expression(p->PC->SModels[smodel_index].base_model);
	for(int m=0;m<n_base_smodels;m++)
	    for(int b=0;b<B;b++)
		base_model_indices(m,b) = p->add_compute_expression((BM,m,b));
    }

    // Add parameters for observed leaf sequence objects
    for(int i=0; i<leaf_sequence_indices.size(); i++)
	leaf_sequence_indices[i] = p->add_compute_expression(Vector<int>(sequences[i]));

    vector<expression_ref> seqs_;
    for(int index: leaf_sequence_indices)
	seqs_.push_back( p->get_expression(index) );
    auto seqs_array = p->get_expression( p->add_compute_expression((identifier("listArray'"),get_list(seqs_))) );
  
    // Add methods indices for sequence lengths
    vector<expression_ref> as_;
    for(int b=0;b<2*B;b++)
    {
	expression_ref a = parameter( p->parameter_name(pairwise_alignment_for_branch[b]) );
	as_.push_back(a);
    }
    expression_ref as = p->get_expression( p->add_compute_expression((identifier("listArray'"),get_list(as_))) );

    for(int n=0;n<t.n_nodes();n++)
    {
	auto L = (identifier("seqlength"), as, p->my_tree(), n);
	sequence_length_indices[n] = p->add_compute_expression( L );
    }

    if (p->t().n_nodes() == 1)
    {
	auto seq = (identifier("!"),seqs_array, 0);
	auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
	likelihood_index = p->add_compute_expression((identifier("peel_likelihood_1"), seq, *a, f));
    }
    else
    {
	auto t = p->my_tree();
	auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
	cl_index = p->add_compute_expression((identifier("cached_conditional_likelihoods"),t,seqs_array,as,*a,transition_ps,f));  // Create and set conditional likelihoods for each branch
	auto cls = p->get_expression(cl_index);
	for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
	    conditional_likelihoods_for_branch[b] = p->add_compute_expression((identifier("!"),cls,b));

	if (p->t().n_nodes() == 2)
	{
	    auto seq1 = (identifier("!"), seqs_array, 0);
	    auto seq2 = (identifier("!"), seqs_array, 1);
	    auto A = (identifier("!"), as, 0);
	    auto P = (identifier("!"), transition_ps, 0);
	    auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);

	    likelihood_index = p->add_compute_expression((identifier("peel_likelihood_2"), seq1, seq2, *a, A, P, f));
	}
	else
	{
	    auto root = parameter("*subst_root");
	    likelihood_index = p->add_compute_expression((identifier("peel_likelihood"), t, cls, as, f, root));
	}
    }

    // Add method indices for calculating branch HMMs and alignment prior
    if (imodel_index != -1)
    {
	// D = Params.substitutionBranchLengths!scale_index
	expression_ref D = (identifier("!"),identifier("Params.substitutionBranchLengths"),scale_index);
	expression_ref heat = parameter("Heat.beta");
	expression_ref training = parameter("*IModels.training");
	expression_ref model = (identifier("!"),identifier("IModels.models"),imodel_index);

	expression_ref hmms = (identifier("branch_hmms"), model, D, heat, training, B);
	hmms = p->get_expression( p->add_compute_expression(hmms) );

	// branch HMMs
	for(int b=0;b<B;b++)
	{
	    // (fst IModels.models!imodel_index) D b heat training
	    int index = p->add_compute_expression( (identifier("!"), hmms, b) );
	    branch_HMM_indices.push_back( index );
	}

	// Alignment prior
	if (p->t().n_nodes() == 1)
	{
	    auto seq = (identifier("!"),seqs_array, 0);
	    alignment_prior_index = p->add_compute_expression( (identifier("alignment_pr1"), seq, model) );
	}
	else
	    alignment_prior_index = p->add_compute_expression( (identifier("alignment_pr"), as, p->my_tree(), hmms, model) );
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
    weighted_frequency_matrix = C.add_compute_expression((identifier("weighted_frequency_matrix"), S));
    frequency_matrix = C.add_compute_expression((identifier("frequency_matrix"), S));
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
	vector<maybe_parameter> p_node;
	expression_ref node;
    
	if (T.node(n).is_leaf_node())
	{
	    if (edges.empty())
	    {
		node = List();
		p_node = { };  
	    }
	    else
	    {
		node = List(edges.front());
		p_node = { {-1,n} };
	    }
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
	maybe_parameter p_source = {-1,T.directed_branch(b).source().name()};
	if (not T.directed_branch(b).source().is_leaf_node())
	{
	    string name_source = "*MyTree.branch"+convertToString(b)+"source"; 
	    p_source = p->add_parameter(name_source,source);
	    source = parameter(name_source);
	}

	expression_ref target = T.directed_branch(b).target().name();
	maybe_parameter p_target = {-1, T.directed_branch(b).target().name()};
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
  
    // Add a *T<b> parameter for each branch b.
    {
	p->evaluate_expression( p->get_expression(tree_head) );
	expression_ref mus;
	if (p->branch_prior_type() == 0)
	    mus = model_expression({identifier("iid_branch_length_model_exp"), p->get_expression(tree_head)});
	else if (p->branch_prior_type() == 1)
	    mus = model_expression({identifier("iid_branch_length_model_gamma"), p->get_expression(tree_head)});
	p->evaluate_expression( perform_exp(mus) );
    }

    // Create the parameters that hold branch lengths
    for(int b=0;b<T.n_branches();b++)
    {
	string name = "*T"+convertToString(b+1);
	int index = p->find_parameter(name);
	branch_length_parameters.push_back(index);
	const context* c = p;
	const_cast<context*>(c)->set_parameter_value(index, T.branch(b).length());
    }
}

bool Parameters::variable_alignment() const
{
    return variable_alignment_;
}

void Parameters::variable_alignment(bool b)
{
    variable_alignment_ = b;

    // Ignore requests to turn on alignment variation when there is no imodel or internal nodes
    if (not n_imodels())
	variable_alignment_ = false;

    // turning ON alignment variation
    if (variable_alignment())
	assert(n_imodels() > 0);
}

data_partition Parameters::get_data_partition(int i) const
{
    return data_partition(this,i);
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

void Parameters::reconnect_branch(int s1, int t1, int t2)
{
    t().reconnect_branch(s1, t1, t2);
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
    reconnect_branch(s1,t1,t2);
    reconnect_branch(s2,t2,t1);
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

// We want to preserve connectedness, except across the branch n1-n4.
// Initially, the tree (0,2,3)5 is guaranteed to be connected.
// We ensure that ((0)4,2,3)5 is connected.
// However the branch (1,4) may be affected.
void disconnect_subtree(alignment& A, const vector<int>& nodes)
{
    for(int c=0; c<A.length(); c++)
    {
	bool n0 = A.character(c, nodes[0]);
	bool n5 = A.character(c, nodes[5]);

	if (n0 and n5)
	    A.set_value(c, nodes[4], alphabet::not_gap);

	if (not n0 and not n5)
	    A.set_value(c, nodes[4], alphabet::gap);
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

void disconnect_subtree(vector<HMM::bitmask_t>& a123456)
{
    for(auto& col:a123456)
    {
	if (col.test(0) and col.test(5) and not col.test(4))
	    col.set(4);

	if (not col.test(0) and not col.test(5) and col.test(4))
	    col.set(4,false);
    }
}

void Parameters::NNI(const tree_edge& B1, const tree_edge& B2, bool allow_disconnect_subtree)
{
    int b1 = t().find_branch(B1);
    int b2 = t().find_branch(B2);
    NNI(b1, b2, allow_disconnect_subtree);
}



// b1 and b2 point outwards, away from the other subtrees.
// The (possibly) disconnected subtree is the sibling of b1.
void Parameters::NNI(int b1, int b2, bool allow_disconnect_subtree)
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
	a123456[i] = A5::get_bitpath((*this)[i], order);

    // 3. Perform NNI
    exchange_subtrees(b1, b2);  // alter tree
    std::swap(nodes[0],nodes[2]); // alter nodes
    for(int i=0;i<n_data_partitions();i++)
	for(auto& col: a123456[i]) // alter matrix
	{
	    auto col2 = col;
	    col.set(0,col2.test(2));
	    col.set(2,col2.test(0));
	}

    // 4. Fix-up the alignment matrix (bits)
    for(int i=0;i<n_data_partitions();i++)
	if (get_data_partition(i).variable_alignment() and allow_disconnect_subtree)
	    disconnect_subtree(a123456[i]);
	else
	{
	    disconnect(a123456[i]);
	    minimally_connect(a123456[i]);
	}

    // 5. Set the pairwise alignments.
    for(int i=0;i<n_data_partitions();i++)
    {
	auto dp = get_data_partition(i);
	dp.set_pairwise_alignment(t().find_branch(nodes[0],nodes[4]), get_pairwise_alignment_from_bits(a123456[i], 0, 4));
	dp.set_pairwise_alignment(t().find_branch(nodes[1],nodes[4]), get_pairwise_alignment_from_bits(a123456[i], 1, 4));
	dp.set_pairwise_alignment(t().find_branch(nodes[2],nodes[5]), get_pairwise_alignment_from_bits(a123456[i], 2, 5));
	dp.set_pairwise_alignment(t().find_branch(nodes[3],nodes[5]), get_pairwise_alignment_from_bits(a123456[i], 3, 5));
	dp.set_pairwise_alignment(t().find_branch(nodes[4],nodes[5]), get_pairwise_alignment_from_bits(a123456[i], 4, 5));
    }
}

void Parameters::show_h_tree() const
{
    for(int b=0; b < 2*t().n_branches(); b++)
    {
	auto source = TC->parameters_for_tree_branch[b].first.get_value(this);
	auto target = TC->parameters_for_tree_branch[b].second.get_value(this);
	std::cerr<<"branch "<<b<<": ("<<source<<","<<target<<")     "<<t().branch_length(b)<<"\n";
    }
}

log_double_t Parameters::prior_no_alignment() const 
{
    log_double_t Pr = Model::prior();

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

void Parameters::select_root(int b) const
{
    if (t().source(b) == subst_root() or t().target(b) == subst_root())
	return;
  
    int r = t().reverse(b);
    if (t().subtree_contains(r, subst_root()))
	b = r;

    set_root(t().target(b));
}

void Parameters::set_root(int node) const
{
    assert(not t().is_leaf_node(node));
    const context* C = this;
    const_cast<context*>(C)->set_parameter_value(subst_root_index, node);
}

int Parameters::subst_root() const
{
    return get_parameter_value(subst_root_index).as_int();
}

void Parameters::recalc()
{
    auto tr = triggers();
    // Check for beta (0) or mu[i] (i+1)
    for(int index: tr)
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
	}
    }
    triggers().clear();
}

object_ptr<const alphabet> Parameters::get_alphabet_for_smodel(int s) const
{
    return evaluate(PC->SModels[s].get_alphabet).assert_is_a<alphabet>();
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
}

double Parameters::branch_mean() const 
{
    return 1.0/t().n_branches();
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
     variable_alignment_( n_imodels() > 0 ),
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

    subst_root_index = add_parameter("*subst_root", t().n_nodes()-1);

#ifndef NDEBUG
    evaluate_expression( (identifier("numNodes"), my_tree()));
    evaluate_expression( (identifier("numBranches"), my_tree()));
    evaluate_expression( (identifier("edgesOutOfNode"), my_tree(), 0));
    evaluate_expression( (identifier("neighbors"), my_tree(), 0));
    //  evaluate_expression( (identifier("nodesForEdge"),my_tree(), 0));
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
	    PC->branch_transition_p_indices(s,m) = add_compute_expression((identifier("transition_p_index"), my_tree(), S, branch_cat_list, DL));
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
	PC->DPC.emplace_back(this,i,A[i]);

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

