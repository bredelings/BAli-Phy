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

#include <map>
#include "util/assert.hh"

#include "models/parameters.H"
#include "util/rng.H"
#include "util/set.H"
#include "util/log-level.H"
#include "substitution/substitution.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "mcmc/proposals.H"
#include "probability/probability.H"
#include "computation/expression/lambda.H"
#include "computation/expression/bool.H"
#include "computation/expression/case.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/var.H"
#include "computation/expression/let.H"
#include "computation/expression/parameter.H"
#include "computation/module.H"
#include "computation/operations.H" // for VectorFromList<>
#include "math/exponential.H"
#include "models/setup.H"

using std::vector;
using std::string;
using std::pair;
using std::cerr;
using std::endl;
using std::ostream;
using std::map;

using std::optional;

/*
 * DONE:
 *
 * \todo: List of things to do to clean up programs.
 *
 * See list in models/parameters.C 
 *
 * 1. Make sure we don't read alignments with ^@ characters in the sequences! (?)
 *
 * 2. Avoid recomputing likelihoods when recomputed branch lengths are unchanged.
 *    - Cast to single precision, and then only recompute when that changes.
 * 3. Rewrite multi-case code to take patterns in terms of expression_ref's that might be seen from the parser.
 *     + Allows moving towards 16 incrementally.
 *
 * 6. Make Context load an entire program, instead of adding pieces incrementally.
 *
 * 7. Optimizations
 *     - Remove let bindings for unused variables?
 *     - Merge let bidings with identical bodies?
 *     - Simplify some case expressions based on knowledge of let-bound variable?
 *
 * 8. Print out simpler names than Test.i for parameter i.
 *     - I think parameters are in a separate namespace?
 *
 * 9. Add the ability to store newtype definitions.
 *
 * 10. Compare the monadic interface with Acar's interface.
 *
 * 11.
 *     - Handle sequences with lengths not divisible by 3.
 *     - Handle loading alignments with codons not together.
 *     - Could we actually handle all SEEN codon triplets?
 *
 * 12. Store alignments in a more sparse format?
 *
 * 13. Rename reg_heap -> something more descriptive/attractive.
 *
 * 14. Not printing RS07 model parameters??
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
    matrix<int> M;
    if (t().n_nodes() == 1)
    {
	M = matrix<int>(DPC().sequences[0].size(),1);
	for(int i=0;i<M.size1();i++)
	    M(i,0) = i;
    }
    else
    {
	vector<pairwise_alignment_t> As;
	for(int b=0;b<2*t().n_branches();b++)
	    As.push_back(get_pairwise_alignment(b));
	M = construct(t(), As);
    }

    return get_alignment(get_alphabet(), DPC().seqs, DPC().sequences, M);
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
    return bool(P->imodel_index_for_partition(partition_index));
}

const EVector& data_partition::get_sequence(int i) const
{
    return P->evaluate( DPC().leaf_sequence_indices[i] ).as_<EVector>();
}

const EVector& data_partition::transition_P(int b) const
{
    b = t().undirected(b);
    assert(b >= 0 and b < t().n_branches());

    return P->evaluate( DPC().transition_p_method_indices[b] ).as_<EVector>();
}

int data_partition::n_base_models() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].n_base_models).as_int();
}

int data_partition::n_states() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].n_states).as_int();
}

vector<double> data_partition::distribution() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return (vector<double>)P->evaluate(P->PC->SModels[s].distribution).as_<EVector>();
}

Matrix data_partition::WeightedFrequencyMatrix() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].weighted_frequency_matrix).as_<Box<Matrix>>();
}

Matrix data_partition::FrequencyMatrix() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].frequency_matrix).as_<Box<Matrix>>();
}

vector<unsigned> data_partition::state_letters() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].state_letters).as_<Vector<unsigned>>();
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

double data_partition::sequence_length_pr(int n) const
{
    return P->evaluate( DPC().sequence_length_pr_indices[n] ).as_log_double();
}

int data_partition::seqlength(int n) const
{
    if (n < DPC().sequences.size())
	return DPC().sequences[n].size();

    int l = P->evaluate(DPC().sequence_length_indices[n]).as_int();

    return l;
}

bool data_partition::pairwise_alignment_is_unset(int b) const
{
    auto E = get_pairwise_alignment_(b);
    return E.is_int();
}

void mutable_data_partition::unset_pairwise_alignment(int b)
{
    assert(not has_IModel() or likelihood_calculator() == 0);
    int B = t().reverse(b);
    assert(pairwise_alignment_is_unset(b) or (get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped()));

    const context* C = P;
    const_cast<context*>(C)->set_parameter_value(DPC().pairwise_alignment_for_branch[b], 0);
    const_cast<context*>(C)->set_parameter_value(DPC().pairwise_alignment_for_branch[B], 0);
    assert(pairwise_alignment_is_unset(b));
}

/// Set the pairwise alignment value, but don't mark the alignment & sequence lengths as changed.
void mutable_data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi)
{
    assert(not has_IModel() or likelihood_calculator() == 0);
    int B = t().reverse(b);
    assert(pairwise_alignment_is_unset(b) or (get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped()));
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
    assert(not has_IModel() or likelihood_calculator() == 0);
    return P->get_parameter_value(DPC().pairwise_alignment_for_branch[b]);
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b) const
{
    return get_pairwise_alignment_(b).as_<pairwise_alignment_t>();
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

int data_partition::likelihood_calculator() const
{
    return DPC().likelihood_calculator;
}

struct column_map
{
    optional<int> value;
    map<int, column_map> key_first;

    optional<int>& insert(const vector<int>& key, int index=0)
    {
	if (index >= key.size()) return value;
	int x = key[index];
	return key_first[x].insert(key, index+1);
    }
};
	
int find_add_column(column_map& M, const vector<int>& column, int next)
{
    auto& result = M.insert(column);
    if (not result)
	result = next;
    return *result;
}

int add_column(column_map& M, const vector<int>& column, vector<vector<int>>& cols, vector<int>& counts)
{
    assert(cols.size() == counts.size());
    int c = find_add_column(M, column, cols.size());
    if (c == cols.size())
    {
	cols.push_back(column);
	counts.push_back(1);
    }
    else
	counts[c]++;
    return c;
}

vector<int> site_pattern(const alignment& A, int n, int c)
{
    assert(n <= A.n_sequences());

    vector<int> pattern(n);
    for(int j=0;j<n;j++)
    {
	int x = A(c,j);
	if (x < 0) x = alphabet::gap;
	pattern[j] = x;
    }
    return pattern;
}

vector<vector<int>> compress_site_patterns(const alignment& A, int n, vector<int>& counts, vector<int>& mapping)
{
    column_map M;
    vector<vector<int>> columns;
    mapping.resize(A.length());
    for(int c=0;c<A.length();c++)
	mapping[c] = add_column(M, site_pattern(A,n,c), columns, counts);

    assert(counts.size() == columns.size());
    return columns;
}

alignment alignment_from_patterns(const alignment& old, const vector<vector<int>>& patterns, const TreeInterface& t)
{
    assert(old.n_sequences() <= t.n_nodes());
    assert(t.n_leaves() == patterns[0].size());
    assert(old.seqs().size() == t.n_nodes());

    alignment A(old.get_alphabet(), old.seqs(), patterns.size());

    for(int i=0;i<t.n_nodes();i++)
	if (i < t.n_leaves())
	    for(int c=0;c<A.length();c++)
		A.set_value(c,i,patterns[c][i]);
	else
	    for(int c=0;c<A.length();c++)
		A.set_value(c,i,alphabet::gap);

    minimally_connect_leaf_characters(A,t);
    return A;
}

alignment compress_alignment(const alignment& A, const TreeInterface& t, vector<int>& counts, vector<int>& mapping)
{
    auto patterns = compress_site_patterns(A, t.n_leaves(), counts, mapping);
    return alignment_from_patterns(A, patterns, t);
}


data_partition::data_partition(const Parameters* p, int i)
    :P(p),partition_index(i)
{ }

mutable_data_partition::mutable_data_partition(const Parameters* p, int i)
    :data_partition(p,i)
{ }

data_partition_constants::data_partition_constants(Parameters* p, int i, const alignment& AA, const vector<int>& counts, int like_calc)
    :pairwise_alignment_for_branch(2*p->t().n_branches()),
     conditional_likelihoods_for_branch(2*p->t().n_branches()),
     leaf_sequence_indices(p->t().n_leaves(),-1),
     sequence_length_indices(AA.n_sequences(),-1),
     sequence_length_pr_indices(AA.n_sequences(),-1),
     transition_p_method_indices(p->t().n_branches(),-1),
     seqs(AA.seqs()),
     sequences( alignment_letters(AA, p->t().n_leaves()) ),
     a(AA.get_alphabet().clone()),
     branch_HMM_type(p->t().n_branches(),0),
     likelihood_calculator(like_calc)
{
    const auto& t = p->t();
    int B = t.n_branches();

    string prefix = "P"+convertToString(i+1)+".";
    string invisible_prefix = "*"+prefix;

    // Create pairwise alignment parameters.
    for(int b=0;b<pairwise_alignment_for_branch.size();b++)
    {
	int n1 = t.source(b);
	int n2 = t.target(b);

	// No evaluation is performed if this is a leaf node.
	int L1 = t.is_leaf_node(n1) ? sequences[n1].size() : 0;
	int L2 = t.is_leaf_node(n2) ? sequences[n2].size() : 0;

	auto pi = make_unaligned_pairwise_alignment(L1,L2);
	// Ensure that for the 2-sequence case, the two directions agree on the alignment.
	if (b > t.reverse(b))
	    pi = make_unaligned_pairwise_alignment(L2,L1).flipped();

	pairwise_alignment_for_branch[b] = p->add_modifiable_parameter_with_value(invisible_prefix+"a"+convertToString(b), pi );
    }

    // Create and set conditional likelihoods for each branch
    for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
	conditional_likelihoods_for_branch[b] = p->add_modifiable_parameter_with_value(invisible_prefix+"CL"+convertToString(b), 0);

    //  const int n_states = state_letters().size();
    int scale_index = *p->scale_index_for_partition(i);
    int smodel_index = *p->smodel_index_for_partition(i);
    auto imodel_index = p->imodel_index_for_partition(i);

    // Add method indices for calculating transition matrices.
    auto transition_ps = p->get_expression(p->PC->branch_transition_p_indices(scale_index, smodel_index));
    {
	for(int b=0;b<B;b++)
	    transition_p_method_indices[b] = p->add_compute_expression( {var("Prelude.!"), transition_ps, b} );
    }

    // Add expressions for leaf sequences
    for(int i=0; i<leaf_sequence_indices.size(); i++)
	leaf_sequence_indices[i] = p->add_compute_expression(EVector(sequences[i]));

    // Add array of leaf sequences
    EVector seqs_;
    for(int index: leaf_sequence_indices)
    {
	seqs_.push_back( p->get_expression(index) );
    }
    auto seqs_array = p->get_expression( p->add_compute_expression({var("Prelude.listArray'"),get_list(seqs_)}) );

    // Add array of pairwise alignments
    EVector as_;
    for(int b=0;b<2*B;b++)
    {
	expression_ref a = parameter( p->parameter_name(pairwise_alignment_for_branch[b]) );
	as_.push_back(a);
    }
    expression_ref as = p->get_expression( p->add_compute_expression({var("Prelude.listArray'"),get_list(as_)}) );

    // Precompute sequence lengths
    for(int n=0;n<t.n_nodes();n++)
    {
	expression_ref L = {var("Alignment.seqlength"), as, p->my_tree(), n};
	sequence_length_indices[n] = p->add_compute_expression( L );
    }

    if (p->t().n_nodes() == 1)
    {
	expression_ref seq = {var("Prelude.!"),seqs_array, 0};
	auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
	likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_1"), seq, *a, f});
    }
    else if (likelihood_calculator == 0)
    {
	vector<vector<int>> seq_counts = alignment_letters_counts(AA, t.n_leaves(), counts);
	EVector counts_;
	for(int i=0; i<leaf_sequence_indices.size(); i++)
	    counts_.push_back( EVector(seq_counts[i]) );
	auto counts_array = p->get_expression( p->add_compute_expression({var("Prelude.listArray'"),get_list(counts_)}) );

	auto t = p->my_tree();
	auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
	cl_index = p->add_compute_expression({var("SModel.Likelihood.cached_conditional_likelihoods"),t,seqs_array,counts_array,as,*a,transition_ps,f});  // Create and set conditional likelihoods for each branch
	auto cls = p->get_expression(cl_index);
	for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
	    conditional_likelihoods_for_branch[b] = p->add_compute_expression({var("Prelude.!"),cls,b});

	// FIXME: broken for fixed alignments of 2 sequences.
	if (p->t().n_nodes() == 2)
	{
	    expression_ref seq1 = {var("Prelude.!"), seqs_array, 0};
	    expression_ref seq2 = {var("Prelude.!"), seqs_array, 1};
	    expression_ref A = {var("Prelude.!"), as, 0};
	    expression_ref P = {var("Prelude.!"), transition_ps, 0};
	    expression_ref f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);

	    likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_2"), seq1, seq2, *a, A, P, f});
	}
	else
	{
	    auto root = parameter("*subst_root");
	    likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood"), t, cls, as, f, root});
	}
    }
    else if (likelihood_calculator == 1)
    {
	auto t = p->my_tree();
	auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
	cl_index = p->add_compute_expression({var("SModel.Likelihood.cached_conditional_likelihoods_SEV"),t,seqs_array,*a,transition_ps,f,AA});  // Create and set conditional likelihoods for each branch
	auto cls = p->get_expression(cl_index);
	for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
	    conditional_likelihoods_for_branch[b] = p->add_compute_expression({var("Prelude.!"),cls,b});

	object_ptr<EVector> Counts(new EVector(counts));

	// FIXME: broken for fixed alignments of 2 sequences.
	if (p->t().n_nodes() == 2)
	{
	    expression_ref seq1 = {var("Prelude.!"), seqs_array, 0};
	    expression_ref seq2 = {var("Prelude.!"), seqs_array, 1};
	    expression_ref A = {var("Prelude.!"), as, 0};
	    expression_ref P = {var("Prelude.!"), transition_ps, 0};
	    expression_ref f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);

	    likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_2"), seq1, seq2, *a, A, P, f});
	}
	else
	{
	    auto root = parameter("*subst_root");
	    likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_SEV"), t, cls, f, root, Counts});
	}
    }

    // Add method indices for calculating branch HMMs and alignment prior
    if (imodel_index)
    {
	// D = Params.substitutionBranchLengths!scale_index
	expression_ref D = {var("Prelude.!"), p->my_substitution_branch_lengths(), scale_index};
	expression_ref heat = parameter("Heat.beta");
	expression_ref training = parameter("*IModels.training");
	expression_ref model = {{var("Prelude.!"),p->my_imodels(), *imodel_index}, heat, training};

	expression_ref hmms = {var("Alignment.branch_hmms"), model, D, B};
	hmms = p->get_expression( p->add_compute_expression(hmms) );

	for(int n=0;n<sequence_length_pr_indices.size();n++)
	{
	    expression_ref l = p->get_expression(sequence_length_indices[n]);
	    expression_ref lengthp = {snd,model};
	    sequence_length_pr_indices[n] = p->add_compute_expression( {lengthp,l} );
	}

        // branch HMMs
	for(int b=0;b<B;b++)
	{
	    // (fst IModels.models!imodel_index) D b heat training
	    int index = p->add_compute_expression( {var("Prelude.!"), hmms, b} );
	    branch_HMM_indices.push_back( index );
	}

	// Alignment prior
	if (p->t().n_nodes() == 1)
	{
	    expression_ref seq = {var("Prelude.!"),seqs_array, 0};
	    alignment_prior_index = p->add_compute_expression( {var("Alignment.alignment_pr1"), seq, model} );
	}
	else
	    alignment_prior_index = p->add_compute_expression( {var("Alignment.alignment_pr"), as, p->my_tree(), hmms, model} );


	expression_ref alignment_pdf = p->get_expression(alignment_prior_index);
	alignment_pdf = make_if_expression(parameter("*variable_alignment"), alignment_pdf, log_double_t(1.0));

	expression_ref sample_alignments = {var("Parameters.random_variable"), as, alignment_pdf, 0, 0.0};
	int alignment_sample_index = p->add_compute_expression( sample_alignments );
	p->evaluate( alignment_sample_index );
    }

    p->add_likelihood_factor(p->get_expression(likelihood_index));
}

//-----------------------------------------------------------------------------//
smodel_methods::smodel_methods(const expression_ref& E, context& C)
{
    expression_ref V = var("Prelude.list_to_vector");

    main = C.add_compute_expression( E );
    expression_ref S = C.get_expression(main);

    n_base_models = C.add_compute_expression({var("SModel.nBaseModels"), S});
    n_states =  C.add_compute_expression({var("SModel.nStates"), S});
    distribution =  C.add_compute_expression({V,{var("SModel.distribution"), S}});
    weighted_frequency_matrix = C.add_compute_expression({var("SModel.weighted_frequency_matrix"), S});
    frequency_matrix = C.add_compute_expression({var("SModel.frequency_matrix"), S});
    get_alphabet = C.add_compute_expression({var("SModel.getAlphabet"), S});
    state_letters = C.add_compute_expression({var("SModel.stateLetters"), S});
    n_states = C.add_compute_expression({var("SModel.nStates"), S});
    rate = C.add_compute_expression({var("SModel.rate"), S});

    frequencies = C.add_compute_expression({var("SModel.componentFrequencies"), S});
    transition_p = C.add_compute_expression({var("SModel.branchTransitionP"), S});
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

void tree_constants::register_branch_lengths(Parameters* p, const expression_ref& branch_lengths)
{
    int B = parameters_for_tree_branch.size()/2;
    if (B == 0) return;

    int branch_lengths_index = p->add_compute_expression( branch_lengths );
    p->evaluate(branch_lengths_index);
    auto branch_durations = p->get_expression(branch_lengths_index);

    // Create the parameters that hold branch lengths
    for(int b=0;b<B;b++)
    {
	int index = p->add_compute_expression( {var("Prelude.!"), branch_durations, b} );
	auto R = p->compute_expression_is_modifiable_reg(index);

	branch_duration_index.push_back(index);
	branch_duration_regs.push_back(R);
    }
}


tree_constants::tree_constants(Parameters* p, const SequenceTree& T, const model_t& branch_length_model)
    :n_leaves(T.n_leaves()),
     node_labels(T.get_labels())
{
    /*------------------------- Create the tree structure -----------------------*/
    vector<expression_ref> node_branches;
    for(int n=0; n < T.n_nodes(); n++)
    {
	auto edges = edges_connecting_to_node(T,n);
	vector<maybe_parameter> p_node;
	vector<expression_ref> node;
    
	if (T.node(n).is_leaf_node())
	{
	    if (edges.empty())
	    {
		p_node = { };  
	    }
	    else
	    {
		node.push_back(edges.front());
		p_node = { {-1,n} };
	    }
	}
	else
	{
	    for(int i=0;i<edges.size();i++)
	    {
		const auto& edge = edges[i];
		string name = "*MyTree.nodeBranches"+convertToString(n) + "." + convertToString(i);
		p_node.push_back( p->add_modifiable_parameter_with_value(name,edge) );
		node.push_back( parameter(name) );
	    }
	}
    
	parameters_for_tree_node.push_back ( p_node );
	node_branches.push_back( get_list(node) );
    }
    expression_ref node_branches_array = {var("Prelude.listArray'"),get_list(node_branches)};

    vector<expression_ref> branch_nodes;
    for(int b=0; b < 2*T.n_branches(); b++)
    {
	expression_ref source = T.directed_branch(b).source().name();
	maybe_parameter p_source = {-1,T.directed_branch(b).source().name()};
	expression_ref source_index = 0;
	maybe_parameter p_source_index = {-1,0};
	if (not T.directed_branch(b).source().is_leaf_node())
	{
	    string name_source = "*MyTree.branch"+convertToString(b)+"source"; 
	    p_source = p->add_modifiable_parameter_with_value(name_source, source);
	    source = parameter(name_source);

	    string name_source_index = "*MyTree.branch"+convertToString(b)+"source_index"; 
	    p_source_index = p->add_modifiable_parameter_with_value(name_source_index, source_index);
	    source_index = parameter(name_source_index);
	}

	expression_ref target = T.directed_branch(b).target().name();
	maybe_parameter p_target = {-1, T.directed_branch(b).target().name()};
	if (not T.directed_branch(b).target().is_leaf_node())
	{
	    string name_target = "*MyTree.branch"+convertToString(b)+"target"; 
	    p_target = p->add_modifiable_parameter_with_value(name_target,target);
	    target = parameter(name_target);
	}

	int reverse_branch = T.directed_branch(b).reverse();
	parameters_for_tree_branch.push_back( std::tuple<maybe_parameter, maybe_parameter, maybe_parameter>{p_source, p_source_index, p_target} );
	branch_nodes.push_back( Tuple(source, source_index, target, reverse_branch) );
    }
    expression_ref branch_nodes_array = {var("Prelude.listArray'"),get_list(branch_nodes)};

    expression_ref tree_con = lambda_expression( constructor("Tree.Tree",4) );

    tree_head = p->add_compute_expression( {tree_con, node_branches_array, branch_nodes_array, T.n_nodes(), T.n_branches()});
    auto tree = p->get_expression(tree_head);
  
    if (T.n_branches() > 0)
    {
	string prefix = "T:lengths";
	expression_ref branch_lengths = {branch_length_model.expression, tree};
	branch_lengths = {var("Distributions.gen_model_no_alphabet"), branch_lengths};
	branch_lengths = {var("Distributions.do_log"), prefix, branch_lengths};
	branch_lengths = {var("Prelude.unsafePerformIO"),branch_lengths};
	branch_lengths = {var("Parameters.evaluate"),-1,branch_lengths};
	branch_lengths = {var("Prelude.listArray'"),branch_lengths };
	register_branch_lengths(p, branch_lengths);
    }
}

bool Parameters::variable_alignment_from_param() const
{
    auto e = get_parameter_value(variable_alignment_param);
    return is_bool_true(e);
}

bool Parameters::variable_alignment() const
{
    assert(variable_alignment_from_param() == variable_alignment_);
    return variable_alignment_;
}

void Parameters::variable_alignment(bool b)
{
    variable_alignment_ = b;

    // Ignore requests to turn on alignment variation when there is no imodel or internal nodes
    if (not n_imodels())
	variable_alignment_ = false;

    if (variable_alignment_from_param() != variable_alignment_)
	set_parameter_value(variable_alignment_param, variable_alignment_);

    // turning ON alignment variation
    if (variable_alignment())
	assert(n_imodels() > 0);
}

data_partition Parameters::get_data_partition(int i) const
{
    return data_partition(this,i);
}

mutable_data_partition Parameters::get_data_partition(int i)
{
    return mutable_data_partition(this,i);
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

string write_newick(const Parameters& P, bool print_lengths)
{
    return write(P.t(), P.get_labels(), print_lengths);
}

void Parameters::prune_subtree(const tree_edge& b_subtree)
{
    // FIXME -- this seems redundant.
    vector<int> connected = t().branches_after(t().find_branch(b_subtree));
    double L = t().branch_length(connected[0]) + t().branch_length(connected[1]);

    if (connected.empty()) throw myexception()<<"We can't prune subtrees that point to leaves!";

    //  int x = b_subtree.node1;
    int y = b_subtree.node2;
    int a = t().target(connected[0]);
    int b = t().target(connected[1]);
    // Avoid changing the orientation of leaf branches.
    if (t().is_leaf_node(b)) std::swap(a,b);
    assert(not t().is_leaf_node(b));

    begin_modify_topology();
    reconnect_branch(a,y,b);
    reconnect_branch(y,b,y);
    end_modify_topology();

    t().set_branch_length(t().find_branch(a,b), L);
    t().set_branch_length(t().find_branch(y,y), 0.0);
}

void Parameters::regraft_subtree(const tree_edge& b_subtree, const tree_edge& b_target)
{
    //  int x = b_subtree.node1;
    int y = b_subtree.node2;

    int a = b_target.node1;
    int b = b_target.node2;
    // Avoid changing the orientation of leaf  branches.
    if (t().is_leaf_node(b)) std::swap(a,b);
    assert(not t().is_leaf_node(b));

    begin_modify_topology();
    reconnect_branch(y,y,b);
    reconnect_branch(a,b,y);
    end_modify_topology();
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

void mutable_data_partition::set_alignment(const alignment& A)
{
    // 1. Check if the alphabet on the alignment is right.
    if (get_alphabet() != A.get_alphabet())
	throw myexception()<<"Can't set alignment with alphabet '"<<A.get_alphabet()<<"' in partition with alphabet '"<<get_alphabet()<<"'";

    // 2. Connect the leaf characters
    auto T = t();
    auto AA = A;
    minimally_connect_leaf_characters(AA, T);

    // 3. Set pairwise alignment parameters.
    for(int b=0;b<T.n_branches();b++)
    {
	int n1 = T.source(b);
	int n2 = T.target(b);
	auto pi = A2::get_pairwise_alignment(AA,n1,n2);
	set_pairwise_alignment(b, pi);
    }
}

void mutable_data_partition::unalign_sequences()
{
    auto T = t();
    for(int b = 0; b<T.n_branches(); b++)
    {
	int n1 = T.source(b);
	int n2 = T.target(b);

	// No evaluation is performed if this is a leaf node.
	int L1 = T.is_leaf_node(n1) ? seqlength(n1) : 0;
	int L2 = T.is_leaf_node(n2) ? seqlength(n2) : 0;

	set_pairwise_alignment(b, make_unaligned_pairwise_alignment(L1,L2) );
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
    using std::get;
    for(int b=0; b < 2*t().n_branches(); b++)
    {
	auto source = get<0>(TC->parameters_for_tree_branch[b]).get_value(this);
	auto target = get<2>(TC->parameters_for_tree_branch[b]).get_value(this);
	std::cerr<<"branch "<<b<<": ("<<source<<","<<target<<")     "<<t().branch_length(b)<<"\n";
    }
}

log_double_t Parameters::prior_no_alignment() const 
{
    if (variable_alignment_)
    {
	auto P2 = *this;
	P2.variable_alignment(false);
	return P2.prior();
    }
    else
	return prior();
}

log_double_t Parameters::prior_alignment() const 
{
    log_double_t Pr = 1;

    for(int i=0;i<n_data_partitions();i++)
	Pr *= get_data_partition(i).prior_alignment();

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

double Parameters::branch_scale(int s) const
{
    return evaluate(branch_scale_index(s)).as_double();
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
}

double Parameters::branch_mean() const 
{
    return 1.0/t().n_branches();
}


int Parameters::branch_scale_index(int i) const 
{
    assert(0 <= i and i < n_branch_scales());

    return PC->scale_parameter_indices[i];
}

optional<int> Parameters::branch_scale_modifiable_reg(int s) const
{
    return compute_expression_is_modifiable_reg(branch_scale_index(s));
}

void Parameters::branch_scale(int s, double x)
{
    if (auto R = branch_scale_modifiable_reg(s))
	return set_modifiable_value(*R, x);
    else
	throw myexception()<<"Branch scale "<<s+1<<" is not directly modifiable!";
}

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
    return branch_scale( *scale_index_for_partition(p) );
}

expression_ref Parameters::my_tree() const
{
    return get_expression(TC->tree_head);
}

expression_ref Parameters::my_imodels() const
{
    return get_expression(PC->imodels_index);
}

expression_ref Parameters::my_substitution_branch_lengths() const
{
    return get_expression(PC->substitution_branch_lengths_index);
}

int num_distinct(const vector<optional<int>>& v)
{
    int m = -1;
    for(auto& x: v)
	if (x)
	    m = std::max(m,*x);
    return m+1;
}

parameters_constants::parameters_constants(const vector<alignment>& A, const SequenceTree& t,
					   const vector<model_t>& SMs,
					   const vector<optional<int>>& s_mapping,
					   const vector<model_t>& /* IMs */,
					   const vector<optional<int>>& i_mapping,
					   const vector<optional<int>>& scale_mapping)
    :smodel_for_partition(s_mapping),
     imodel_for_partition(i_mapping),
     n_imodels(num_distinct(i_mapping)),
     scale_for_partition(scale_mapping),
     n_scales(num_distinct(scale_mapping)),
     scale_parameter_indices(n_scales),
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
	int m = *smodel_for_partition[i];
	if (m >= SMs.size())
	    throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
			       <<" because there are only "<<SMs.size()<<" smodels.";
    }
}

vector<int> Parameters::partitions_for_imodel(int i) const
{
    assert(0 <= i and i < n_imodels());
    vector<int> partitions;
    for(int p=0;p<n_data_partitions();p++)
	if (auto index = imodel_index_for_partition(p))
	    if (*index == i)
		partitions.push_back(p);
    return partitions;
}

vector<int> Parameters::partitions_for_scale(int i) const
{
    assert(0 <= i and i < n_branch_scales());
    vector<int> partitions;
    for(int p=0;p<n_data_partitions();p++)
	if (auto index = scale_index_for_partition(p))
	    if (*index == i)
		partitions.push_back(p);
    return partitions;
}

/* OK, so we should in theory be able to do two different things:
   * construct a giant model and then run gen_model_no_alphabet on it.
     - we would need to run set_alphabet in front of each of the substitution models
     - this would be in the model monad.
     - each model yields (I think) a pair of (value, loggers) when performed.
     - So, the code looks like

          (s1, s1_loggers) <- smodel1_action
          (s2, s2_loggers) <- smodel2_action
          (i1, i1_loggers) <- imodel1_action
          (scale1, scale1_loggers) <- scale1_action
	  (topology, topology_loggers) <- topology_action
          (a1, a1_loggers) <- alignment1_action
          (a2, a2_loggers) <- alignment2_action
          (a3, a3_loggers) <- alignment3_action
          (branch_lengths, branch_lengths_loggers) <- branch_lengths_action topology
	  observe dist1 partition1_data
	  observe dist2 partition2_data
	  observe dist3 partition3_data
	  let model = ATModel [s1,s2] [i1] [scale1,scale2] [a1,a2,a3] branch_lengths topology
	  let loggers = [("S1", (Nothing, s1_loggers)),
	                 ("S2", (Nothing, s2_loggers)),
                         ("I1", (Nothing, i1_loggers)),
                         ("T" , (Nothing, branch_length_loggers)),
			 ("Scale[1]", (Just scale1, scale1_loggers))]
*/

/*
  The other option is to construct a giant expression in the cmd-line language:
- let[S1=with_alphabet[alph1,smodel1],
  let[S2=with_alphabet[alph2,smodel2],
  let[I1=imodel1,
  let[Scale=List[scale1,scale2],
  let[T=branch_lengths,
  let[A1=a1,
  let[A2=a2,
  let[A3=a3,
  ATModel[smodels=[S1,S2],imodels=[I1],scales=Scale,alignments=[a1,a2,a3]]

This second option has the nice feature that it would automatically allow 
defining other variables as let-variables.  However, it would duplicately log
things.  We could probably add a flag to ATModel so that it doesn't log anything.

The other thing we could do is something like

ATModel[S=[with_alphabet[alph1,smodel1],with_alphabet[alph2,smodel2]],
        I=[imodel1],
        Scale=[scale1,scale2],
        A=[a1,a2,a3]
        topology=topology,
        T=branch_lengths]

We could then add a flag to avoid prefixing things with ATModel:

One thing that both approaches have in common is that they both require
an ATModel object that can be returned.

The main problems with this approach are:
- error messages. I think we could solve this by (i) parsing everything separately - given the lets.
- we may need to set initial values for the tree.  I think this is OK?
- we need to be able to generate alignments.
  + do we want to generate these conditional on the sequences?  That's what we are currently doing.
  + do we want to initially generate an empty alignment?  yeah.
  + alternatively, we could do a sample_with_initial_value.
 */

class do_block
{
    std::function<expression_ref(const expression_ref&)> code = [](const expression_ref& E) {return E;};
public:
    expression_ref get_expression() const {return code({});}

    do_block& perform(const expression_ref& E1)
    {
	auto new_code = [code=code,E1](const expression_ref& E2) 
			    {
				return code({var("Compiler.Base.>>"),E1,E2});
			    };
	code = new_code;
	return *this;
    }

    do_block& perform(const var& x, const expression_ref& E1)
    {
	auto new_code = [code=code,x,E1](const expression_ref& E2) 
			    {
				return code({var("Compiler.Base.>>="),E1,lambda_quantify(x,E2)});
			    };
	code = new_code;
	return *this;
    }

    do_block& let(const CDecls& decls)
    {
	auto new_code = [code=code,decls](const expression_ref& E)
			    {
				return code(let_expression(decls,E));
			    };
	code = new_code;
	return *this;
    }

    expression_ref finish(const expression_ref& E1)
    {
	auto new_code = [code=code,E1](const expression_ref&)
			    {
				return code(E1);
			    };
	code = new_code;
	return get_expression();
    }

    expression_ref finish_return(const expression_ref& E)
    {
	return finish({var("Compiler.Base.return"),E});
    }

    pair<expression_ref, expression_ref> bind_model(const std::string& prefix, const expression_ref& model)
    {
	var pair("pair_arg_" + prefix);
	var x("arg_" + prefix);
	var loggers("log_arg_" + prefix);
	perform(pair,model);               // pair <- smodel
	let({{x,{fst,pair}},               // let x     = fst pair
	     {loggers,{snd,pair}}});       //     loggers = snd smodel_pair
	return {x,loggers};
    }

    expression_ref bind_and_log_model(const string& prefix, const expression_ref& model, vector<expression_ref>& loggers)
    {
	auto [x, x_loggers] = bind_model(prefix,model);
	loggers.push_back( Tuple( prefix, Tuple( { var("Data.Maybe.Just"), x }, x_loggers) ) );
	return x;
    }
};

Parameters::Parameters(const std::shared_ptr<module_loader>& L,
		       const vector<alignment>& A, const SequenceTree& tt,
		       const vector<model_t>& SMs,
		       const vector<optional<int>>& s_mapping,
		       const vector<model_t>& IMs,
		       const vector<optional<int>>& i_mapping,
		       const vector<model_t>& scaleMs,
		       const vector<optional<int>>& scale_mapping,
		       const model_t& branch_length_model,
		       const std::vector<int>& like_calcs,
		       const key_map_t& k)
    :Model(L,k),
     PC(new parameters_constants(A,tt,SMs,s_mapping,IMs,i_mapping,scale_mapping)),
     variable_alignment_( n_imodels() > 0 ),
     updown(-1)
{
    // \todo FIXME:cleanup|fragile - Don't touch C here directly!
    *this += { "SModel","Distributions","Range","PopGen","Alignment","IModel","BAliPhy.ATModel" };
  
    // Don't call set_parameter_value here, because recalc( ) depends on branch_length_indices, which is not ready.

    PC->constants.push_back(-1);

    add_modifiable_parameter_with_value("Heat.beta", 1.0);
    variable_alignment_param = add_modifiable_parameter_with_value("*variable_alignment", variable_alignment_);

    /* ---------------- Set up the tree ------------------------------ */
    branches_from_affected_node.resize(tt.n_nodes());

    TC = new tree_constants(this, tt, branch_length_model);

    t().read_tree(tt);

    /* --------------------------------------------------------------- */
    vector<expression_ref> smodels;
    do_block program;
    // ATModel smodels imodels scales branch_lengths
    // Loggers = [(string,(Maybe a,Loggers)]
    vector<expression_ref> program_loggers;
    // Therefore, we are constructing a list with values [(prefix1,(Just value1, loggers1)), (prefix1, (Just value1, loggers2))

    // register the substitution models as sub-models
    vector<expression_ref> smodels_list;
    for(int i=0;i<SMs.size();i++)
    {
	string prefix = "S" + convertToString(i+1);

	optional<int> first_index;
	for(int j=0;j<s_mapping.size();j++)
	    if (s_mapping[j] and *s_mapping[j] == i)
		first_index = j;

	const alphabet& a = A[*first_index].get_alphabet();

	expression_ref smodel = SMs[i].expression;
	smodel = {var("Distributions.set_alphabet'"), a, smodel};

	auto smodel_var = program.bind_and_log_model(prefix , smodel, program_loggers);
	smodels_list.push_back(smodel_var);

	smodel = {var("Distributions.gen_model_no_alphabet"), smodel};
	smodel = {var("Distributions.do_log"), prefix, smodel};
	smodel = {var("Prelude.unsafePerformIO"),smodel};
	smodel = {var("Parameters.evaluate"),-1,smodel};
	smodels.push_back(smodel);
    }


    // register the indel models as sub-models
    vector<expression_ref> imodels;
    vector<expression_ref> imodels_list;
    for(int i=0;i<n_imodels();i++)
    {
	string prefix = "I" + convertToString(i+1);
	expression_ref imodel = IMs[i].expression;
	auto imodel_var = program.bind_and_log_model(prefix, imodel, program_loggers);

	imodel = {var("Distributions.gen_model_no_alphabet"), imodel};
	imodel = {var("Distributions.do_log"), prefix, imodel};
	imodel = {var("Prelude.unsafePerformIO"),imodel};
	imodel = {var("Parameters.evaluate"),-1,imodel};
	imodels.push_back({imodel,my_tree()});
	imodels_list.push_back({imodel_var,my_tree()});
    }

    // Add parameter for each scale
    vector<expression_ref> scales;
    vector<expression_ref> scales_list_;
    for(int i=0; i<n_branch_scales(); i++)
    {
	string prefix = "Scale"+convertToString(i+1);

	auto scale_model = scaleMs[i].expression;
	auto scale_var = program.bind_and_log_model(prefix , scale_model, program_loggers);
	scales_list_.push_back(scale_var);

	scale_model = {var("Distributions.gen_model_no_alphabet"), scale_model};
	scale_model = {var("Distributions.do_log"), prefix, scale_model};
	scale_model = {var("Prelude.unsafePerformIO"),scale_model};
	scale_model = {var("Parameters.evaluate"),-1,scale_model};
	int scale_index = add_compute_expression( scale_model );
	scales.push_back( get_expression(scale_index) );
    }


    expression_ref branch_lengths = {branch_length_model.expression, my_tree()};
    expression_ref branch_lengths_list;
    {
	string prefix = "T:lengths";
	auto [x,loggers] = program.bind_model(prefix , branch_lengths);
	branch_lengths_list = x;

	branch_lengths = {var("Distributions.gen_model_no_alphabet"), branch_lengths};
	branch_lengths = {var("Distributions.do_log"), prefix, branch_lengths};
	branch_lengths = {var("Prelude.unsafePerformIO"),branch_lengths};
	branch_lengths = {var("Parameters.evaluate"),-1,branch_lengths};
    }

    // We haven't done the observe's yet, though.
    expression_ref program_exp = program.finish_return(
	Tuple(
	    {var("BAliPhy.ATModel.ATModel"),get_list(smodels_list),get_list(imodels_list),get_list(scales_list_),branch_lengths_list},
	    get_list(program_loggers))
	);
    
    int atmodel_index = add_compute_expression({var("BAliPhy.ATModel.ATModel"),get_list(smodels),get_list(imodels),get_list(scales),branch_lengths});
    program_exp = {var("Distributions.gen_model_no_alphabet"), program_exp};
    program_exp = {var("Distributions.do_log"), "top", program_exp};
    program_exp = {var("Prelude.unsafePerformIO"),program_exp};
    program_exp = {var("Parameters.evaluate"),-1,program_exp};
    expression_ref atmodel = get_expression(atmodel_index);

    /* --------------------------------------------------------------- */

    // R1. Register branch lengths
    TC->register_branch_lengths(this, {var("Prelude.listArray'"),{var("BAliPhy.ATModel.branch_lengths"),atmodel}});

    int scales_list_index = add_compute_expression( {var("BAliPhy.ATModel.scales"),atmodel} );
    expression_ref scales_list = get_expression(scales_list_index);

    // L1. Log scales as list Scale
    add_parameter("Scale", scales_list); // Log the scales as a list.

    // R2. Register individual scales
    for(int i=0; i<n_branch_scales();i++)
	PC->scale_parameter_indices[i] = add_compute_expression( {var("Data.List.!!"),scales_list,i} );

    // P1. Add substitution root node
    subst_root_index = add_modifiable_parameter_with_value("*subst_root", t().n_nodes()-1);

#ifndef NDEBUG
    evaluate_expression( {var("Tree.numNodes"), my_tree()});
    evaluate_expression( {var("Tree.numBranches"), my_tree()});
    evaluate_expression( {var("Tree.edgesOutOfNode"), my_tree(), 0});
    evaluate_expression( {var("Tree.neighbors"), my_tree(), 0});

    for(int b=0; b < 2*tt.n_branches(); b++)
    {
	vector<const_branchview> branch_list;
	append(tt.directed_branch(b).branches_before(),branch_list);
	vector<int> branch_list_;
	for(auto b: branch_list)
	    branch_list_.push_back(b);

	auto b2 = (vector<int>) evaluate_expression( {var("Prelude.list_to_vector"),{var("Tree.edgesBeforeEdge"),my_tree(),b}}).as_<EVector>();
	assert(b2.size() == branch_list_.size());
	for( int i: branch_list_)
	    assert(includes(b2,i));
    }
#endif

    // R3. Register methods for each of the individual substitution models
    for(int i=0;i<SMs.size();i++)
    {
	expression_ref smodel = {var("Data.List.!!"),{var("BAliPhy.ATModel.smodels"),atmodel},i};

	PC->SModels.push_back( smodel_methods( smodel, *this) );
    }

    // P2. Add training parameter
    add_modifiable_parameter_with_value("*IModels.training", false);

    // R4. Register an array of indel models
    PC->imodels_index = add_compute_expression({var("Prelude.listArray'"), {var("BAliPhy.ATModel.imodels"),atmodel}});
  
    // don't constrain any branch lengths
    for(int b=0;b<PC->TC.n_branches();b++)
	PC->TC.branch(b).set_length(-1);

    // R5. Register D[b,s] = T[b]*scale[s]
    for(int s=0;s<n_branch_scales();s++)
    {
	expression_ref scale = get_expression(branch_scale_index(s));
	PC->branch_length_indices.push_back(vector<int>());
	for(int b=0;b<t().n_branches();b++)
	{
	    expression_ref length = get_expression(TC->branch_duration_index[b]);
	    int index = add_compute_expression( {var("Compiler.Num.*"),scale,length} );
	    PC->branch_length_indices[s].push_back(index);
	}
    }

    // R6. Register branch categories
    vector<expression_ref> branch_categories;
    for(int b=0;b<t().n_branches();b++)
    {
	string name = "*Main.branchCat" + convertToString(b+1);
	add_modifiable_parameter_with_value(name, 0);
	branch_categories.push_back(parameter(name));
    }
    expression_ref branch_cat_list = get_expression( add_compute_expression( (get_list(branch_categories) ) ) );

    // R7. Register array of arrays to lookup D[s][b]
    expression_ref substitutionBranchLengthsList;
    {
	vector<expression_ref> SBLL;
	for(int s=0;s < n_branch_scales(); s++)
	{
	    vector<expression_ref> D;
	    for(int b=0;b<t().n_branches();b++)
		D.push_back( get_expression(PC->branch_length_indices[s][b]) );
	    SBLL.push_back(get_list(D));
	}
	substitutionBranchLengthsList = get_list(SBLL);
    }

    PC->substitution_branch_lengths_index = add_compute_expression({var("Prelude.listArray'"),{var("Prelude.fmap"),var("Prelude.listArray'"),substitutionBranchLengthsList}});

    // R8. register the cached transition_p indices
    PC->branch_transition_p_indices.resize(n_branch_scales(), n_smodels());
    for(int s=0;s < n_branch_scales(); s++)
    {
	// Better yet, make a substitutionBranchLengths!scale!branch that can be referenced elsewhere.
	expression_ref DL = {var("Prelude.!"), my_substitution_branch_lengths(), s};

	// Here, for each (scale,model) pair we're construction a function from branches -> Vector<transition matrix>
	for(int m=0;m < n_smodels(); m++)
	{
	    expression_ref S = get_expression(PC->SModels[m].main);
	    PC->branch_transition_p_indices(s,m) = add_compute_expression({var("SModel.transition_p_index"), my_tree(), S, branch_cat_list, DL});
	}
    }

    // create data partitions

    // FIXME! Make likelihood_calculators for 1- and 2-sequence alignments handle compressed alignments.
    bool allow_compression = load_value("site-compression", t().n_nodes() > 2);

    assert(like_calcs.size() == A.size());
    for(int i=0;i<A.size();i++)
    {
	if (not imodel_index_for_partition(i) and allow_compression)
	{
	    // construct compressed alignment, counts, and mapping
	    vector<int> counts;
	    vector<int> mapping;
	    auto AA = compress_alignment(A[i], t(), counts, mapping);
	    std::cerr<<"Partition #"<<i+1<<": "<<A[i].length()<<" columns -> "<<AA.length()<<" unique patterns.\n";

	    PC->DPC.emplace_back(this, i, AA, counts, like_calcs[i]);
	    get_data_partition(i).set_alignment(AA);
	}
	else
	{
	    auto counts = vector<int>(A[i].length(), 1);
	    PC->DPC.emplace_back(this, i, A[i], counts, like_calcs[i]);
	    if (not imodel_index_for_partition(i))
		get_data_partition(i).set_alignment(A[i]);
	}
    }

    // FIXME: We currently need this to make sure all parameters get instantiated before we finish the constructor.
    probability();

    bool some_branch_lengths_not_set = false;
    for(int b=0;b<tt.n_branches();b++)
	if (tt.branch(b).has_length())
	{
	    if (t().can_set_branch_length(b))
		t().set_branch_length(b, tt.branch(b).length());
	    else
		some_branch_lengths_not_set = true;
	}
    if (some_branch_lengths_not_set)
	std::cerr<<"Warning!  Some branch lengths not set because they are not directly modifiable.\n\n";
}

Parameters::Parameters(const std::shared_ptr<module_loader>& L,
		       const vector<alignment>& A, const SequenceTree& t,
		       const vector<model_t>& SMs,
		       const vector<optional<int>>& s_mapping,
		       const vector<model_t>& scaleMs,
		       const vector<optional<int>>& scale_mapping,
		       const model_t& branch_length_model,
		       const std::vector<int>& like_calcs,
		       const key_map_t& k)
    :Parameters(L, A, t, SMs, s_mapping, vector<model_t>{}, vector<optional<int>>{}, scaleMs, scale_mapping, branch_length_model, like_calcs, k)
{ }

// FIXME - move to .. model.cc? mcmc?
bool accept_MH(const Model& P1,const Model& P2,log_double_t rho)
{
    if (log_verbose >= 3)
    {
	std::cerr<<"accept_MH: rho = "<<rho<<endl;

	show_parameters(std::cerr,P1);
	std::cerr<<P1.probability()<<" = "<<P1.likelihood()<<" + "<<P1.prior()<<endl;
	std::cerr<<endl;

	show_parameters(std::cerr,P2);
	std::cerr<<P2.probability()<<" = "<<P2.likelihood()<<" + "<<P2.prior();
	std::cerr<<endl<<endl;
    }

    log_double_t ratio = rho*P2.heated_probability_ratio(P1);

    bool accept = (ratio >= 1.0 or uniform() < ratio);

    if (log_verbose >=3) std::cerr<<"accept_MH: accept = "<<accept<<endl;

    return accept;
}

