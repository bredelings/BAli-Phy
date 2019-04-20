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
#include "computation/expression/reg_var.H"
#include "computation/expression/let.H"
#include "computation/expression/parameter.H"
#include "computation/module.H"
#include "computation/operations.H" // for VectorFromList<>
#include "math/exponential.H"
#include "models/setup.H"
#include "site-compression.H"

using std::vector;
using std::string;
using std::pair;
using std::cerr;
using std::endl;
using std::ostream;
using std::map;
using std::tuple;

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
    return DPC().leaf_sequence_indices[i].get_value(*P).as_<EVector>();
}

const EVector& data_partition::transition_P(int b) const
{
    b = t().undirected(b);
    assert(b >= 0 and b < t().n_branches());

    return DPC().transition_p_method_indices[b].get_value(*P).as_<EVector>();
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

    return DPC().branch_HMM_indices[b].get_value(*P).as_<indel::PairHMM>();
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
    return DPC().sequence_length_pr_indices[n].get_value(*P).as_log_double();
}

int data_partition::seqlength(int n) const
{
    if (n < DPC().sequences.size())
        return DPC().sequences[n].size();

    int l = DPC().sequence_length_indices[n].get_value(*P).as_int();

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
    DPC().pairwise_alignment_for_branch[b].set_value(*const_cast<context*>(C),0);
    DPC().pairwise_alignment_for_branch[B].set_value(*const_cast<context*>(C),0);
    assert(pairwise_alignment_is_unset(b));
}

/// Set the pairwise alignment value, but don't mark the alignment & sequence lengths as changed.
void mutable_data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi)
{
    assert(not has_IModel() or likelihood_calculator() == 0);
    int B = t().reverse(b);
    assert(pairwise_alignment_is_unset(b) or (get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped()));
    const context* C = P;
    DPC().pairwise_alignment_for_branch[b].set_value(*const_cast<context*>(C), new Box<pairwise_alignment_t>(pi));
    DPC().pairwise_alignment_for_branch[B].set_value(*const_cast<context*>(C), new Box<pairwise_alignment_t>(pi.flipped()));
    assert(get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped());
}

const matrix<int>& data_partition::alignment_constraint() const
{
    return DPC().alignment_constraint;
}

expression_ref data_partition::get_pairwise_alignment_(int b) const
{
    assert(not has_IModel() or likelihood_calculator() == 0);
    return DPC().pairwise_alignment_for_branch[b].get_value(*P);
}

const pairwise_alignment_t& data_partition::get_pairwise_alignment(int b) const
{
    return get_pairwise_alignment_(b).as_<Box<pairwise_alignment_t>>();
}

// We want to decrease 
// (a) the number of times get_counts( ) is called
// (b) the number of times seqlength( ) is called
// (c) the number of times log( ) is called
// This should give a further 6% speedup.

log_double_t data_partition::prior_alignment() const 
{
    if (not variable_alignment()) return 1;

    log_double_t Pr = DPC().alignment_prior_index.get_value(*P).as_log_double();

    return Pr;
}

const Likelihood_Cache_Branch& data_partition::cache(int b) const
{
    return P->evaluate(DPC().conditional_likelihoods_for_branch[b]).as_<Likelihood_Cache_Branch>();
}

log_double_t data_partition::likelihood() const 
{
    substitution::total_likelihood++;
    return DPC().likelihood_index.get_value(*P).as_log_double();
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

data_partition::data_partition(const Parameters* p, int i)
    :P(p),partition_index(i)
{ }

mutable_data_partition::mutable_data_partition(const Parameters* p, int i)
    :data_partition(p,i)
{ }

    // P1. Create pairwise alignment parameters.
EVector unaligned_alignments_on_tree(const TreeInterface& t, const vector<vector<int>>& sequences)
{
    int B = t.n_branches();
    EVector alignments;

    for(int b=0;b<2*B;b++)
    {
        int n1 = t.source(b);
        int n2 = t.target(b);

        // No evaluation is performed if this is a leaf node.
        int L1 = t.is_leaf_node(n1) ? sequences[n1].size() : 0;
        int L2 = t.is_leaf_node(n2) ? sequences[n2].size() : 0;

        Box<pairwise_alignment_t> pi = make_unaligned_pairwise_alignment(L1,L2);

        // Ensure that for the 2-sequence case, the two directions agree on the alignment.
        if (b > t.reverse(b))
            pi = make_unaligned_pairwise_alignment(L2,L1).flipped();

        alignments.push_back(pi);
    }
    return alignments;
}

    // P1. Create pairwise alignment parameters.
EVector unaligned_alignments_on_tree(const Tree& t, const vector<vector<int>>& sequences)
{
    int B = t.n_branches();
    EVector alignments;

    for(int b=0;b<2*B;b++)
    {
        int n1 = t.source(b);
        int n2 = t.target(b);

        // No evaluation is performed if this is a leaf node.
        int L1 = t.is_leaf_node(n1) ? sequences[n1].size() : 0;
        int L2 = t.is_leaf_node(n2) ? sequences[n2].size() : 0;

        Box<pairwise_alignment_t> pi = make_unaligned_pairwise_alignment(L1,L2);

        // Ensure that for the 2-sequence case, the two directions agree on the alignment.
        if (b > t.reverse(b))
            pi = make_unaligned_pairwise_alignment(L2,L1).flipped();

        alignments.push_back(pi);
    }
    return alignments;
}

data_partition_constants::data_partition_constants(Parameters* p, int i, const alignment& AA, const vector<int>& counts, int like_calc)
    :conditional_likelihoods_for_branch(2*p->t().n_branches()),
     sequence_length_indices(AA.n_sequences()),
     sequence_length_pr_indices(AA.n_sequences()),
     seqs(AA.seqs()),
     sequences( alignment_letters(AA, p->t().n_leaves()) ),
     a(AA.get_alphabet().clone()),
     branch_HMM_type(p->t().n_branches(),0),
     likelihood_calculator(like_calc)
{
    const auto& t = p->t();
    int B = t.n_branches();

    // Can we compute the pairwise alignment in such a way that recomputing the alignments when
    // the tree changes has the same cost as modifying the solution and setting the alignment to the
    // new modified solution?

    // Until we find a way to do that, we need to find a way to create modifiables for the alignment
    // even when it is not random.
    
    // Suppose we convert the alignment matrix into a graph of partially-ordered columns, where
    // each column object just records pointers to the next column object for each sequence with
    // a character in that column.
    // This would be like what we get when we start with a line graph for each sequence and then begin
    // merging columns.

    expression_ref partition = {var("Data.List.!!"), {var("BAliPhy.ATModel.partitions"),p->my_atmodel()}, i};

    param alignments = p->add_compute_expression({var("BAliPhy.ATModel.DataPartition.pairwise_alignments"), partition});

    //  const int n_states = state_letters().size();
    int scale_index = *p->scale_index_for_partition(i);
    int smodel_index = *p->smodel_index_for_partition(i);
    auto imodel_index = p->imodel_index_for_partition(i);

    // R1. Add method indices for calculating transition matrices.
    auto transition_ps = p->get_expression(p->PC->branch_transition_p_indices(scale_index, smodel_index));
    for(int b=0;b<B;b++)
        transition_p_method_indices.push_back( p->add_compute_expression( {var("Data.Array.!"), transition_ps, b} ) );

    // R2. Register array of leaf sequences
    for(int i=0; i<p->t().n_leaves(); i++)
        leaf_sequence_indices.push_back ( p->add_compute_expression(EVector(sequences[i])) );

    expression_ref seqs_array = {var("BAliPhy.ATModel.DataPartition.leaf_sequences"),partition};

    seqs_array = p->get_expression( p->add_compute_expression(seqs_array));

    // R3. Register array of pairwise alignments
    expression_ref as = p->get_expression( p->add_compute_expression({var("Data.Array.listArray'"), alignments.ref(*p)}) );

    // R4. Register sequence length methods
    auto seq_lengths = expression_ref{{var("Data.Array.listArray'"),{var("Alignment.compute_sequence_lengths"), seqs_array, p->my_tree(), as}}};
    expression_ref alignment_on_tree = {var("Alignment.AlignmentOnTree"), p->my_tree(), t.n_nodes(), seq_lengths, as};
    alignment_on_tree = p->get_expression( p->add_compute_expression(alignment_on_tree) );

    seq_lengths = expression_ref{var("Alignment.sequence_lengths"),alignment_on_tree};
    for(int n=0;n<t.n_nodes();n++)
        sequence_length_indices[n] = p->add_compute_expression( {var("Data.Array.!"), seq_lengths, n} );

    // Add method indices for calculating branch HMMs and alignment prior
    if (imodel_index)
    {
        // D = Params.substitutionBranchLengths!scale_index
        expression_ref D = {var("Data.Array.!"), p->my_substitution_branch_lengths(), scale_index};
        expression_ref heat = parameter("Heat.beta");
        expression_ref training = parameter("*IModels.training");
        expression_ref model = {{var("Data.Array.!"),p->my_imodels(), *imodel_index}, heat, training};

        expression_ref hmms = {var("Alignment.branch_hmms"), model, D, B};
        hmms = p->get_expression( p->add_compute_expression(hmms) );

        for(int n=0;n<sequence_length_pr_indices.size();n++)
        {
            expression_ref l = sequence_length_indices[n].ref(*p);
            expression_ref lengthp = {snd,model};
            sequence_length_pr_indices[n] = p->add_compute_expression( {lengthp,l} );
        }

        // branch HMMs
        for(int b=0;b<B;b++)
        {
            // (fst IModels.models!imodel_index) D b heat training
            int index = p->add_compute_expression( {var("Data.Array.!"), hmms, b} );
            branch_HMM_indices.push_back( index );
        }

        // Alignment prior
        alignment_prior_index = p->add_compute_expression( {var("Alignment.alignment_pr"), alignment_on_tree, hmms, model} );

        expression_ref alignment_pdf = alignment_prior_index.ref(*p);
        alignment_pdf = make_if_expression(parameter("*variable_alignment"), alignment_pdf, log_double_t(1.0));

        expression_ref sample_alignments = {var("Parameters.random_variable"), as, alignment_pdf, 0, 0.0};
        int alignment_sample_index = p->add_compute_expression( sample_alignments );
        p->evaluate( alignment_sample_index );
    }

    if (p->t().n_nodes() == 1)
    {
        expression_ref seq = {var("Data.Array.!"),seqs_array, 0};
        auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
        likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_1"), seq, *a, f});
    }
    else if (likelihood_calculator == 0)
    {
        // R5. Register counts array
        vector<vector<int>> seq_counts = alignment_letters_counts(AA, t.n_leaves(), counts);
        EVector counts_;
        for(int i=0; i<leaf_sequence_indices.size(); i++)
            counts_.push_back( EVector(seq_counts[i]) );
        auto counts_array = p->get_expression( p->add_compute_expression({var("Data.Array.listArray'"),get_list(counts_)}) );

        // R6. Register conditional likelihoods
        auto t = p->my_tree();
        auto f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);
        cl_index = p->add_compute_expression({var("SModel.Likelihood.cached_conditional_likelihoods"),t,seqs_array,counts_array,as,*a,transition_ps,f});  // Create and set conditional likelihoods for each branch
        auto cls = cl_index.ref(*p);
        for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
            conditional_likelihoods_for_branch[b] = p->add_compute_expression({var("Data.Array.!"),cls,b});

        // FIXME: broken for fixed alignments of 2 sequences.
        if (p->t().n_nodes() == 2)
        {
            expression_ref seq1 = {var("Data.Array.!"), seqs_array, 0};
            expression_ref seq2 = {var("Data.Array.!"), seqs_array, 1};
            expression_ref A = {var("Data.Array.!"), as, 0};
            expression_ref P = {var("Data.Array.!"), transition_ps, 0};
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
        Box<alignment> AAA = AA;
        cl_index = p->add_compute_expression({var("SModel.Likelihood.cached_conditional_likelihoods_SEV"),t,seqs_array,*a,transition_ps,f,AAA});  // Create and set conditional likelihoods for each branch
        auto cls = cl_index.ref(*p);
        for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
            conditional_likelihoods_for_branch[b] = p->add_compute_expression({var("Data.Array.!"),cls,b});

        object_ptr<EVector> Counts(new EVector(counts));

        // FIXME: broken for fixed alignments of 2 sequences.
        if (p->t().n_nodes() == 2)
        {
            expression_ref seq1 = {var("Data.Array.!"), seqs_array, 0};
            expression_ref seq2 = {var("Data.Array.!"), seqs_array, 1};
            expression_ref A = {var("Data.Array.!"), as, 0};
            expression_ref P = {var("Data.Array.!"), transition_ps, 0};
            expression_ref f = p->get_expression(p->PC->SModels[smodel_index].weighted_frequency_matrix);

            likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_2"), seq1, seq2, *a, A, P, f});
        }
        else
        {
            auto root = parameter("*subst_root");
            likelihood_index = p->add_compute_expression({var("SModel.Likelihood.peel_likelihood_SEV"), t, cls, f, root, Counts});
        }
    }

    p->add_likelihood_factor(likelihood_index.ref(*p));

    /* Initialize params -- from alignments.ref(*p) */
    expression_ref alignments_structure = p->evaluate_expression({var("Parameters.maybe_modifiable_structure"), alignments.ref(*p)});
    if (log_verbose >= 3)
        std::cerr<<"alignments = "<<alignments_structure<<"\n";
    auto alignments_vector = *list_to_evector(alignments_structure);
    assert(alignments_vector.size() == 2*B);
    for(int b=0;b<2*B;b++)
        pairwise_alignment_for_branch.push_back( get_param(*p, alignments_vector[b]) );
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

void tree_constants::register_branch_lengths(context* C, const expression_ref& branch_lengths_exp)
{
    int B = parameters_for_tree_branch.size()/2;
    if (B == 0) return;

    int branch_lengths_index = C->add_compute_expression( branch_lengths_exp );
    C->evaluate(branch_lengths_index);
    auto branches_structure = C->evaluate_expression({var("Parameters.maybe_modifiable_structure"), C->get_expression(branch_lengths_index)});
    auto branch_durations_exp = C->get_expression(branch_lengths_index);

    if (log_verbose >= 3)
        std::cerr<<"branch lengths = "<<branches_structure<<"\n\n";

    // Create the parameters that hold branch lengths
    for(int b=0;b<B;b++)
    {
        int index = C->add_compute_expression( {var("Data.Array.!"), branch_durations_exp, b} );

        branch_durations.push_back( get_param(*C, branches_structure.sub()[b]) );
        branch_duration_index.push_back(index);
    }
}

tree_constants::tree_constants(context* C, const vector<string>& labels, int tree_head_)
    :tree_head(tree_head_),
     n_leaves(0),
     node_labels(labels)
{
    //------------------------- Create the tree structure -----------------------//
    auto tree_structure = C->evaluate_expression({var("Parameters.maybe_modifiable_structure"), C->get_expression(tree_head)});

    if (log_verbose >= 3)
        std::cerr<<"tree = "<<tree_structure<<"\n\n";

    auto edges_out_of_node = tree_structure.sub()[0];
    auto nodes_for_edge    = tree_structure.sub()[1];
    int n_nodes            = tree_structure.sub()[2].as_int();
    int n_branches         = tree_structure.sub()[3].as_int();

    if (log_verbose >= 3)
        std::cerr<<"num_branches = "<<C->evaluate_expression({var("Parameters.maybe_modifiable_structure"), {var("Tree.numBranches"), C->get_expression(tree_head)}})<<"\n\n";;

    assert(node_labels.size() == n_nodes);

    for(int n=0; n < n_nodes; n++)
    {
        auto edges = list_to_evector(edges_out_of_node.sub()[n]);
        assert(edges);

        vector<param> m_edges;
        for(auto& edge: *edges)
            m_edges.push_back(get_param(*C,edge));

        parameters_for_tree_node.push_back ( m_edges );

        if (m_edges.size() < 2) n_leaves++;
    }

    for(int b=0; b < 2*n_branches; b++)
    {
        auto nodes = nodes_for_edge.sub()[b];

        assert(has_constructor(nodes,"(,,,)"));
        param m_source = get_param(*C, nodes.sub()[0]);
        param m_source_index = get_param(*C, nodes.sub()[1]);
        param m_target = get_param(*C, nodes.sub()[2]);

        parameters_for_tree_branch.push_back( std::tuple<param, param, param>{m_source, m_source_index, m_target} );
    }
}

expression_ref tree_expression(const SequenceTree& T)
{
    /*------------------------- Create the tree structure -----------------------*/
    vector<expression_ref> node_branches;
    for(int n=0; n < T.n_nodes(); n++)
    {
        vector<expression_ref> node;
        for(auto& edge: edges_connecting_to_node(T,n))
            node.push_back( edge );
    
        node_branches.push_back( get_list(node) );
    }
    expression_ref node_branches_array = {var("Data.Array.listArray'"),get_list(node_branches)};

    vector<expression_ref> branch_nodes;
    for(int b=0; b < 2*T.n_branches(); b++)
    {
        expression_ref source = T.directed_branch(b).source().name();
        // FIXME: What position in the edges_out_of_node list for the source node is this branch?
        expression_ref source_index = 0;
        expression_ref target = T.directed_branch(b).target().name();
        int reverse_branch = T.directed_branch(b).reverse();
        branch_nodes.push_back( Tuple(source, source_index, target, reverse_branch) );
    }
    expression_ref branch_nodes_array = {var("Data.Array.listArray'"),get_list(branch_nodes)};

    expression_ref tree_con = var("Tree.Tree");

    return {tree_con, node_branches_array, branch_nodes_array, T.n_nodes(), T.n_branches()};
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
        throw myexception()<<"Can't set alignment with alphabet '"<<A.get_alphabet()<<"' in partition with alphabet '"<<get_alphabet().name<<"'";

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
        auto source = get<0>(TC->parameters_for_tree_branch[b]).get_value(*this);
        auto target = get<2>(TC->parameters_for_tree_branch[b]).get_value(*this);
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

double Parameters::get_branch_scale(int s) const
{
    return branch_scale(s).get_value(*this).as_double();
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


const param& Parameters::branch_scale(int i) const
{
    assert(0 <= i and i < n_branch_scales());

    return PC->branch_scales_[i];
}

void Parameters::set_branch_scale(int s, double x)
{
    if (auto R = branch_scale(s).is_modifiable(*this))
        return set_modifiable_value(*R, x);
    else
        throw myexception()<<"Branch scale "<<s+1<<" is not directly modifiable!";
}

double Parameters::get_branch_subst_rate(int p, int /* b */) const
{
    return get_branch_scale( *scale_index_for_partition(p) );
}

expression_ref Parameters::my_tree() const
{
    assert(TC);
    return get_expression(TC->tree_head);
}

expression_ref Parameters::my_atmodel() const
{
    assert(atmodel);
    return PC->atmodel.ref(*this);
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

expression_ref logger(const string& prefix, const expression_ref& x, const expression_ref& x_loggers, bool do_log = true)
{
    auto maybe_x = do_log ? expression_ref({ var("Data.Maybe.Just"), x }) : expression_ref(var("Data.Maybe.Nothing"));
    return Tuple( prefix, Tuple( maybe_x, x_loggers) );
}

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

    expression_ref bind_and_log_model(const string& prefix, const expression_ref& model, vector<expression_ref>& loggers, bool do_log = true)
    {
        auto [x, x_loggers] = bind_model(prefix,model);
        loggers.push_back( logger(prefix, x, x_loggers, do_log) );
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
    *this += { "SModel", "Probability", "Range", "PopGen", "Alignment", "IModel", "BAliPhy.ATModel" };
  
    // Don't call set_parameter_value here, because recalc( ) depends on branch_length_indices, which is not ready.

    PC->constants.push_back(-1);

    add_modifiable_parameter("Heat.beta", 1.0);
    variable_alignment_param = add_modifiable_parameter("*variable_alignment", variable_alignment_);

    /* ---------------- compress alignments -------------------------- */

    // FIXME! Make likelihood_calculators for 1- and 2-sequence alignments handle compressed alignments.
    bool allow_compression = load_value("site-compression", tt.n_nodes() > 2);

    vector<optional<compressed_alignment>> compressed_alignments(A.size());
    vector<const alignment*> alignments(A.size());
    for(int i=0;i<A.size();i++)
    {
        if (not imodel_index_for_partition(i) and allow_compression)
        {
            compressed_alignments[i] = compress_alignment(A[i], tt);
            alignments[i] = &compressed_alignments[i]->A;
            std::cerr<<"Partition #"<<i+1<<": "<<A[i].length()<<" columns -> "<<alignments[i]->length()<<" unique patterns.\n";
        }
        else
            alignments[i] = &A[i];
    }

    /* ---------------- Set up the tree ------------------------------ */
    branches_from_affected_node.resize(tt.n_nodes());

    auto tree_var = var("topology1");

    /* --------------------------------------------------------------- */
    do_block program;
    // ATModel smodels imodels scales branch_lengths
    // Loggers = [(string,(Maybe a,Loggers)]
    vector<expression_ref> program_loggers;
    // Therefore, we are constructing a list with values [(prefix1,(Just value1, loggers1)), (prefix1, (Just value1, loggers2))

    expression_ref topology_model1 = {var("Probability.Random.sample"), {var("Probability.Distribution.Tree.uniform_topology"), tt.n_leaves()}};
    program.perform(tree_var, topology_model1);

    // P1. Substitution models
    vector<expression_ref> smodels;
    for(int i=0;i<SMs.size();i++)
    {
        string prefix = "S" + convertToString(i+1);

        optional<int> first_index;
        for(int j=0;j<s_mapping.size();j++)
            if (s_mapping[j] and *s_mapping[j] == i)
                first_index = j;

        const alphabet& a = A[*first_index].get_alphabet();

        expression_ref smodel = SMs[i].expression;
        smodel = {var("Probability.Random.set_alphabet'"), a, smodel};

        auto smodel_var = program.bind_and_log_model(prefix , smodel, program_loggers, false);
        smodels.push_back(smodel_var);
    }


    // P2. Indel models
    vector<expression_ref> imodels;
    for(int i=0;i<n_imodels();i++)
    {
        string prefix = "I" + convertToString(i+1);
        expression_ref imodel = IMs[i].expression;
        auto imodel_var = program.bind_and_log_model(prefix, imodel, program_loggers, false);

        imodels.push_back({imodel_var, tree_var});
    }


    // P3. Scales
    vector<expression_ref> scales;
    for(int i=0; i<n_branch_scales(); i++)
    {
        // FIXME: Ideally we would actually join these models together using a Cons operation and prefix.
        //        This would obviate the need to create a Scale1 (etc) prefix here.
        string prefix = "Scale"+convertToString(i+1);

        auto scale_model = scaleMs[i].expression;
        auto scale_var = program.bind_and_log_model(prefix , scale_model, program_loggers, false);
        scales.push_back(scale_var);
    }
    program_loggers.push_back( logger("Scale", get_list(scales), List()) );


    // P4. Branch lengths
    expression_ref branch_lengths_list = List();
    if (tt.n_branches() > 0)
    {
        string prefix = "T:lengths";
        expression_ref branch_lengths = {branch_length_model.expression, tree_var};
        auto [x,loggers] = program.bind_model(prefix , branch_lengths);
        branch_lengths_list = x;
    }


    // P5. Create objects for data partitions
    vector<expression_ref> partitions;
    for(int i=0; i < A.size(); i++)
    {
        // P5.I Register array of leaf sequences
        auto sequences = alignment_letters(*alignments[i], tt.n_leaves());

        // Add array of leaf sequences
        EVector seqs_;
        for(int j=0; j<tt.n_leaves(); j++)
        {
            param leaf_seq_j = add_compute_expression(EVector(sequences[j]));
            seqs_.push_back( leaf_seq_j.ref(*this) );
        }

        param seqs_array = add_compute_expression({var("Data.Array.listArray'"),get_list(seqs_)});

        // P5.II Create modifiables for pairwise alignments
        expression_ref initial_alignments_exp = get_list(unaligned_alignments_on_tree(tt, sequences));

        expression_ref alignments = {var("Data.List.map"),var("Parameters.modifiable"),initial_alignments_exp};

        partitions.push_back({var("BAliPhy.ATModel.DataPartition.Partition"), tree_var, seqs_array.ref(*this), alignments, 0});
    }


    // We haven't done the observe's yet, though.
    expression_ref program_exp = program.finish_return(
        Tuple(
            {var("BAliPhy.ATModel.ATModel"), tree_var, get_list(smodels), get_list(imodels), get_list(scales), branch_lengths_list, get_list(partitions)},
            get_list(program_loggers))
        );
    
    PC->atmodel = add_program(program_exp);

    int tree_index = add_compute_expression( {var("BAliPhy.ATModel.tree"), my_atmodel()} );

    TC = new tree_constants(this, tt.get_labels(), tree_index);

    t().read_tree(tt);

    /* --------------------------------------------------------------- */

    // R1. Register branch lengths
    TC->register_branch_lengths(this, {var("Data.Array.listArray'"),{var("BAliPhy.ATModel.branch_lengths"),my_atmodel()}});

    int scales_list_index = add_compute_expression( {var("BAliPhy.ATModel.scales"),my_atmodel()} );
    expression_ref scales_list = get_expression(scales_list_index);

    // R2. Register individual scales
    expression_ref scales_structure = evaluate_expression({var("Parameters.maybe_modifiable_structure"), scales_list});
    auto scales_vector = *list_to_evector(scales_structure);
    for(int i=0; i<n_branch_scales();i++)
        PC->branch_scales_.push_back( get_param(*this, scales_vector[i] ) );

    // P1. Add substitution root node
    subst_root_index = add_modifiable_parameter("*subst_root", t().n_nodes()-1);

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
        expression_ref smodel = {var("Data.List.!!"),{var("BAliPhy.ATModel.smodels"), my_atmodel()}, i};

        PC->SModels.push_back( smodel_methods( smodel, *this) );
    }

    // P2. Add training parameter
    add_modifiable_parameter("*IModels.training", false);

    // R4. Register an array of indel models
    PC->imodels_index = add_compute_expression({var("Data.Array.listArray'"), {var("BAliPhy.ATModel.imodels"), my_atmodel()}});
  
    // don't constrain any branch lengths
    for(int b=0;b<PC->TC.n_branches();b++)
        PC->TC.branch(b).set_length(-1);

    // R5. Register D[b,s] = T[b]*scale[s]
    for(int s=0;s<n_branch_scales();s++)
    {
        expression_ref scale = branch_scale(s).ref(*this);
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
        add_modifiable_parameter(name, 0);
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

    PC->substitution_branch_lengths_index = add_compute_expression({var("Data.Array.listArray'"),{var("Prelude.fmap"),var("Data.Array.listArray'"),substitutionBranchLengthsList}});

    // R8. register the cached transition_p indices
    PC->branch_transition_p_indices.resize(n_branch_scales(), n_smodels());
    for(int s=0;s < n_branch_scales(); s++)
    {
        // Better yet, make a substitutionBranchLengths!scale!branch that can be referenced elsewhere.
        expression_ref DL = {var("Data.Array.!"), my_substitution_branch_lengths(), s};

        // Here, for each (scale,model) pair we're construction a function from branches -> Vector<transition matrix>
        for(int m=0;m < n_smodels(); m++)
        {
            expression_ref S = get_expression(PC->SModels[m].main);
            PC->branch_transition_p_indices(s,m) = add_compute_expression({var("SModel.transition_p_index"), my_tree(), S, branch_cat_list, DL});
        }
    }

    // create data partitions

    assert(like_calcs.size() == A.size());
    for(int i=0;i<A.size();i++)
    {
        if (compressed_alignments[i])
        {
            // construct compressed alignment, counts, and mapping
            auto& [AA, counts, mapping] = *compressed_alignments[i];
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

