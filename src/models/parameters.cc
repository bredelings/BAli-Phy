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
#include <sstream>
#include "util/assert.hh"

#include "models/parameters.H"
#include "dp/2way.H"
#include "util/rng.H"
#include "util/set.H"
#include "util/io.H"
#include "util/log-level.H"
#include "substitution/substitution.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "mcmc/proposals.H"
#include "probability/probability.H"
#include "computation/expression/lambda.H"
#include "computation/expression/bool.H"
#include "computation/expression/apply.H"
#include "computation/expression/maybe.H"
#include "computation/expression/case.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/let.H"
#include "computation/expression/do_block.H"
#include "computation/module.H"
#include "computation/operations.H" // for VectorFromList<>
#include "math/exponential.H"
#include "models/setup.H"
#include "site-compression.H"
#include "tree-align/link2.H"
#include "computation/machine/graph_register.H"

using std::vector;
using std::string;
using std::pair;
using std::set;
using std::cerr;
using std::endl;
using std::ostream;
using std::map;
using std::tuple;

using std::optional;
namespace fs = boost::filesystem;

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

Matrix data_partition::WeightedFrequencyMatrix() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].weighted_frequency_matrix).as_<Box<Matrix>>();
}

EVector data_partition::state_letters() const
{
    int s = *P->smodel_index_for_partition(partition_index);
    return P->evaluate(P->PC->SModels[s].state_letters).as_<EVector>();
}

const indel::PairHMM& data_partition::get_branch_HMM(int b) const
{
    assert(variable_alignment());

    b = t().undirected(b);

    return DPC().branch_HMMs[b].get_value(*P).as_<indel::PairHMM>();
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

    assert(has_pairwise_alignments());

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
    assert(likelihood_calculator() == 0);
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
    assert(likelihood_calculator() == 0);
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
    assert(likelihood_calculator() == 0);
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

EVector data_partition::ancestral_sequences() const
{
    return DPC().ancestral_sequences_index.get_value(*P).as_<EVector>();
}

log_double_t data_partition::heated_likelihood() const 
{
    // Don't waste time calculating likelihood if we're sampling from the prior.
    if (get_beta() == 0)
        return 1;
    else
        return pow(likelihood(),get_beta());
}

bool data_partition::has_pairwise_alignments()  const
{
    // FIXME -- perhaps we should make the vector of pairwise alignments into an optionl<vector<T>> and check that.
    return likelihood_calculator() == 0;
}

bool data_partition::has_alignment_matrix() const
{
    return likelihood_calculator() == 1;
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

vector<param> get_params_from_list(context* C, const expression_ref& list, std::optional<int> check_size = {})
{
    vector<param> params;
    expression_ref structure = C->evaluate_expression({var("Parameters.maybe_modifiable_structure"), list});

    if (log_verbose >= 3)
        std::cerr<<"structure = "<<structure<<"\n\n";

    auto vec = *list_to_evector(structure);
    for(auto& e: vec)
        params.push_back( get_param(*C, e) );

    if (check_size and (params.size() != *check_size))
        throw myexception()<<"Expected a list of length "<<*check_size<<", but got one of length "<<params.size()<<"!";

    return params;
}

vector<param> get_params_from_array(context* C, const expression_ref& array, std::optional<int> check_size = {})
{
    vector<param> params;
    expression_ref structure = C->evaluate_expression({var("Parameters.maybe_modifiable_structure"), array});

    if (log_verbose >= 3)
        std::cerr<<"structure = "<<structure<<"\n\n";

    for(auto& e: structure.sub())
        params.push_back( get_param(*C, e) );

    if (check_size and (params.size() != *check_size))
        throw myexception()<<"Expected an array "<<*check_size<<", but got one of length "<<params.size()<<"!";

    return params;
}

data_partition_constants::data_partition_constants(Parameters* p, int i, const alphabet& a_, int like_calc)
    :conditional_likelihoods_for_branch(2*p->t().n_branches()),
     sequence_length_indices(p->t().n_nodes()),
     sequence_length_pr_indices(p->t().n_nodes()),
     seqs( p->t().n_nodes() ),
     sequences(),
     a(a_.clone()),
     branch_HMM_type(p->t().n_branches(),0),
     likelihood_calculator(like_calc)
{
    const auto& t = p->t();
    int B = t.n_branches();

    auto labels = p->get_labels();
    for(int i=0;i<t.n_nodes();i++)
        seqs[i].name = labels[i];

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

    expression_ref partition = {var("Data.List.!!"), {var("BAliPhy.ATModel.partitions"), p->my_atmodel()}, i};

    //  const int n_states = state_letters().size();
    auto imodel_index = p->imodel_index_for_partition(i);

    // R1. Add method indices for calculating transition matrices.
    expression_ref transition_ps = {var("Data.List.!!"),p->my_partition_transition_ps(),i};
    for(int b=0;b<B;b++)
        transition_p_method_indices.push_back( p->add_compute_expression( {var("Data.Array.!"), transition_ps, b} ) );

    if (like_calc == 0)
    {
        // R2. Register array of leaf sequences
        expression_ref leaf_sequences = {var("Data.List.!!"),{var("BAliPhy.ATModel.leaf_sequences"),p->my_atmodel_export()},i};
        for(int i=0; i<p->t().n_leaves(); i++)
            leaf_sequence_indices.push_back( p->add_compute_expression({var("Data.Array.!"),leaf_sequences,i}) );

        for(int i=0; i<p->t().n_leaves(); i++)
            sequences.push_back( (vector<int>)(leaf_sequence_indices[i].get_value(*p).as_<EVector>()) );
    }

    if (like_calc == 0)
    {
        // Extract pairwise alignments from data partition
        auto alignment_on_tree = expression_ref{var("BAliPhy.ATModel.DataPartition.get_alignment"), partition};
        alignment_on_tree = p->get_expression( p->add_compute_expression(alignment_on_tree) );

        /* Initialize params -- from alignments.ref(*p) */
        auto as = expression_ref{var("Bio.Alignment.pairwise_alignments"), alignment_on_tree};
        pairwise_alignment_for_branch = get_params_from_array(p, as, 2*B);

        auto seq_lengths = expression_ref{var("Bio.Alignment.sequence_lengths"),alignment_on_tree};
        for(int n=0;n<t.n_nodes();n++)
            sequence_length_indices[n] = p->add_compute_expression( {var("Data.Array.!"), seq_lengths, n} );

        // Add method indices for calculating branch HMMs and alignment prior
        if (imodel_index)
        {
            // D = Params.substitutionBranchLengths!scale_index

            // R5. Register probabilities of each sequence length
            expression_ref model = {fromJust,{var("BAliPhy.ATModel.DataPartition.imodel"),partition}};
            expression_ref lengthp = {snd,model};
            for(int n=0;n<sequence_length_pr_indices.size();n++)
            {
                expression_ref l = sequence_length_indices[n].ref(*p);
                sequence_length_pr_indices[n] = p->add_compute_expression( {lengthp,l} );
            }

            // R6. Register branch HMMs
            param hmms = p->add_compute_expression({fromJust,{var("BAliPhy.ATModel.DataPartition.hmms"),partition}});
            for(int b=0;b<B;b++)
                branch_HMMs.push_back( p->add_compute_expression( {var("Data.Array.!"), hmms.ref(*p), b} ) );

            // Alignment prior
            alignment_prior_index = p->add_compute_expression( {var("Probability.Distribution.RandomAlignment.alignment_pr"), alignment_on_tree, hmms.ref(*p), model} );
        }
    }

    cl_index = p->add_compute_expression({var("Data.List.!!"),p->my_partition_cond_likes(),i});

    likelihood_index = p->add_compute_expression({var("Data.List.!!"),p->my_partition_likelihoods(),i});

    ancestral_sequences_index = p->add_compute_expression({var("Data.List.!!"),p->my_partition_ancestral_sequences(),i});

    for(int b=0;b<conditional_likelihoods_for_branch.size();b++)
        conditional_likelihoods_for_branch[b] = p->add_compute_expression({var("Data.Array.!"),cl_index.ref(*p),b});

}

//-----------------------------------------------------------------------------//
smodel_methods::smodel_methods(const expression_ref& E, context& C)
{
    expression_ref V = var("Foreign.Vector.list_to_vector");

    main = C.add_compute_expression( E );
    expression_ref S = C.get_expression(main);

    n_base_models = C.add_compute_expression({var("SModel.nBaseModels"), S});
    n_states =  C.add_compute_expression({var("SModel.nStates"), S});
    weighted_frequency_matrix = C.add_compute_expression({var("SModel.weighted_frequency_matrix"), S});
    get_alphabet = C.add_compute_expression({var("SModel.getAlphabet"), S});
    state_letters = C.add_compute_expression({var("SModel.stateLetters"), S});
    n_states = C.add_compute_expression({var("SModel.nStates"), S});
    rate = C.add_compute_expression({var("SModel.rate"), S});
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
    auto branch_lengths = C->get_expression(branch_lengths_index);

    branch_durations = get_params_from_list(C, branch_lengths, B);
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

    if (has_constructor(tree_structure,"Tree.LabelledTree"))
    {
        assert(tree_structure.sub().size() == 2);
        tree_structure = tree_structure.sub()[0];
    }
    assert(tree_structure.sub().size() == 4);

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
    assert(PC);
    auto e = PC->variable_alignment.get_value(*this);
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
        PC->variable_alignment.set_value(*this, variable_alignment_);

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
        throw myexception()<<"Can't set alignment with alphabet '"<<A.get_alphabet().print()<<"' in partition with alphabet '"<<get_alphabet().name<<"'";

    auto T = t();

    auto labels = P->get_labels();

    // 2. Reorder and maybe extend the alignment
    auto AA = link_A(A, labels, T);

    // 3. Check that the alignment doesn't disagree with existing leaf sequences lengths!
    for(int i=0;i<T.n_leaves();i++)
    {
        assert(AA.seq(i).name == labels[i]);
        if (AA.seqlength(i) != seqlength(i))
            throw myexception()<<"partition "<<partition_index+1<<", sequence "<<AA.seq(i).name<<": alignment sequence length "<<AA.seqlength(i)<<" does not match required sequence length "<<seqlength(i);
    }

    // 4. Set pairwise alignment parameters.
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
    int b04 = t().find_branch(nodes[0],nodes[4]);
    int b14 = t().find_branch(nodes[1],nodes[4]);
    int b25 = t().find_branch(nodes[2],nodes[5]);
    int b35 = t().find_branch(nodes[3],nodes[5]);

    vector<vector<HMM::bitmask_t>> a123456(n_data_partitions());
    for(int i=0;i<n_data_partitions();i++)
    {
        if (get_data_partition(i).has_pairwise_alignments())
            a123456[i] = A5::get_bitpath((*this)[i], order);
    }

    // 3. Perform NNI
    exchange_subtrees(b1, b2);  // alter tree
    std::swap(nodes[0],nodes[2]); // alter nodes
    std::swap(b04, b25);

    for(int i=0;i<n_data_partitions();i++)
    {
        if (get_data_partition(i).has_pairwise_alignments())
            for(auto& col: a123456[i]) // alter matrix
            {
                auto col2 = col;
                col.set(0,col2.test(2));
                col.set(2,col2.test(0));
            }
    }

    // 4. Fix-up the alignment matrix (bits)
    for(int i=0;i<n_data_partitions();i++)
    {
        if (not get_data_partition(i).has_pairwise_alignments()) continue;

        if (get_data_partition(i).variable_alignment() and allow_disconnect_subtree)
            disconnect_subtree(a123456[i]);
        else
        {
            disconnect(a123456[i]);
            minimally_connect(a123456[i]);
        }
    }

    assert(b04 == t().find_branch(nodes[0],nodes[4]));
    assert(b14 == t().find_branch(nodes[1],nodes[4]));
    assert(b25 == t().find_branch(nodes[2],nodes[5]));
    assert(b35 == t().find_branch(nodes[3],nodes[5]));
    assert(b45 == t().find_branch(nodes[4],nodes[5]));

    // 5. Set the pairwise alignments.
    for(int i=0;i<n_data_partitions();i++)
    {
        auto dp = get_data_partition(i);
        if (dp.has_pairwise_alignments())
        {
            dp.set_pairwise_alignment(b04, get_pairwise_alignment_from_bits(a123456[i], 0, 4));
            dp.set_pairwise_alignment(b14, get_pairwise_alignment_from_bits(a123456[i], 1, 4));
            dp.set_pairwise_alignment(b25, get_pairwise_alignment_from_bits(a123456[i], 2, 5));
            dp.set_pairwise_alignment(b35, get_pairwise_alignment_from_bits(a123456[i], 3, 5));
            dp.set_pairwise_alignment(b45, get_pairwise_alignment_from_bits(a123456[i], 4, 5));
        }
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

void Parameters::cache_likelihood_branches() const
{
    auto branches = t().branches_in(subst_root());
    for(int i=0; i < n_data_partitions();i++)
        for(auto b: branches)
            (*this)[i].cache(b);
}

void Parameters::select_root(int b) const
{
    if (t().source(b) == subst_root() or t().target(b) == subst_root())
        return;
  
    int r = t().reverse(b);
    if (t().subtree_contains(r, subst_root()))
        b = r;

    set_root_(t().target(b));
}

void Parameters::set_root_(int node) const
{
    assert(not t().is_leaf_node(node));
    const context* C = this;
    PC->subst_root.set_value(*const_cast<context*>(C), node);
}

void Parameters::set_root(int node) const
{
    if (subst_root() != node)
        set_root_(node);
    assert(subst_root() == node);
}

bool Parameters::get_imodel_training() const
{
    return is_bool_true( PC->imodel_training.get_value(*this) );
}

void Parameters::set_imodel_training(bool t) const
{
    const context* C = this;
    PC->imodel_training.set_value(*const_cast<context*>(C), t);
}

int Parameters::subst_root() const
{
    return PC->subst_root.get_value(*this).as_int();
}

void Parameters::set_beta(double b)
{
    PC->heat.set_value(*this, b);
}

double Parameters::get_beta() const
{
    return PC->heat.get_value(*this).as_double();
}

int Parameters::get_branch_category(int b) const
{
    return PC->branch_categories[b].get_value(*this).as_int();
}

void Parameters::set_branch_category(int b, int c)
{
    PC->branch_categories[b].set_value(*this,c);
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

expression_ref Parameters::my_tree() const
{
    assert(TC);
    return get_expression(TC->tree_head);
}

expression_ref Parameters::my_atmodel() const
{
    assert(PC);
    return PC->atmodel.ref(*this);
}

expression_ref Parameters::my_atmodel_export() const
{
    assert(PC);
    return PC->atmodel_export.ref(*this);
}

expression_ref Parameters::my_partition_likelihoods() const
{
    assert(PC);
    return PC->partition_likelihoods.ref(*this);
}

expression_ref Parameters::my_partition_cond_likes() const
{
    assert(PC);
    return PC->partition_cond_likes.ref(*this);
}

expression_ref Parameters::my_partition_ancestral_sequences() const
{
    assert(PC);
    return PC->partition_ancestral_seqs.ref(*this);
}

expression_ref Parameters::my_partition_transition_ps() const
{
    assert(PC);
    return PC->partition_transition_ps.ref(*this);
}

expression_ref Parameters::my_subst_root() const
{
    assert(PC);
    return PC->subst_root.ref(*this);
}

expression_ref Parameters::my_variable_alignment() const
{
    assert(PC);
    return PC->variable_alignment.ref(*this);
}

expression_ref Parameters::heat_exp() const
{
    assert(PC);
    return PC->heat.ref(*this);
}

expression_ref Parameters::imodel_training_exp() const
{
    assert(PC);
    return PC->imodel_training.ref(*this);
}

int num_distinct(const vector<optional<int>>& v)
{
    int m = -1;
    for(auto& x: v)
        if (x)
            m = std::max(m,*x);
    return m+1;
}

int get_num_models(const vector<optional<int>>& mapping)
{
    int m = -1;
    set<int> models;
    for(auto& model: mapping)
        if (model)
        {
            assert(*model >= 0);
            m = std::max(m,*model);
            models.insert(*model);
        }

    int n = models.size();
    assert(m+1 == n);
    return n;
}

parameters_constants::parameters_constants(int n_partitions, const SequenceTree& t,
                                           const vector<optional<int>>& s_mapping,
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
    if (smodel_for_partition.size() != n_partitions)
        throw myexception()<<"There are "<<n_partitions
                           <<" data partitions, but you mapped smodels onto "
                           <<smodel_for_partition.size();

    int n_smodels = get_num_models(s_mapping);

    // check that we only map existing smodels to data partitions
    for(int i=0;i<smodel_for_partition.size();i++) {
        int m = *smodel_for_partition[i];
        if (m >= n_smodels)
            throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
                               <<" because there are only "<<n_smodels<<" smodels.";
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

expression_ref get_genetic_code_expression(const Genetic_Code& code)
{
    if (code.name() == "standard")
        return var("standard_code");
    else
        throw myexception()<<"Need to add a Haskell function for genetic code '"<<code.name()<<"'";
}

expression_ref get_alphabet_expression(const alphabet& a)
{
    if (a.name == "DNA")
        return  var("Bio.Alphabet.dna");
    else if (a.name == "RNA")
        return var("Bio.Alphabet.rna");
    else if (a.name == "Amino-Acids")
        return var("Bio.Alphabet.aa");
    else if (auto codons = dynamic_cast<const Codons*>(&a))
    {
        auto nucs = get_alphabet_expression(codons->getNucleotides());
        auto code = get_genetic_code_expression(codons->getGenetic_Code());
        return {var("codons"), nucs, code};
    }
    else if (auto triplets = dynamic_cast<const Triplets*>(&a))
    {
        auto nucs = get_alphabet_expression(triplets->getNucleotides());
        return {var("triplets"),nucs};
    }
    else if (auto doublets = dynamic_cast<const Doublets*>(&a))
    {
        auto nucs = get_alphabet_expression(doublets->getNucleotides());
        return {var("doublets"),nucs};
    }
    else
    {
        throw myexception()<<"Can't translate alphabet "<<a.name;
    }
}

// FIXME: move this routine to A-T-Model

std::string generate_atmodel_program(int n_sequences,
                                     const vector<expression_ref>& alphabet_exps,
                                     const vector<pair<string,string>>& filename_ranges,
                                     const vector<model_t>& SMs,
                                     const vector<optional<int>>& s_mapping,
                                     const vector<model_t>& IMs,
                                     const vector<optional<int>>& i_mapping,
                                     const vector<model_t>& scaleMs,
                                     const vector<optional<int>>& scale_mapping,
                                     const model_t& branch_length_model,
                                     const std::vector<int>& like_calcs,
                                     bool allow_compression)
{
    int n_partitions = filename_ranges.size();

    bool variable_alignment = false;
    for(auto& i: i_mapping)
        if (i)
            variable_alignment = true;

    int n_leaves   = n_sequences;
    int n_nodes    = (n_leaves==1)?1:2*n_leaves - 2;
    int n_branches = (n_leaves==1)?0:2*n_leaves - 3;

    set<string> imports;
    imports.insert("Parameters");                        // for Parameters.modifiable
    imports.insert("Bio.Alignment");                         // for Alignment.load_alignment
    imports.insert("Bio.Alphabet");                          // for Bio.Alphabet.dna, etc.
    imports.insert("BAliPhy.ATModel");                   // for ATModel
    imports.insert("BAliPhy.ATModel.DataPartition");     // for Partition
    for(auto& m: SMs)
        add(imports, m.imports);
    for(auto& m: IMs)
        add(imports, m.imports);
    for(auto& m: scaleMs)
        add(imports, m.imports);
    add(imports, branch_length_model.imports);

    std::ostringstream program_file;
    program_file<<"-- Use the program `brittany` (or `hindent`) to indent this file for readability\n";
    program_file<<"module Main where";
    for(auto& mod: imports)
        program_file<<"\nimport "<<mod;
    program_file <<"\nimport qualified Data.Map as Map"; // for Map.fromList

    // F1. Substitution models
    for(int i=0;i<SMs.size();i++)
        program_file<<"\n\nsample_smodel_"<<i+1<<" = "<<SMs[i].expression.print();

    // F2. Indel models
    for(int i=0;i<IMs.size();i++)
        program_file<<"\n\nsample_imodel_"<<i+1<<" = "<<IMs[i].expression.print();

    // F3. Scale models
    for(int i=0; i<scaleMs.size(); i++)
        program_file<<"\n\nsample_scale_"<<i+1<<" = "<<scaleMs[i].expression.print();

    // F4. Branch lengths
    program_file<<"\n\nsample_branch_lengths_1 = "<<branch_length_model.expression.print();

    // F5. Topology
    program_file<<"\n\nsample_topology_1 taxa = uniform_labelled_topology taxa";

    /* --------------------------------------------------------------- */
    do_block program;

    do_block sample_atmodel;
    var imodel_training_var("imodel_training");
    var heat_var("heat");
    var variable_alignment_var("variable_alignment");
    var subst_root_var("subst_root");
    var modifiable("Parameters.modifiable");

    // FIXME: We can't load the alignments to read their names until we know the alphabets!
    // FIXME: Can we load the alignments as SEQUENCES first?
    var taxon_names_var("taxa");

    program.let({
            {imodel_training_var, {modifiable, make_Bool(false)}},
            {heat_var           , {modifiable, 1.0}},
            {variable_alignment_var, {modifiable, make_Bool(variable_alignment)}},
            {subst_root_var,         {modifiable, n_nodes-1}}
        });
    program.empty_stmt();

    // ATModel smodels imodels scales branch_lengths
    // Loggers = [(string,(Maybe a,Loggers)]
    vector<expression_ref> program_loggers;
    // Therefore, we are constructing a list with values [(prefix1,(Just value1, loggers1)), (prefix1, (Just value1, loggers2))

    // P1. Topology
    auto tree_var = var("topology1");
    sample_atmodel.perform(tree_var, {var("sample_topology_1"),taxon_names_var});

    // P2. Branch lengths
    expression_ref branch_lengths = List();
    if (n_branches > 0)
    {
        string prefix = "branch_lengths";
        expression_ref branch_lengths_model = {var("sample_branch_lengths_1"), tree_var};
        auto [x,loggers] = sample_atmodel.bind_model(prefix , branch_lengths_model);
        branch_lengths = x;
    }


    // P3. Branch categories
    var branch_categories("branch_categories");
    sample_atmodel.let(branch_categories, { var("map"), var("modifiable"), {var("replicate"), n_branches, 0} });

    // P4. Scales
    vector<expression_ref> scales;
    for(int i=0; i<scaleMs.size(); i++)
    {
        // FIXME: Ideally we would actually join these models together using a Cons operation and prefix.
        //        This would obviate the need to create a Scale1 (etc) prefix here.
        string prefix = "scale"+convertToString(i+1);

        auto scale_model = var("sample_scale_"+std::to_string(i+1));
        auto scale_var = sample_atmodel.bind_and_log_model(prefix , scale_model, program_loggers, false);
        scales.push_back(scale_var);
    }
    if (auto l = logger("scale", get_list(scales), List()) )
        program_loggers.push_back( l );

    // P5. Distances
    for(int i=0; i < n_partitions; i++)
    {
        string part = std::to_string(i+1);
        int scale_index = *scale_mapping[i];

        // L1. scale_P ...
        expression_ref scale = scales[scale_index];

        // L2. distances_P = map (*scale_P) branch_lengths
        var distances("distances_part"+part);
        {
            var x("x");
            sample_atmodel.let(distances, {var("listArray'"),{var("map"), lambda_quantify(x,{var("*"),scale,x}), branch_lengths}});
        }
    }
    sample_atmodel.empty_stmt();

    // P6. Substitution models
    vector<expression_ref> smodels;
    for(int i=0;i<SMs.size();i++)
    {
        string prefix = "S" + convertToString(i+1);

        optional<int> first_partition;
        for(int j=0;j<s_mapping.size();j++)
            if (s_mapping[j] and *s_mapping[j] == i)
                first_partition = j;

        expression_ref smodel = var("sample_smodel_"+std::to_string(i+1));
        smodel = {var("set_alphabet'"), alphabet_exps[*first_partition], smodel};

        auto smodel_var = sample_atmodel.bind_and_log_model(prefix , smodel, program_loggers, false);
        auto smodel_var2 = var("smodel_"+std::to_string(i+1));
        sample_atmodel.let(smodel_var2, {smodel_var, branch_categories});
        smodels.push_back(smodel_var2);
    }


    // P2. Indel models
    vector<expression_ref> imodels;
    for(int i=0;i<IMs.size();i++)
    {
        string prefix = "I" + convertToString(i+1);
        expression_ref imodel = var("sample_imodel_"+std::to_string(i+1));
        auto imodel_var = sample_atmodel.bind_and_log_model(prefix, imodel, program_loggers, false);

        var imodel_var2("imodel_"+std::to_string(i+1));
        sample_atmodel.let(imodel_var2, {imodel_var, tree_var, heat_var, imodel_training_var});
        imodels.push_back(imodel_var2);
    }
    sample_atmodel.empty_stmt();


    // FIXME: We aren't using the ranges to select columns!

    // FIXME: We are loading files multiple times!

    // P6. Create objects for data partitions
    vector<expression_ref> partitions;
    vector<expression_ref> leaf_sequences;

    // Emit filenames var
    var filenames_var("filenames");
    {
        set<string> filenames;
        for(auto& [filename,_]: filename_ranges)
            filenames.insert(filename);
        vector<expression_ref> filenames_;
        for(auto& filename: filenames)
            filenames_.push_back(String(filename));
        program.let(filenames_var,get_list(filenames_));
    }

    var filename_to_seqs("filename_to_seqs");
    {
        expression_ref body = Tuple(var("filename"),{var("load_sequences"),var("filename")});
        vector<expression_ref> quals = { PatQual(var("filename"),filenames_var) };
        program.let(filename_to_seqs,{var("Map.fromList"), list_comprehension( body, quals)});
    }
    program.empty_stmt();

    for(int i=0; i < n_partitions; i++)
    {
        string part = std::to_string(i+1);
        int scale_index = *scale_mapping[i];
        int smodel_index = *s_mapping[i];
        auto imodel_index = i_mapping[i];

        // L0. scale_P ...
        var alphabet_var("alphabet_part"+part);
        program.let(alphabet_var, alphabet_exps[i]);
        var alignment_var("alignment_part"+part);
        expression_ref loaded_sequences = {var("Map.!"),filename_to_seqs,String(filename_ranges[i].first)};
        if (not filename_ranges[i].second.empty())
            loaded_sequences = {var("select_range"), String(filename_ranges[i].second), loaded_sequences};
        expression_ref loaded_alignment = {var("alignment_from_sequences"), alphabet_var, loaded_sequences};

        if (i==0)
        {
            program.let(alignment_var, loaded_alignment);
            program.let(taxon_names_var, {var("Bio.Alignment.sequence_names"),alignment_var});
        }
        else
        {
            // This is using EVector String instead of [[Char]] for the sequence names!
            if (like_calcs[i] == 0)
                program.let(alignment_var, {var("reorder_alignment"),taxon_names_var,loaded_alignment});
            else
                program.let(alignment_var, loaded_alignment);
        }

        // L1. scale_P ...
        expression_ref scale = scales[scale_index];

        // L3. let smodel_P = ...
        expression_ref smodel = smodels[smodel_index];

        //---------------------------------------------------------------------------
        var compressed_alignment_var("compressed_alignment_part"+part);
        var counts_var("counts_part"+part);
        if (allow_compression and (not i_mapping[i]))
        {
            program.let(compressed_alignment_var, {var("compress_alignment"), alignment_var});
        }
        //---------------------------------------------------------------------------//

        var sequences_var("sequences_part"+part);
        var leaf_sequences_var("leaf_sequences_part"+part);
        if (like_calcs[i] == 0 or n_nodes == 1)
        {
            program.let(sequences_var, {var("sequences_from_alignment"),alignment_var});
            program.let(leaf_sequences_var, {var("listArray'"),{var("take"),n_leaves,sequences_var}});
            leaf_sequences.push_back(leaf_sequences_var);
        }
        else
            leaf_sequences.push_back(var("Nothing"));
        program.empty_stmt();

        // L4. let imodel_P = Nothing | Just
        expression_ref maybe_imodel = var("Nothing");
        expression_ref maybe_hmms   = var("Nothing");

        // Sample the alignment
        var distances("distances_part"+part);
        var alignment_on_tree("alignment_on_tree_part"+part);
        if (imodel_index)
        {
            expression_ref imodel = imodels[*imodel_index];

            var branch_hmms("branch_hmms_part"+part);
            sample_atmodel.let(branch_hmms, {var("branch_hmms"), imodel, distances, n_branches});
            maybe_imodel = {var("Just"), imodel};
            maybe_hmms   = {var("Just"), branch_hmms};

            var leaf_sequence_lengths("leaf_sequence_lengths_part"+part);
            sample_atmodel.let(leaf_sequence_lengths, {var("get_sequence_lengths"),leaf_sequences_var});

            // alignment_on_tree <- sample $ random_alignment tree hmms model leaf_seqs_array p->my_variable_alignment()
            sample_atmodel.perform(alignment_on_tree, {var("random_alignment"), tree_var, branch_hmms, imodel, leaf_sequence_lengths, variable_alignment_var});
            sample_atmodel.empty_stmt();
        }
        else
        {
            if (like_calcs[i] == 0)
            {
                // P5.II Create modifiables for pairwise alignments
                expression_ref initial_alignments_exp = {var("pairwise_alignments_from_matrix"), compressed_alignment_var, tree_var};

                var pairwise_as("pairwise_as_part"+part);
                sample_atmodel.let(pairwise_as,  {var("force_struct"),{var("listArray'"), {var("map"),var("modifiable"),initial_alignments_exp}}});

                // R4. Register sequence length methods
                auto seq_lengths = expression_ref{{var("listArray'"),{var("compute_sequence_lengths"), leaf_sequences_var, tree_var, pairwise_as}}};

                sample_atmodel.let(alignment_on_tree, {var("AlignmentOnTree"), tree_var, n_nodes, seq_lengths, pairwise_as});
                sample_atmodel.empty_stmt();
            }
            else if (like_calcs[i] == 1)
            {
                alignment_on_tree = var("Nothing");
            }
        }

        // FIXME - to make an AT *model* we probably need to remove the data from here.
        partitions.push_back({var("Partition"), smodel, maybe_imodel, distances, tree_var, alignment_on_tree, maybe_hmms});
    }

    // FIXME - we need to observe the likelihoods for each partition here.
    //       - Current we are creating data_partition_constants::likelihood_index in data_partition_constants()

    //       - In tests/prob_prog/infer_tree/1/infer.hs, we are doing this:
    //  subst_like_on_tree topology root as alphabet smodel ts scale branch_cats seqs = substitution_likelihood topology root seqs' as' alphabet ps f
    //      where f = weighted_frequency_matrix smodel
    //      ps = transition_p_index topology smodel branch_cats ds
    //      ds = listArray' $ map (scale*) ts
    //      as' = listArray' as
    //      seqs' = listArray' seqs
    // observe (ctmc_on_tree topology root as alphabet smodel ts scale branch_cats) seqs


    sample_atmodel.finish_return(
        Tuple(
            {var("ATModel"), tree_var, get_list(smodels), get_list(scales), branch_lengths, branch_categories, get_list(partitions)},
            get_list(program_loggers))
        );
    
    if (log_verbose >= 4)
        std::cout<<sample_atmodel.get_expression()<<std::endl;

    program.let(var("leaf_sequences"),get_list(leaf_sequences));
    program.empty_stmt();
    program.perform(Tuple(var("atmodel"),var("loggers")), {var("$"),var("random"),sample_atmodel.get_expression()});
    for(int i=0; i < n_partitions; i++)
    {
        program.empty_stmt();
        string part = std::to_string(i+1);
        int likelihood_calculator = like_calcs[i];

        // L0. scale_P ...
        var alphabet_var("alphabet_part"+part);
        var transition_ps("transition_ps_part"+part);
        var cls_var("cls_part"+part);
        var ancestral_sequences_var("ancestral_sequences_part"+part);
        var likelihood_var("likelihood_part"+part);
        var leaf_sequences_var("leaf_sequences_part"+part);
        var compressed_alignment_var("compressed_alignment_part"+part);
        var counts_var("counts_part"+part);

        var partition("part"+part);
        program.let(partition,{var("!!"),{var("partitions"),var("atmodel")},i});

        if (n_nodes > 2 and likelihood_calculator == 0)
        {
            program.let(Tuple(transition_ps, cls_var, ancestral_sequences_var, likelihood_var),
                        {var("observe_partition_type_0"),partition,leaf_sequences_var,subst_root_var});
            continue;
        }
        else if (n_nodes > 2 and not i_mapping[i])
        {
            assert(likelihood_calculator == 1);

            program.let(Tuple(transition_ps, cls_var, ancestral_sequences_var, likelihood_var),
                        {var("observe_partition_type_1"),partition,compressed_alignment_var,subst_root_var});
            continue;
        }

        var tree_var("tree_part"+part);
        program.let(tree_var, {var("BAliPhy.ATModel.DataPartition.get_tree"),partition});

        auto alignment_on_tree = expression_ref{var("BAliPhy.ATModel.DataPartition.get_alignment"),partition};
        var as("as_part"+part);
        program.let(as, {var("pairwise_alignments"), alignment_on_tree});

        var smodel("smodel_part"+part);
        program.let(smodel,{var("BAliPhy.ATModel.DataPartition.smodel"),partition});
        var smap("smap_part"+part);
        program.let(smap,{var("SModel.get_smap"),smodel});

        var distances("distances_part"+part);
        program.let(distances,{var("BAliPhy.ATModel.DataPartition.get_branch_lengths"),partition});

        var smodel_on_tree("smodel_on_tree_part"+part);
        program.let(smodel_on_tree,{var("SModel.SingleBranchLengthModel"), tree_var, distances, smodel});

        auto f = expression_ref{var("weighted_frequency_matrix"), smodel};

        program.let(transition_ps, {var("transition_p_index"), smodel_on_tree});

        if (n_nodes == 1)
        {
            expression_ref seq = {var("!"),leaf_sequences_var, 0};
            program.let(cls_var, 0);
            program.let(ancestral_sequences_var, 0);
            program.let(likelihood_var, {var("peel_likelihood_1"), seq, alphabet_var, f});
            
        }
        else if (likelihood_calculator == 0)
        {
            // Create and set conditional likelihoods for each branch
            program.let(cls_var, {var("cached_conditional_likelihoods"), tree_var, leaf_sequences_var, as, alphabet_var, transition_ps, f, smap});

            // FIXME: broken for fixed alignments of 2 sequences.
            if (n_nodes > 2)
                program.let(likelihood_var, {var("peel_likelihood"), tree_var, cls_var, as, f, subst_root_var});
        }
        else if (likelihood_calculator == 1)
        {
            // Create and set conditional likelihoods for each branch
            program.let(cls_var,{var("cached_conditional_likelihoods_SEV"),tree_var,leaf_sequences_var, alphabet_var ,transition_ps,f, compressed_alignment_var, smap});  

            // FIXME: broken for fixed alignments of 2 sequences.
            if (n_nodes > 2)
                program.let(likelihood_var,{var("peel_likelihood_SEV"), tree_var, cls_var, f, subst_root_var, counts_var});
        }

        if (n_nodes == 2)
        {
            // We probably want the cls?
            expression_ref seq1 = {var("!"), leaf_sequences_var, 0};
            expression_ref seq2 = {var("!"), leaf_sequences_var, 1};
            expression_ref A = {var("!"), as, 0};
            expression_ref P = {var("!"), transition_ps, 0};
            program.let(likelihood_var,{var("peel_likelihood_2"), seq1, seq2, alphabet_var, A, P, f});
            program.let(ancestral_sequences_var, 0);
        }
    }
    program.empty_stmt();

    vector<expression_ref> transition_ps;
    vector<expression_ref> cond_likes;
    vector<expression_ref> anc_seqs;
    vector<expression_ref> likelihoods;
    for(int i=0; i < n_partitions; i++)
    {
        string part = std::to_string(i+1);
        transition_ps.push_back(var("transition_ps_part"+part));
        cond_likes.push_back(var("cls_part"+part));
        anc_seqs.push_back(var("ancestral_sequences_part"+part));
        likelihoods.push_back(var("likelihood_part"+part));
    }

    program.let(var("transition_ps"),get_list(transition_ps));
    program.let(var("cond_likes"),get_list(cond_likes));
    program.let(var("anc_seqs"),get_list(anc_seqs));
    program.let(var("likelihoods"),get_list(likelihoods));
    program.empty_stmt();
    program.perform({var("observe"),{var("fake_dist"),var("likelihoods")},var("leaf_sequences")});
    program.empty_stmt();
    program.finish_return(Tuple({var("ATModelExport"),
                                 var("atmodel"),
                                 var("transition_ps"),
                                 var("cond_likes"),
                                 var("anc_seqs"),
                                 var("likelihoods"),
                                 var("leaf_sequences"),
                                 imodel_training_var,
                                 heat_var,
                                 variable_alignment_var,
                                 subst_root_var,
                                 taxon_names_var},

                           var("loggers")));
    program_file<<"\n\nmain = "<<program.get_expression().print();

    return program_file.str();
}

template<typename T>
T load_value(const Model::key_map_t& keys, const std::string& key, const T& t)
{
    auto loc = keys.find(key);
    if (loc != keys.end())
        return loc->second;
    else
        return t;
}

Program gen_atmodel_program(const std::shared_ptr<module_loader>& L,
                            const Model::key_map_t& k,
                            const fs::path& program_filename,
                            const vector<expression_ref>& alphabet_exps,
                            const vector<pair<string,string>>& filename_ranges,
                            int n_leaves,
                            const vector<model_t>& SMs,
                            const vector<optional<int>>& s_mapping,
                            const vector<model_t>& IMs,
                            const vector<optional<int>>& i_mapping,
                            const vector<model_t>& scaleMs,
                            const vector<optional<int>>& scale_mapping,
                            const model_t& branch_length_model,
                            const std::vector<int>& like_calcs)
{
    // FIXME! Make likelihood_calculators for 1- and 2-sequence alignments handle compressed alignments.
    bool allow_compression = load_value(k, "site-compression", n_leaves) and not load_value(k, "write-fixed-alignments",false);

    {
        checked_ofstream program_file(program_filename.string());
        program_file<<generate_atmodel_program(n_leaves,
                                               alphabet_exps,
                                               filename_ranges,
                                               SMs, s_mapping,
                                               IMs, i_mapping,
                                               scaleMs, scale_mapping,
                                               branch_length_model,
                                               like_calcs,
                                               allow_compression);
    }

    Program P(L, Program::exe_type::log_pair);
    auto m = P.get_module_loader()->load_module_from_file(program_filename.string());
    P.add(m);
    P.main = "Main.main";
    return P;
}

// scaleMs
// 

Parameters::Parameters(const Program& prog,
                       const key_map_t& keys,
                       const vector<alignment>& A,
                       const vector<pair<string,string>>& filename_ranges,
                       const SequenceTree& ttt,
                       const vector<optional<int>>& s_mapping,
                       const vector<optional<int>>& i_mapping,
                       const vector<optional<int>>& scale_mapping,
                       const std::vector<int>& like_calcs)
:Model(prog, keys),
 PC(new parameters_constants(filename_ranges.size(), ttt, s_mapping, i_mapping, scale_mapping)),
 variable_alignment_( n_imodels() > 0 ),
 updown(-1)
{
    bool allow_compression = load_value("site-compression", ttt.n_nodes() > 2) and not load_value("write-fixed-alignments",false);
    const int n_partitions = filename_ranges.size();
    PC->atmodel_export = *memory()->program_result_head;

    PC->constants.push_back(-1);

    /* ---------------- compress alignments -------------------------- */

    vector<optional<compressed_alignment>> compressed_alignments(n_partitions);
    vector<const alignment*> alignments(n_partitions);
    for(int i=0;i<n_partitions;i++)
    {
        if (not imodel_index_for_partition(i) and allow_compression)
        {
            compressed_alignments[i] = compress_alignment(A[i], ttt);
            alignments[i] = &compressed_alignments[i]->compressed;
            std::cerr<<"Partition #"<<i+1<<": "<<A[i].length()<<" columns -> "<<alignments[i]->length()<<" unique patterns.\n";
        }
        else
            alignments[i] = &A[i];
    }

    /* ---------------- Set up the tree ------------------------------ */
    branches_from_affected_node.resize(ttt.n_nodes());

    PC->atmodel = add_compute_expression({var("BAliPhy.ATModel.get_atmodel"), my_atmodel_export()});

    PC->partition_transition_ps = add_compute_expression({var("BAliPhy.ATModel.get_all_transition_ps"),my_atmodel_export()});
    PC->partition_cond_likes = add_compute_expression({var("BAliPhy.ATModel.get_all_cond_likes"),my_atmodel_export()});
    PC->partition_likelihoods = add_compute_expression({var("BAliPhy.ATModel.get_all_likelihoods"),my_atmodel_export()});
    PC->partition_ancestral_seqs = add_compute_expression({var("BAliPhy.ATModel.get_all_ancestral_sequences"),my_atmodel_export()});

    // 1. Get the leaf labels out of the machine.  These should be based on the leaf sequences alignment for partition 1.
    // FIXME, if partition 1 has ancestral sequences, we will do the wrong thing here, even if we pass in a tree.
    expression_ref sequence_names_exp = {var("BAliPhy.ATModel.sequence_names"), my_atmodel_export()};
    sequence_names_exp = {var("Data.List.map"),var("Foreign.Vector.pack_cpp_string"),sequence_names_exp};
    sequence_names_exp = {var("Foreign.Vector.list_to_vector"),sequence_names_exp};
    PC->sequence_names     = add_compute_expression(sequence_names_exp);

    // FIXME: make the program represent these as [String], and translate it to an EVector just for export purposes?
    auto sequence_names = PC->sequence_names.get_value(*this).as_<EVector>();
    vector<string> labels;
    for(auto& name: sequence_names)
        labels.push_back(name.as_<String>());
    auto leaf_labels = labels;

    // FIXME: maybe do this inside the program?
    for(int i=sequence_names.size();i<2*sequence_names.size()-2;i++)
        labels.push_back("A"+std::to_string(i));

    // 2. Set up the TreeInterface mapping to the tree inside the machine

    int tree_index = add_compute_expression( {var("BAliPhy.ATModel.tree"), my_atmodel()} );
    TC = new tree_constants(this, labels, tree_index);

    // 3. Remap the input tree to have the same label_string <-> node-number mapping FOR LEAVES.
    // FIXME: We need ALL the nodes to have the right label_string <-> node-number mapping in
    //           order to handle alignments with internal-node sequences.
    auto tt = ttt;
    assert(tt.n_leaves() == leaf_labels.size());
    remap_T_leaf_indices(tt, leaf_labels);
    for(int i=0;i<tt.n_leaves();i++)
        assert(tt.get_label(i) == labels[i]);

    // 4. Load the specified tree TOPOLOGY into the machine. (branch lengths are loaded later).
    t().read_tree(tt);

    PC->imodel_training    = get_param(*this, evaluate_expression({var("Parameters.maybe_modifiable_structure"),{var("BAliPhy.ATModel.imodel_training"), my_atmodel_export()}}));
    PC->heat               = get_param(*this, evaluate_expression({var("Parameters.maybe_modifiable_structure"),{var("BAliPhy.ATModel.heat"), my_atmodel_export()}}));
    PC->variable_alignment = get_param(*this, evaluate_expression({var("Parameters.maybe_modifiable_structure"),{var("BAliPhy.ATModel.variable_alignment"), my_atmodel_export()}}));
    PC->subst_root         = get_param(*this, evaluate_expression({var("Parameters.maybe_modifiable_structure"),{var("BAliPhy.ATModel.subst_root"), my_atmodel_export()}}));

    /* --------------------------------------------------------------- */

    // R1. Register branch lengths
    TC->register_branch_lengths(this, {var("BAliPhy.ATModel.branch_lengths"),my_atmodel()});

    param scales_list = add_compute_expression( {var("BAliPhy.ATModel.scales"),my_atmodel()} );

    // R2. Register individual scales
    PC->branch_scales_ = get_params_from_list(this, scales_list.ref(*this));

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

        auto b2 = (vector<int>) evaluate_expression( {var("Foreign.Vector.list_to_vector"),{var("Tree.edgesBeforeEdge"),my_tree(),b}}).as_<EVector>();
        assert(b2.size() == branch_list_.size());
        for( int i: branch_list_)
            assert(includes(b2,i));
    }
#endif

    // R3. Register methods for each of the individual substitution models
    int n_smodels = get_num_models(s_mapping);
    for(int i=0;i<n_smodels;i++)
    {
        expression_ref smodel = {var("Data.List.!!"),{var("BAliPhy.ATModel.smodels"), my_atmodel()}, i};

        PC->SModels.push_back( smodel_methods( smodel, *this) );
    }

    // don't constrain any branch lengths
    for(int b=0;b<PC->TC.n_branches();b++)
        PC->TC.branch(b).set_length(-1);

    // R5. Register branch categories
    PC->branch_categories = get_params_from_list(this, {var("BAliPhy.ATModel.branch_categories"), my_atmodel()}, tt.n_branches());

    // create data partitions

    assert(like_calcs.size() == n_partitions);
    for(int i=0;i<n_partitions;i++)
    {
        if (compressed_alignments[i])
        {
            // construct compressed alignment, counts, and mapping
            auto& [AA, counts, mapping] = *compressed_alignments[i];
            PC->DPC.emplace_back(this, i, AA.get_alphabet(), like_calcs[i]);
            if (like_calcs[i] == 0)
                get_data_partition(i).set_alignment(AA);
        }
        else
        {
            auto counts = vector<int>(A[i].length(), 1);
            PC->DPC.emplace_back(this, i, A[i].get_alphabet(), like_calcs[i]);
            if (like_calcs[i] == 0)
                get_data_partition(i).set_alignment(A[i]);
        }
    }

    // FIXME: We currently need this to make sure all parameters get instantiated before we finish the constructor.
    probability();

    // Load the specified tree BRANCH LENGTHS into the machine.
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
