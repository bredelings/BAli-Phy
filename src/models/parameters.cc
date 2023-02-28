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
#include "models/TreeInterface.H"
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
#include "computation/machine/gcobject.H"

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
namespace fs = std::filesystem;

/*
 * \todo: List of things to do to clean up programs.
 *
 * See list in models/parameters.C 
 *
 * 1. Make sure we don't read alignments with ^@ characters in the sequences! (?)
 *
 * 2. Avoid recomputing likelihoods when recomputed branch lengths are unchanged.
 *    - Cast to single precision, and then only recompute when that changes.
 *
 * 3. Compare the monadic interface with Acar's interface.
 *
 * 4.
 *     - Handle sequences with lengths not divisible by 3.
 *     - Handle loading alignments with codons not together.
 *     - Could we actually handle all SEEN codon triplets?
 *
 * 5. Store alignments in a more sparse format?
 *
 * 6. Rename reg_heap -> something more descriptive/attractive.
 *
 */

/// Is the alignment allowed to vary?
bool data_partition::variable_alignment() const
{
    return has_IModel() and P->variable_alignment();
}

const data_partition_constants& data_partition::DPC() const
{
    return P->PC->DPC[partition_index];
}

std::shared_ptr<const alphabet> data_partition::get_alphabet() const
{
    return property(9).value().as_<PtrBox<alphabet>>();
}

alignment data_partition::A() const
{
    matrix<int> M;
    if (t().n_nodes() == 1)
    {
        M = matrix<int>(get_sequence(0)->size(),1);
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

    return get_alignment(*get_alphabet(), all_seqs(), leaf_sequences(), M);
}

ParametersTreeInterface data_partition::t() const
{
    return P->t();
}


double data_partition::get_beta() const
{
    return P->get_beta();
}

int data_partition::subst_root() const {
    return property(0).value().as_int();
}

vector< vector<int> > data_partition::leaf_sequences() const
{
    vector< vector<int> > sequences;
    for(int i=0;i<t().n_leaves();i++)
        sequences.push_back( (vector<int>)(*get_sequence(i)) );
    return sequences;
}

string data_partition::label(int i) const
{
    auto labels = property(5);

    if (auto label = labels[i].value().as_<EMaybe>())
        return label->as_<String>();
    else
        return "A"+std::to_string(i);
}

vector< sequence> data_partition::all_seqs() const
{
    vector< sequence > sequences ( t().n_nodes() );

    for(int i=0;i<sequences.size();i++)
        sequences[i].name = label(i);

    return sequences;
}

bool data_partition::has_IModel() const
{
    return (bool)DPC().alignment_properties_reg;
}

object_ptr<const EVector> data_partition::get_sequence(int i) const
{
    return property(8)[i].value().as_ptr_to<EVector>();
}

object_ptr<const EVector> data_partition::transition_P(int b) const
{
    b = t().undirected(b);
    assert(b >= 0 and b < t().n_branches());
    return property(1)[b].value().as_ptr_to<EVector>();
}

context_ptr data_partition::properties() const
{
    return context_ptr(*P, DPC().properties_reg);
}

context_ptr data_partition::property(int i) const
{
    return properties()[i];
}

context_ptr data_partition::alignment_properties() const
{
    assert(DPC().alignment_properties_reg);
    return context_ptr(*P, *DPC().alignment_properties_reg);
}

context_ptr data_partition::alignment_property(int i) const
{
    return alignment_properties()[i];
}

int data_partition::n_base_models() const
{
    return property(12).value().as_int();
}

int data_partition::n_states() const
{
    return property(11).value().as_int();
}

object_ptr<const Box<Matrix>> data_partition::WeightedFrequencyMatrix() const
{
    return property(6).value().as_ptr_to<Box<Matrix>>();
}

object_ptr<const EVector> data_partition::state_letters() const
{
    return property(7).value().as_ptr_to<EVector>();
}

const indel::PairHMM& data_partition::get_branch_HMM(int b) const
{
    assert(variable_alignment());

    b = t().undirected(b);

    return alignment_property(1)[b].value().as_<indel::PairHMM>();
}

vector<indel::PairHMM> data_partition::get_branch_HMMs(const vector<int>& br) const
{
    vector<indel::PairHMM> HMMs(br.size());

    for(int i=0;i<HMMs.size();i++)
        HMMs[i] = get_branch_HMM(br[i]);

    return HMMs;
}

log_double_t data_partition::sequence_length_pr(int n) const
{
    return alignment_property(5)[n].value().as_log_double();
}

int data_partition::seqlength(int n) const
{
    if (n < t().n_leaves())
        return get_sequence(n)->size();

    assert(has_pairwise_alignments());

    return alignment_property(4)[n].value().as_int();
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

    alignment_property(3)[b].set_value( 0 );
    alignment_property(3)[B].set_value( 0 );
    assert(pairwise_alignment_is_unset(b));
}

/// Set the pairwise alignment value, but don't mark the alignment & sequence lengths as changed.
void mutable_data_partition::set_pairwise_alignment(int b, const pairwise_alignment_t& pi)
{
    assert(likelihood_calculator() == 0);
    int B = t().reverse(b);
    assert(pairwise_alignment_is_unset(b) or (get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped()));
    alignment_property(3)[b].set_value( new Box<pairwise_alignment_t>(pi) );
    alignment_property(3)[B].set_value( new Box<pairwise_alignment_t>(pi.flipped()));
    assert(get_pairwise_alignment(b) == get_pairwise_alignment(B).flipped());
}

expression_ref data_partition::get_pairwise_alignment_(int b) const
{
    assert(likelihood_calculator() == 0);
    return alignment_property(3)[b].value();
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

    return alignment_property(0).value().as_log_double();
}

object_ptr<const Likelihood_Cache_Branch> data_partition::cache(int b) const
{
    return property(2)[b].value().as_ptr_to<Likelihood_Cache_Branch>();
}

log_double_t data_partition::likelihood() const 
{
    substitution::total_likelihood++;
    return property(4).value().as_log_double();
}

EVector data_partition::ancestral_sequences() const
{
    return property(3).value().as_<EVector>();
}

expression_ref data_partition::ancestral_sequence_alignment() const
{
    return property(3).value();
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

data_partition_constants::data_partition_constants(context_ref& C, const TreeInterface& t, int r_data)
{
    // -------------- Extract method indices from dist properties -----------------//
    auto to_var = C.out_edges_to_var(r_data);
    if (to_var->size() > 1)
        throw myexception()<<"Some partitions are identical!";

    int s_sequences = *to_var->begin();
    auto properties = C.dist_properties(s_sequences);
    properties_reg = *properties->get("properties");

    auto dist_type = *C.dist_type(s_sequences);

    if (dist_type == "ctmc_on_tree")
    {
        likelihood_calculator = 0;

        // Extract pairwise alignments from data partition
        auto in_edges = C.in_edges_to_dist(s_sequences);
        int r_alignment = *in_edges->get("alignment");
        auto alignment_on_tree = reg_var( r_alignment );

        // Add method indices for calculating branch HMMs and alignment prior
        int s_alignment = *C.out_edges_to_var( r_alignment )->begin();

        auto A_properties = C.dist_properties(s_alignment);
        alignment_properties_reg = A_properties->get("properties");
    }
    else if (dist_type == "ctmc_on_tree_fixed_A")
    {
        likelihood_calculator = 1;
    }
    else
        throw myexception()<<"data_partition_constant: I don't recognize data from distribution '"<<dist_type<<"'";
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

    return {tree_con, node_branches_array, branch_nodes_array, T.n_nodes()};
}

bool Parameters::variable_alignment() const
{
    return variable_alignment_;
}

data_partition Parameters::get_data_partition(int i) const
{
    return data_partition(this,i);
}

mutable_data_partition Parameters::get_data_partition(int i)
{
    return mutable_data_partition(this,i);
}

ParametersTreeInterface Parameters::t() const
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
    auto T = t();
    ::NNI(T, br1, br2);
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
    if (*get_alphabet() != A.get_alphabet())
        throw myexception()<<"Can't set alignment with alphabet '"<<A.get_alphabet().print()<<"' in partition with alphabet '"<<get_alphabet()->name<<"'";

    auto T = t();

    auto labels = P->get_labels();

    // 2. Reorder and maybe extend the alignment
    auto AA = link_A(A, labels, T);

    // 3. Check that the alignment doesn't disagree with existing leaf sequences lengths!
    for(auto node: T.leaf_nodes())
    {
        assert(AA.seq(node).name == labels[node]);
        if (AA.seqlength(node) != seqlength(node))
            throw myexception()<<"partition "<<partition_index+1<<", sequence "<<AA.seq(node).name<<": alignment sequence length "<<AA.seqlength(node)<<" does not match required sequence length "<<seqlength(node);
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

// b1 and b2 point outwards, away from the other subtrees.
// The (possibly) disconnected subtree is the sibling of b1.
void Parameters::NNI_discard_alignment(int b1, int b2)
{
    if (not variable_alignment())
    {
        NNI(b1,b2);
        return;
    }

#ifndef NDEBUG
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
#endif

    // 3. Perform NNI
    exchange_subtrees(b1, b2);  // alter tree
#ifndef NDEBUG
    std::swap(nodes[0],nodes[2]); // alter nodes
    std::swap(b04, b25);

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
            dp.unset_pairwise_alignment(b04);
            dp.unset_pairwise_alignment(b14);
            dp.unset_pairwise_alignment(b25);
            dp.unset_pairwise_alignment(b35);
            dp.unset_pairwise_alignment(b45);
        }
    }
#endif
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

log_double_t Parameters::prior_alignment() const 
{
    log_double_t Pr = 1;

    for(int i=0;i<n_data_partitions();i++)
        Pr *= get_data_partition(i).prior_alignment();

    return Pr;
}

void Parameters::cache_likelihood_branches() const
{
    // We currently can't cache values except by running the program.
    // Unfortunately, this runs calc_root unnecessarily.
    probability();

    // This WAS more efficient, since it avoided doing a calc_root.
//    auto branches = t().branches_in(subst_root());
//    for(int i=0; i < n_data_partitions();i++)
//        for(auto b: branches)
//            (*this)[i].cache(b);
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
    // What if all the partitions have the SAME subst_root modifiable?
    for(int p=0;p<n_data_partitions();p++)
    {
        auto subst_root = get_data_partition(p).property(0);
        if (auto m = subst_root.modifiable())
        {
            const_cast<context*>(C)->set_modifiable_value(m->get_reg(), node);
        }
    }
}

void Parameters::set_root(int node) const
{
    if (subst_root() != node)
        set_root_(node);
    assert(subst_root() == node);
}

int Parameters::subst_root(int i) const
{
    return get_data_partition(i).subst_root();
}

int Parameters::subst_root() const
{
    return subst_root(0);
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
    return TC->tree_exp;
}

expression_ref Parameters::my_atmodel() const
{
    assert(PC);
    return PC->atmodel.ref(*this);
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

parameters_constants::parameters_constants()
{
}

parameters_constants::parameters_constants(int n_partitions, const SequenceTree& t,
                                           const vector<optional<int>>& s_mapping,
                                           const vector<optional<int>>& i_mapping,
                                           const vector<optional<int>>& scale_mapping)
    :n_scales(num_distinct(scale_mapping))
{
    // check that smodel mapping has correct size.
    if (s_mapping.size() != n_partitions)
        throw myexception()<<"There are "<<n_partitions
                           <<" data partitions, but you mapped smodels onto "
                           <<s_mapping.size();

    int n_smodels = get_num_models(s_mapping);

    // check that we only map existing smodels to data partitions
    for(int i=0;i<s_mapping.size();i++) {
        int m = *s_mapping[i];
        if (m >= n_smodels)
            throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
                               <<" because there are only "<<n_smodels<<" smodels.";
    }
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
        return  var("dna");
    else if (a.name == "RNA")
        return var("rna");
    else if (a.name == "Amino-Acids")
        return var("aa");
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

string maybe_emit_code(map<string,string>& code_to_name, const string& name, const expression_ref& E)
{
    auto code = print_equals_function(E);
    if (code_to_name.count(code))
        code = code_to_name.at(code);
    else
        code_to_name.insert({code," = " + name});
    return name + code + "\n";
}


var bind_and_log(bool do_log, const var& x, const var& log_x, const string& name, const expression_ref& E, bool is_action, bool has_loggers, do_block& block, vector<expression_ref>& loggers, bool is_referenced=true)
{
    perform_action_simplified(block.get_stmts(), x, log_x, is_referenced, E, is_action, has_loggers);
    maybe_log(loggers, name, do_log?x:expression_ref{}, has_loggers?log_x:expression_ref{});
    return x;
}

var bind_and_log(bool do_log, const string& name, const expression_ref& E, bool is_action, bool has_loggers, do_block& block, vector<expression_ref>& loggers, bool is_referenced=true)
{
    string var_name = name;
    if (var_name.empty() or not std::islower(var_name[0]))
        var_name = "_"+var_name;
    var x(var_name);
    var log_x("log_"+name);
    return bind_and_log(do_log, x, log_x, name, E, is_action, has_loggers, block, loggers, is_referenced);
}


// Given a collection of different functions, check if the code is the same to avoid printing the same code twice.
// Only subscript the functions if there is more than one unique code fragment.
vector<string> print_models(const string& tag, const vector<model_t>& models, std::ostringstream& file)
{
    map<string,int> functions;
    vector<string> function_for_index;
    for(int i=0;i<models.size();i++)
    {
        auto code = print_equals_function(models[i].code.generate());
        if (not functions.count(code))
            functions.insert({code,functions.size()});
    }
    int printed = 0;
    for(int i=0;i<models.size();i++)
    {
        auto code = print_equals_function(models[i].code.generate());
        int index = functions.at(code);
        string name = tag;
        if (functions.size() > 1) name += "_"+std::to_string(index+1);
        function_for_index.push_back(name);
        if (index >= printed)
        {
            file<<name<<" "<<code<<"\n\n";
            printed++;
        }
    }
    return function_for_index;
}

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
                                     const std::vector<int>& like_calcs)
{
    int n_partitions = filename_ranges.size();

    int n_leaves   = n_sequences;
    int n_branches = (n_leaves==1)?0:2*n_leaves - 3;

    set<string> imports;
    imports.insert("Bio.Alignment");                         // for Alignment.load_alignment
    imports.insert("Bio.Alphabet");                          // for Bio.Alphabet.dna, etc.
    imports.insert("BAliPhy.ATModel");                   // for ATModel
    imports.insert("Probability.Distribution.OnTree");   // for ctmc_on_tree{,fixed_A}
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

    // F1. Substitution models
    map<string,string> code_to_name;

    program_file<<"\n\n";

    auto SM_function_for_index = print_models("sample_smodel", SMs, program_file);
    auto IM_function_for_index = print_models("sample_imodel", IMs, program_file);
    vector<string> scaleM_function_for_index;
    if (n_branches > 0)
    {
        scaleM_function_for_index = print_models("sample_scale", scaleMs, program_file);

        // F4. Branch lengths
        program_file<<"sample_branch_lengths"<<print_equals_function(branch_length_model.code.generate())<<"\n";
    }

    // F5. Topology
    program_file<<"\nsample_topology taxa = uniform_labelled_topology taxa\n";

    /* --------------------------------------------------------------- */
    do_block program;

    // FIXME: We can't load the alignments to read their names until we know the alphabets!
    // FIXME: Can we load the alignments as SEQUENCES first?
    var taxon_names_var("taxa");

    // ATModel smodels imodels scales branch_lengths
    // Loggers = [(string,(Maybe a,Loggers)]
    vector<expression_ref> program_loggers;
    // Therefore, we are constructing a list with values [(prefix1,(Just value1, loggers1)), (prefix1, (Just value1, loggers2))

    // M1. Taxa

    if (n_partitions > 0)
    {
        expression_ref sequence_data1 = var("sequence_data");
        if (n_partitions > 1)
            sequence_data1 = {var("!!"),sequence_data1,0};
        program.let(taxon_names_var, {var("map"),var("sequence_name"),sequence_data1});
        program.empty_stmt();
    }

    // M2. Topology
    auto topology_var = var("topology");
    program.perform(topology_var, {var("RanSamplingRate"),0.0,{var("sample_topology"),taxon_names_var}});

    // M3. Branch lengths
    expression_ref branch_lengths = List();
    if (n_branches > 0)
    {
        string var_name = "branch_lengths";
        auto code = branch_length_model.code;
        expression_ref E = {var("sample_"+var_name),topology_var};
        E = {var("RanSamplingRate"),0.0,E};

        branch_lengths = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), program, program_loggers);
    }
    // M4. Branch-length tree
    auto tree_var = var("tree'");
    program.let(tree_var, {var("branch_length_tree"),topology_var,branch_lengths});

    set<string> used_states;
    for(int i=0;i<SMs.size();i++)
        add(used_states, SMs[i].code.used_states);

    // M5. Branch categories
    expression_ref maybe_branch_categories = var("Nothing");
    expression_ref branch_categories;
    if (used_states.count("branch_categories"))
    {
        var branch_categories_var("branch_categories");
        program.let(branch_categories_var, { var("map"), var("modifiable"), {var("replicate"), n_branches, 0} });
        branch_categories = branch_categories_var;
        maybe_branch_categories = {var("Just"),branch_categories_var};
    }

    // M6. Scales
    vector<expression_ref> scales;
    if (n_branches > 0)
    {
        for(int i=0; i<scaleMs.size(); i++)
        {
            // FIXME: Ideally we would actually join these models together using a Cons operation and prefix.
            //        This would obviate the need to create a Scale1 (etc) prefix here.
            string indexsuffix = (scaleMs.size()>1)?convertToString(i+1):"";
            string index_suffix = (scaleMs.size()>1)?"_"+convertToString(i+1):"";
            string var_name = "scale"+indexsuffix;

            auto code = scaleMs[i].code;
            expression_ref E = var(scaleM_function_for_index[i]);

            auto scale_var = bind_and_log(true, var_name, E, code.is_action(), code.has_loggers(), program, program_loggers);

            scales.push_back(scale_var);
        }
        if (auto l = logger("scale", get_list(scales), List()) )
            program_loggers.push_back( l );
    }

    // M7. Substitution models
    vector<expression_ref> smodels;
    for(int i=0;i<SMs.size();i++)
    {
        string prefix = "S" + convertToString(i+1);
        string _suffix = (SMs.size()>1)?"_"+convertToString(i+1):"";
        string suffix = (SMs.size()>1)?convertToString(i+1):"";

        optional<int> first_partition;
        for(int j=0;j<s_mapping.size();j++)
            if (s_mapping[j] and *s_mapping[j] == i)
                first_partition = j;

        auto code = SMs[i].code;

        expression_ref smodel = var(SM_function_for_index[i]);
        for(auto& state_name: code.used_states)
        {
            if (state_name == "alphabet")
                smodel = {smodel, alphabet_exps[*first_partition]};
            else if (state_name == "branch_categories")
            {
                assert(branch_categories);
                smodel = {smodel, branch_categories};
            }
            else
                throw myexception()<<"Don't know how to supply variable for state '"<<state_name<<"'";
        }

        auto smodel_var = var("smodel" + suffix);
        auto log_smodel = var("log_"+smodel_var.name);
        bind_and_log(false, smodel_var, log_smodel, prefix, smodel, code.is_action(), code.has_loggers(), program, program_loggers);
        smodels.push_back(smodel_var);
    }


    // M8. Indel models
    vector<expression_ref> imodels;
    for(int i=0;i<IMs.size();i++)
    {
        string prefix = "I" + convertToString(i+1);
        string _suffix = (IMs.size()>1)?"_"+convertToString(i+1):"";
        string suffix = (IMs.size()>1)?convertToString(i+1):"";

        auto code = IMs[i].code;

        expression_ref imodel = var(IM_function_for_index[i]);
        for(auto& state_name: code.used_states)
        {
            if (state_name == "topology")
                imodel = {imodel, tree_var};
        }

        auto imodel_var = var("imodel" + suffix);
        auto log_imodel = var("log_"+imodel_var.name);
        bind_and_log(false, imodel_var, log_imodel, prefix, imodel, code.is_action(), code.has_loggers(), program, program_loggers);
        imodels.push_back(imodel_var);
    }
    program.empty_stmt();

    for(int i=0; i < n_partitions; i++)
    {
        string part = std::to_string(i+1);
        string part_suffix = (n_partitions>1) ? part : "";
        int scale_index = *scale_mapping[i];
        int smodel_index = *s_mapping[i];
        auto imodel_index = i_mapping[i];
        expression_ref smodel = smodels[smodel_index];
        expression_ref sequence_data_var = var("sequence_data");
        if (n_partitions > 1)
            sequence_data_var = {var("!!"),sequence_data_var,i};

        // Model.Partition.1. tree_part<i> = scale_branch_lengths scale tree
        var branch_dist_tree("tree" + part_suffix);
        if (n_branches > 0)
        {
            expression_ref scale = scales[scale_index];
            program.let(branch_dist_tree, {var("scale_branch_lengths"), scale, tree_var});
        }
        else
            program.let(branch_dist_tree, tree_var);

        // Model.Partition.2. Sample the alignment
        var alignment_on_tree("alignment" + part_suffix);
        if (imodel_index)
        {
            assert(like_calcs[i] == 0);
            expression_ref imodel = imodels[*imodel_index];

            var leaf_sequence_lengths("sequence_lengths" + part_suffix);
            expression_ref alphabet = {var("getAlphabet"),smodel};
            program.let(leaf_sequence_lengths, {var("get_sequence_lengths"), alphabet,  sequence_data_var});
            program.perform(alignment_on_tree, {var("random_alignment"), branch_dist_tree, imodel, leaf_sequence_lengths});
        }

        // Model.Partition.3. Observe the sequence data from the distribution
        expression_ref distribution;
        if (like_calcs[i] == 0)
            distribution = {var("ctmc_on_tree"), branch_dist_tree, alignment_on_tree, smodel};
        else
            distribution = {var("ctmc_on_tree_fixed_A"), branch_dist_tree, smodel};
        program.perform({var("~>"),sequence_data_var,distribution});

        program.empty_stmt();
    }

    var loggers_var("loggers");
    program.let(loggers_var, get_list(program_loggers));
    program.empty_stmt();

    var atmodel_var("atmodel");
    program.let(atmodel_var, {var("ATModel"), tree_var, get_list(scales), maybe_branch_categories});
    program.empty_stmt();

    expression_ref sequence_data_list = var("sequence_data");
    if (n_partitions == 1)
        sequence_data_list = List(sequence_data_list);
    program.finish_return(
        Tuple(
            Tuple(
                var("atmodel"),
                sequence_data_list
                ),
            var("loggers")
            )
        );

    program_file<<"\nmodel sequence_data = "<<program.get_expression().print()<<"\n";

    do_block main;

    var sequence_data_var("sequence_data");
    if (n_partitions == 1)
    {
        auto [filename, range] = filename_ranges[0];
        expression_ref E = {var("load_sequences"),String(filename)};
        if (not range.empty())
            E = {var("<$>"), {var("select_range"),String(range)}, E};
        main.empty_stmt();

        main.perform(sequence_data_var, E);
    }
    else
    {
        // Main.1: Emit let filenames = ...
        var filenames_var("filenames");
        bool any_ranges = false;
        map<string,int> index_for_filename;
        {
            vector<expression_ref> filenames_;
            for(auto& [filename,range]: filename_ranges)
            {
                if (not index_for_filename.count(filename))
                {
                    index_for_filename.insert({filename,filenames_.size()});
                    filenames_.push_back(String(filename));
                }
                if (not range.empty())
                    any_ranges = true;
            }
            main.let(filenames_var,get_list(filenames_));
        }

        if (index_for_filename.size() == n_partitions and not any_ranges)
            main.perform(sequence_data_var,{var("mapM"), var("load_sequences"), filenames_var});
        else
        {
            // Main.2: Emit let filenames_to_seqs = ...
            var filename_to_seqs("seqs");
            {
                main.perform(filename_to_seqs,{var("mapM"), var("load_sequences"), filenames_var});
            }
            main.empty_stmt();

            // Main.3. Emit let sequence_data<n> = 
            vector<expression_ref> sequence_data;
            for(int i=0;i<n_partitions;i++)
            {
                string part = std::to_string(i+1);
                var sequence_data_var("sequence_data"+part);
                int index = index_for_filename.at( filename_ranges[i].first );
                expression_ref loaded_sequences = {var("!!"),filename_to_seqs,index};
                if (not filename_ranges[i].second.empty())
                    loaded_sequences = {var("select_range"), String(filename_ranges[i].second), loaded_sequences};
                main.let(sequence_data_var, loaded_sequences);
                sequence_data.push_back(sequence_data_var);
                main.empty_stmt();
            }

            // Main.4. Emit let sequence_data = ...
            var sequence_data_var("sequence_data");
            main.let(sequence_data_var, get_list(sequence_data));
            main.empty_stmt();
        }
    }

    // Main.5. Emit mcmc $ model sequence_data
    main.perform({var("$"),var("mcmc"),{var("model"),sequence_data_var}});

    program_file<<"\nmain = "<<main.get_expression().print()<<"\n";

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
                            const Model::key_map_t&,
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
    {
        checked_ofstream program_file(program_filename);
        program_file<<generate_atmodel_program(n_leaves,
                                               alphabet_exps,
                                               filename_ranges,
                                               SMs, s_mapping,
                                               IMs, i_mapping,
                                               scaleMs, scale_mapping,
                                               branch_length_model,
                                               like_calcs);
    }

    Program P(L, Program::exe_type::log_pair);
    auto m = P.get_module_loader()->load_module_from_file(program_filename);
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
                       const vector<optional<int>>& scale_mapping)
:Model(prog, keys),
 PC(new parameters_constants(filename_ranges.size(), ttt, s_mapping, i_mapping, scale_mapping)),
 variable_alignment_( false ),
 updown(-1)
{
    const int n_partitions = filename_ranges.size();
    int result_head = *memory()->program_result_head;
    param atmodel_plus_partitions = result_head;
    PC->atmodel = add_compute_expression({var("Data.Tuple.fst"),atmodel_plus_partitions.ref(*this)});
    
    context_ptr program_result(*this, memory()->reg_for_head(result_head));
    auto sequence_data = program_result[1].list_elements();

    vector<int> partition_sampling_events;
    optional<int> r_prop0;
    for(int i=0;i<sequence_data.size();i++)
    {
        auto& sequences = sequence_data[i];
        int r = sequences.get_reg();

        auto& to_var = memory()->out_edges_to_var.at(r);
        if (to_var.size() > 1)
            throw myexception()<<"Some partitions are identical!";

        int s_sequences = *to_var.begin();
        auto properties = dist_properties(s_sequences);
        if (i==0)
        {
            r_prop0 = properties->get("properties");
        }
    }

    /* ---------------- Set up the tree ------------------------------ */
    branches_from_affected_node.resize(ttt.n_nodes());

    // 1. Get the leaf labels out of the machine.  These should be based on the leaf sequences alignment for partition 1.
    context_ptr taxa_ptr(*this, *r_prop0);
    taxa_ptr = taxa_ptr[5];
    vector<string> labels;
    int n_nodes = taxa_ptr.value().as_<IntMap>().size();
    for(int node = 0; node < n_nodes; node++)
    {
        if (auto name = taxa_ptr[node].value().as_<EMaybe>())
            labels.push_back(name->as_<String>());
    }
    auto leaf_labels = labels;

    // FIXME: maybe do this inside the program?
    for(int node = 0; node < n_nodes; node++)
    {
        if (not taxa_ptr[node].value().as_<EMaybe>())
            labels.push_back("A"+std::to_string(node));
    }

    // 2. Set up the TreeInterface mapping to the tree inside the machine

    int tree_index = add_compute_expression( {var("BAliPhy.ATModel.tree"), my_atmodel()} );
    TC = new tree_constants(*this, labels, get_expression(tree_index));

    // 3. Remap the input tree to have the same label_string <-> node-number mapping FOR LEAVES.
    // FIXME: We need ALL the nodes to have the right label_string <-> node-number mapping in
    //           order to handle alignments with internal-node sequences.
    auto tt = ttt;
    assert(tt.n_leaves() == leaf_labels.size());
    remap_T_leaf_indices(tt, leaf_labels);
    for(auto node: tt.leaf_nodes())
        assert(tt.get_label(node) == labels[node]);

    // 4. We need to do this so that we can compute the likelihood of specified trees.
    t().read_tree(tt);

    /* --------------------------------------------------------------- */

    param scales_list = add_compute_expression( {var("BAliPhy.ATModel.scales"),my_atmodel()} );

    // R2. Register individual scales
    PC->branch_scales_ = get_params_from_list(*this, scales_list.ref(*this));

    // R5. Register branch categories
    auto maybe_branch_cats = evaluate_expression( {var("BAliPhy.ATModel.branch_categories"), my_atmodel()} );
    if (has_constructor(maybe_branch_cats,"Data.Maybe.Just"))
        PC->branch_categories = get_params_from_list(*this, {fromJust,{var("BAliPhy.ATModel.branch_categories"), my_atmodel()}}, tt.n_branches());

    // create data partitions
    for(int i=0;i<n_partitions;i++)
        PC->DPC.emplace_back(*this, t(), sequence_data[i].get_reg());

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

    for(int i=0;i<n_partitions;i++)
        if (get_data_partition(i).has_pairwise_alignments())
        {
            variable_alignment_ = true;
            get_data_partition(i).set_alignment(A[i]);
        }
}

Parameters::Parameters(const context_ref& C, int tree_reg)
    :Model(C)
{
    TC = new tree_constants(*this, tree_reg);
    branches_from_affected_node.resize(t().n_nodes());

    // Find the downstream partitions.
    vector<int> partition_regs;
    {
        auto tweak_tree = [&](context_ref& C)
        {
            ModifiablesTreeInterface T(C, tree_reg);
            optional<int> internal_branch;
            for(int b=0;b<T.n_branches();b++)
                if (T.is_internal_branch(b))
                    internal_branch=b;
            if (internal_branch)
            {
                vector<int> branches;
                T.append_branches_after(T.reverse(*internal_branch), branches);
                T.append_branches_after(*internal_branch, branches);
                ::NNI(T, branches[0], branches[2]);
            }

            if (T.can_set_branch_length(0))
                T.set_branch_length(0,1.0);
        };

        auto downstream_sampling_events = find_affected_sampling_events(tweak_tree);

        for(auto se: downstream_sampling_events)
        {
            // Under what circumstances would se get unregistered?
            if (auto type = dist_type(se))
            {
                if (*type == "ctmc_on_tree" or *type == "ctmc_on_tree_fixed_A")
                {
                    auto r_out = out_edges_from_dist(se);
                    if (not r_out)
                        throw myexception()<<"Can't find output reg for sampling event";
                    partition_regs.push_back(*r_out);
                }
            }
            else
                throw myexception()<<"missing sampling event";
        }
    }

    PC = new parameters_constants();

    // create data partitions
    for(int partition_reg: partition_regs)
        PC->DPC.emplace_back(*this, t(), partition_reg);

    // set variable_alignment_
    for(int i=0;i<n_data_partitions();i++)
        if (get_data_partition(i).has_IModel())
            variable_alignment_ = true;
}
