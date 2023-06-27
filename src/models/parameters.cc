
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

#include "sequence/doublets.H"
#include "sequence/codons.H"
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

string data_partition::ancestral_sequence_alignment() const
{
    return property(3)[0].value().as_<String>();
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

data_partition_constants::data_partition_constants(context_ref& C, int r_data)
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

template<typename T>
T load_value(const Model::key_map_t& keys, const std::string& key, const T& t)
{
    auto loc = keys.find(key);
    if (loc != keys.end())
        return loc->second;
    else
        return t;
}

// scaleMs
// 

Parameters::Parameters(const Program& prog,
                       const key_map_t& keys,
                       const vector<alignment>& A,
                       const SequenceTree& ttt)
:Model(prog, keys),
 PC(new parameters_constants()),
 variable_alignment_( false ),
 updown(-1)
{
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
    branches_from_affected_node.resize(t().n_nodes());

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

    // R5. Register branch categories
    auto maybe_branch_cats = evaluate_expression( {var("BAliPhy.ATModel.branch_categories"), my_atmodel()} );
    if (has_constructor(maybe_branch_cats,"Data.Maybe.Just"))
        PC->branch_categories = get_params_from_list(*this, {fromJust,{var("BAliPhy.ATModel.branch_categories"), my_atmodel()}}, tt.n_branches());

    // create data partitions
    for(auto partition: sequence_data)
        PC->DPC.emplace_back(*this, partition.get_reg());

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

    for(int i=0;i<n_data_partitions();i++)
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

            int branch = 0;
            if (T.has_branch_lengths() and T.can_set_branch_length(branch))
                T.set_branch_length(branch,1.0);

            int node = 0;
            if (T.has_node_times() and T.can_set_node_time(node))
                T.set_node_time(node,0.0);
        };

        auto downstream_sampling_events = find_affected_sampling_events(tweak_tree);

        /* OK, so is there a problem here if we observe the same thing twice? */
        
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
        PC->DPC.emplace_back(*this, partition_reg);

    // set variable_alignment_
    for(int i=0;i<n_data_partitions();i++)
        if (get_data_partition(i).has_IModel())
            variable_alignment_ = true;
}
