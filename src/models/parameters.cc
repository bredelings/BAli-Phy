
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
using std::unordered_map;

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
    return property(8).value().as_<PtrBox<alphabet>>();
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

bool data_partition::has_IModel() const
{
    return (bool)DPC().alignment_properties_reg;
}

object_ptr<const EVector> data_partition::get_sequence(int i) const
{
    return property(7)[i].value().as_ptr_to<EVector>();
}

object_ptr<const EVector> data_partition::transition_P(int b) const
{
    b = t().undirected(b);
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
    return property(10).value().as_int();
}

int data_partition::n_states() const
{
    return property(9).value().as_int();
}

object_ptr<const Box<Matrix>> data_partition::WeightedFrequencyMatrix() const
{
    return property(5).value().as_ptr_to<Box<Matrix>>();
}

object_ptr<const EVector> data_partition::state_letters() const
{
    return property(6).value().as_ptr_to<EVector>();
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
        if (auto out_edges = C.out_edges_to_var( r_alignment ))
        {
            int s_alignment = *out_edges->begin();
            auto A_properties = C.dist_properties(s_alignment);
            alignment_properties_reg = A_properties->get("properties");
        }
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

string write_newick(const Parameters& P, bool print_lengths)
{
    return write(P.t(), P.t().labels(), print_lengths);
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

    reconnect_branch(a,y,b);
    reconnect_branch(y,b,y);

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

    reconnect_branch(y,y,b);
    reconnect_branch(a,b,y);
}

void Parameters::reconnect_branch(int s1, int t1, int t2)
{
    t().reconnect_branch(s1, t1, t2);
}

// This could create loops it we don't check that the subtrees are disjoint.
// br{1,2} point out of the subtrees.  b{1,2} point into the subtrees, towards the other subtree.
void Parameters::exchange_subtrees(int br1, int br2)
{
    auto T = t();
    tryNNI(T, br1, br2);
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
        auto target = get<1>(TC->parameters_for_tree_branch[b]).get_value(*this);
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


/*
 * OK, so the idea is that under some tree change, some prior or likelihood term gets invalidated.
 * We look in that term to see if it contains an alignment.
 * Then we can try and (for example) sum out alignments under that tree change.
 *
 * Here, we don't do a specific change, but still try to find the terms that depend on the tree.
 */

Parameters::Parameters(const context_ref& C, int tree_reg, const std::vector<int>& alignments_regs)
    :Model(C)
{
/*
 * FIXME: If we are given the alignments_regs, should we try to use properties(3) to access them?
 */

    TC = new tree_constants(*this, tree_reg);

    // Find the downstream partitions.
    vector<int> partition_regs;
    {
        auto tweak_tree = [&](context_ref& C)
        {
            ModifiablesTreeInterface T(C, tree_reg);
            optional<int> internal_branch;
            for(int b: T.internal_branches())
                internal_branch=b;
            if (internal_branch)
            {
                vector<int> branches;
                T.append_branches_after(T.reverse(*internal_branch), branches);
                T.append_branches_after(*internal_branch, branches);
                tryNNI(T, branches[0], branches[2]);
            }

            int branch = T.branches()[0];
            if (T.has_branch_lengths() and T.can_set_branch_length(branch))
                T.set_branch_length(branch,1.0);

            int node = T.nodes()[0];
            if (T.has_node_times() and T.can_set_node_time(node))
                T.set_node_time(node, 0.0);

            for(auto& alignments_reg: alignments_regs)
            {
                context_ptr alignments(C, alignments_reg);
                expression_ref tmp = alignments.value();
                for(auto& [b,_]: tmp.as_<IntMap>())
                {
		    // This should unset both forward and reverse alignments.
                    auto a_for_b = alignments[b];
                    if (auto m = a_for_b.modifiable())
                        m->set_value(0);
                }
            }
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


int alignment_length(const data_partition& P)
{
    if (not P.has_pairwise_alignments()) return 0;

    auto branches = P.t().all_branches_from_node(0);

    int total = P.seqlength(0);
    for(int b: branches)
        total += P.get_pairwise_alignment(b).count_insert();

    return total;
}


int alignment_length(const Parameters& P)
{
    int total = 0;
    for(int p=0;p<P.n_data_partitions();p++)
        total += alignment_length(P[p]);
    return total;
}

#ifdef HAVE_MPI
void exchange_random_pairs(int iterations, Parameters& P, MCMC::MoveStats& /*Stats*/)
{
    mpi::communicator world;
    world.barrier();

    int proc_id = world.rank();
    int n_procs = world.size();

    if (n_procs < 2) return;

    vector<int> p(n_procs);
    if (proc_id == 0)
    {
	p = random_permutation(n_procs);

	for(int dest=1;dest<n_procs;dest++)
	    world.send(dest,0,p);
    }
    else
	world.recv(0, 0, p);

    int partner = -1;

    for(int i=0;i<p.size();i++)
    {
	if (p[i] != proc_id) continue;

	if (i%2)
	    partner = i-1;
	else
	    partner = i+1;

	break;
    }

    if (partner < p.size())
	partner = p[partner];
    else
	partner = -1;

    cerr<<"iteration="<<iterations<<"   proc="<<proc_id<<": choosing partner "<<partner<<endl;

    if (partner >=0 and partner < n_procs)
    {
	double L1 = log(P.likelihood());

	double b1 = P.get_beta();

	if (proc_id > partner) {
	    cerr<<"Proc "<<proc_id<<": sending beta = "<<b1<<endl;

	    world.send(partner, 0, b1);

	    cerr<<"Proc "<<proc_id<<": sending likelihood = "<<L1<<endl;

	    world.send(partner, 0, L1);

	    int exchange = -1;

	    world.recv(partner, mpi::any_tag, exchange);

	    cerr<<"Proc "<<proc_id<<": result = "<<exchange<<endl;
	    if (exchange == 1) {
		world.recv(partner, mpi::any_tag, b1);
		cerr<<"Proc "<<proc_id<<": new beta = "<<b1<<endl;
		P.set_beta(b1);
	    }
	}
	else {
	    double L2;
	    double b2;

	    world.recv(partner, 0, b2);
	    world.recv(partner, 0, L2);

	    cerr<<"Proc "<<proc_id<<": b1 = "<<b1<<endl;
	    cerr<<"Proc "<<proc_id<<": b2 = "<<b2<<endl;
	    cerr<<"Proc "<<proc_id<<": L1 = "<<L1<<endl;
	    cerr<<"Proc "<<proc_id<<": L2 = "<<L2<<endl;

	    // db * dL = -db*dE   because E = -L = -log(likelihood)
	    log_double_t ratio = exp_to<log_double_t>( (b2-b1)*(L1-L2) );
	    int exchange = 0;
	    if (ratio >= 1 or uniform() < ratio)
		exchange = 1;

	    cerr<<"Proc "<<proc_id<<": ratio = "<<ratio<<endl;
	    cerr<<"Proc "<<proc_id<<": result = "<<exchange<<endl;

	    world.send(partner, 0, exchange); // MPI::COMM_WORLD.Send   (&exchange, 1, MPI::INT,  partner, 0);
	    if (exchange == 1)
	    {
		world.send(partner, 0, b1); // MPI::COMM_WORLD.Send   (&b1, 1, MPI::DOUBLE,  partner, 0);

		P.set_beta(b2);
	    }
	}
    }
}

void exchange_adjacent_pairs(int /*iterations*/, Parameters& P, MCMC::MoveStats& Stats)
{
    mpi::communicator world;
    world.barrier();

    int proc_id = world.rank();
    int n_procs = world.size();

    if (n_procs < 2) return;
    if (not P.all_betas.size()) return;

    // Determine the probability of this chain at each temperature
    log_double_t Pr1 = P.heated_probability();
    vector<double> Pr;
    for(int i=0;i<P.all_betas.size();i++)
    {
	P.set_beta(P.all_betas[i]);
	Pr.push_back(log(P.heated_probability()));
    }
    P.set_beta(P.all_betas[P.beta_index]);
    log_double_t Pr2 = P.heated_probability();

    assert(std::abs(log(Pr1)-log(Pr2)) < 1.0e-9);


    //  double oldbeta = beta;
    vector< vector<double> > Pr_all;

    // maps from chain -> position
    vector< int > chain_to_beta;

    // Has each chain recently been at the high beta (1) or the low beta (0)
    vector<int> updowns;

    // Collect the Betas and probabilities in chain 0 (master)
    gather(world, Pr, Pr_all, 0);
    gather(world, P.beta_index, chain_to_beta, 0);
    gather(world, P.updown, updowns, 0);

    // maps from beta index to chain index
    vector<int> beta_to_chain = invert(chain_to_beta);

    if (proc_id == 0)
    {
	//----- Compute an order of chains in decreasing order of beta -----//
	MCMC::Result exchange(n_procs-1,0);

	for(int i=0;i<3;i++)
	{
	    //----- Propose pairs of adjacent-temperature chains  ----//
	    for(int j=0;j<n_procs-1;j++)
	    {
		int chain1 = beta_to_chain[j];
		int chain2 = beta_to_chain[j+1];

		// Compute the log probabilities for the two terms in the current order
		double log_Pr1 = Pr_all[chain1][j] + Pr_all[chain2][j+1];
		// Compute the log probabilities for the two terms in the proposed order
		double log_Pr2 = Pr_all[chain2][j] + Pr_all[chain1][j+1];

		// Swap the chain in beta positions j and j+1 if we accept the proposal
		exchange.counts[j]++;
		if (uniform() < exp(log_Pr2 - log_Pr1) )
		{
		    std::swap(beta_to_chain[j],beta_to_chain[j+1]);
		    exchange.totals[j]++;
		}
	    }
	}

	// estimate average regeneration times for beta high->low->high
	MCMC::Result regeneration(n_procs,0);

	if (updowns[beta_to_chain[0]] == 0)
	    regeneration.counts[beta_to_chain[0]]++;

	for(int i=0;i<n_procs;i++)
	    regeneration.totals[i]++;

	// fraction of visitors that most recently visited highest Beta
	MCMC::Result f_recent_high(n_procs, 0);

	// the lowest chain has hit the lower bound more recently than the higher bound
	updowns[beta_to_chain[0]] = 1;
	// the highest chain has hit the upper bound more recently than the higher bound
	updowns[beta_to_chain.back()] = 0;

	for(int j=0;j<n_procs;j++)
	    if (updowns[beta_to_chain[j]] == 1) {
		f_recent_high.counts[j] = 1;
		f_recent_high.totals[j] = 1;
	    }
	    else if (updowns[beta_to_chain[j]] == 0)
		f_recent_high.counts[j] = 1;

	Stats.inc("MC3.exchange",exchange);
	Stats.inc("MC3.fracRecentHigh",f_recent_high);
	Stats.inc("MC3.betaRegenerationTimes",regeneration);
    }

    // recompute the chain_to_beta mapping
    vector<int> chain_to_beta2 = invert(beta_to_chain);

    // Broadcast the new betas for each chain
    int old_index = P.beta_index;
    scatter(world, chain_to_beta2, P.beta_index, 0);
    scatter(world, updowns, P.updown, 0);

    if (log_verbose)
	cerr<<"Proc["<<proc_id<<"] changing from "<<old_index<<" -> "<<P.beta_index<<endl;

    P.set_beta(P.all_betas[P.beta_index]);
}
#endif
