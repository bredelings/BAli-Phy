/*
  Copyright (C) 2004-2010 Benjamin Redelings

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

#include "sample.H"
#include "util/range.H"
#include "util/rng.H"
#include "util/log-level.H"
#include "util/settings.H"       // for get_setting_or( )
#include <algorithm>
#include "dp/3way.H"
#include "util/permute.H"
#include "alignment/alignment-util.H"

using MCMC::MoveStats;

using std::valarray;
using std::vector;
using std::string;
using std::map;
using std::optional;


void slide_node_move(owned_ptr<context>& P, MoveStats& Stats,int b) 
{
    slide_node(P,Stats,b);
}

void change_branch_length_move(owned_ptr<context>& P, MoveStats& Stats,int b) 
{
    change_branch_length(P,Stats,b);
}

void change_branch_length_multi_move(owned_ptr<context>& P, MoveStats& Stats,int b) 
{
    change_branch_length_multi(P,Stats,b);
}

void sample_tri_one(owned_ptr<context>& P, MoveStats&,int b) 
{
    Parameters* PP = P.as<Parameters>();
    auto t = PP->t();
    assert(is_degree3_edge(t,b));

    int node1 = t.target(t.undirected(b));
    int node2 = t.source(t.undirected(b));
  
    if (uniform() < 0.5)
        std::swap(node1,node2);

    if (t.degree(node1) != 3)
        std::swap(node1,node2);
    
    tri_sample_alignment(*PP,node1,node2);
}

void sample_tri_branch_one(owned_ptr<context>& P, MoveStats& Stats,int b) 
{
    Parameters* PP = P.as<Parameters>();

    MCMC::Result result(2);

//    assert(PP->variable_alignment()); 

    auto t = PP->t();
    assert(is_degree3_edge(t,b));
  
    int node1 = t.target(t.undirected(b));
    int node2 = t.source(t.undirected(b));

    if (uniform() < 0.5)
        std::swap(node1,node2);

    if (t.degree(node1) != 3)
        std::swap(node1,node2);
    
    const double sigma = 0.3/2;
    double length1 = t.branch_length(b);
    double length2 = length1 + gaussian(0,sigma);
    if (length2 < 0) length2 = -length2;

    if (tri_sample_alignment_branch(*PP,node1,node2,b,1,length2)) {
        result.totals[0] = 1;
        result.totals[1] = std::abs(length2 - length1);
    }

    Stats.inc("sample_tri_branch",result);
}

void sample_cube_one(owned_ptr<context>& P, MoveStats&,int b) 
{
    Parameters* PP = P.as<Parameters>();
    auto t = PP->t();
    assert(is_degree3_edge(t,b));

    int node1 = t.target(t.undirected(b));
    int node2 = t.source(t.undirected(b));
  
    if (uniform() < 0.5)
        std::swap(node1,node2);

    if (t.degree(node1) != 3)
        std::swap(node1,node2);
    
    cube_sample_alignment(*PP,node1,node2);
}

void sample_cube_branch_one(owned_ptr<context>& P, MoveStats& Stats,int b) 
{
    Parameters* PP = P.as<Parameters>();

    MCMC::Result result(2);

//    assert(PP->variable_alignment()); 

    auto t = PP->t();
    assert(is_degree3_edge(t,b));
  
    int node1 = t.target(t.undirected(b));
    int node2 = t.source(t.undirected(b));

    if (uniform() < 0.5)
        std::swap(node1,node2);

    if (t.degree(node1) != 3)
        std::swap(node1,node2);
    
    const double sigma = 0.3/2;
    double length1 = t.branch_length(b);
    double length2 = length1 + gaussian(0,sigma);
    if (length2 < 0) length2 = -length2;

    if (cube_sample_alignment_branch(*PP,node1,node2,b,1,length2)) {
        result.totals[0] = 1;
        result.totals[1] = std::abs(length2 - length1);
    }

    Stats.inc("sample_cube_branch",result);
}


void sample_parameter_and_alignment_on_branch(owned_ptr<context>& P, MoveStats& Stats, int b, const Proposal& proposal)
{
    Parameters* PP = P.as<Parameters>();

    MCMC::Result result(1);

//    assert(PP->variable_alignment()); 

    auto t = PP->t();
    assert(is_degree3_edge(t,b));

    int node1 = t.target(t.undirected(b));
    int node2 = t.source(t.undirected(b));

    if (uniform() < 0.5)
        std::swap(node1,node2);

    if (t.degree(node1) != 3)
        std::swap(node1,node2);
    
    if (tri_sample_alignment_and_parameter(*PP, node1, node2, proposal))
    {
        result.totals[0] = 1;
    }

    // FIXME... somehow the proposal itself has to know something about what to log.

    Stats.inc("sample_and_alignment_on_branch",result);
}


void sample_tri_branch_type_one(owned_ptr<context>& P, MoveStats& Stats,int b) 
{
    Parameters* PP = P.as<Parameters>();

    MCMC::Result result(1);

//    assert(PP->variable_alignment()); 

    auto t = PP->t();
    assert(is_degree3_edge(t,b));

    int node1 = t.target(t.undirected(b));
    int node2 = t.source(t.undirected(b));

    if (uniform() < 0.5)
        std::swap(node1,node2);

    if (t.degree(node1) != 3)
        std::swap(node1,node2);
    
    if (tri_sample_alignment_branch_model(*PP,node1,node2)) {
        result.totals[0] = 1;
    }

    Stats.inc("sample_tri_branch_type",result);
}


void sample_alignments_one(owned_ptr<context>& P, MoveStats& Stats, int b)
{
    double alignment_plus_branch_length_fraction = get_setting_or("alignment_plus_branch_length_fraction",0.01);
    Parameters* PP = P.as<Parameters>();
//    assert(PP->variable_alignment()); 

    if (PP->t().has_branch_lengths() and uniform() < alignment_plus_branch_length_fraction)
        alignment_slice_sample_branch_length(P, Stats, b);
    else
        sample_alignment(*PP,b);
}

void sample_node_move(owned_ptr<context>& P, MoveStats&,int node) 
{
    Parameters* PP = P.as<Parameters>();
//    assert(PP->variable_alignment()); 

    sample_node(*PP,node);
}

void sample_A5_move(owned_ptr<context>& P, MoveStats&,int n0) 
{
    Parameters* PP = P.as<Parameters>();
//    assert(PP->variable_alignment()); 

    auto t = PP->t();
    assert(t.degree(n0) == 3);

    vector<int> nodes = A3::get_nodes_random(t,n0);
    optional<int> n1;
    for(int i=1;i<nodes.size();i++)
        if (t.degree(nodes[i]) == 3)
	{
            n1 = nodes[i];
            break;
        };

    if (not n1) return;
    
    int b = PP->t().undirected(PP->t().find_branch(n0, *n1));

    sample_A5(*PP,b);
}

// cost[b] should be the cost to visit all the branches after b.
// * the cost is the number of branches we need to move the LC.root across
// * we don't need to move the root across leaf branches -- we just have to
//   be on one of the endpoints of the branch
// * the cost of a non-leaf branch with branches_out={l,r} is 2*cost[l]+cost[r]
//   if we visit the subtree in front of l first.
std::unordered_map<int,int> get_cost(const TreeInterface& t)
{
    std::unordered_map<int,int> cost;

    vector<int> stack1; stack1.reserve(t.n_branches()*2);
    vector<int> stack2; stack2.reserve(t.n_branches()*2);

    for(auto b: t.leaf_branches())
    {
        // Leaf branches point away from the leaf.
        // So b2 points toward the leaf.
        int b2 = t.reverse(b);
        cost.insert({b2,0});
        stack1.push_back(b2);
    }

    if (t.n_nodes() == 2)
        return cost;
    
    while(not stack1.empty())
    {
        // fill 'stack2' with branches before 'stack1'
        stack2.clear();
        for(int b: stack1)
            t.append_branches_before(b, stack2);

        // clear 'stack1'
        // these branches are already done -- we just needed to process the branches before them
        stack1.clear();

        for(int b: stack2)
        {
            // Skip b if we already have already processed it.
            if (cost.contains(b)) continue;

            auto children = t.branches_after(b);

            bool all_children_done = true;
            int total_cost = 0;
            int worst_cost = 0;
            for(int b: children)
            {
                if (not cost.contains(b))
                {
                    all_children_done = false;
                    break;
                }

                int child_cost = cost.at(b);

                // It costs 1 more to cross b if it is not a leaf branch.
                if (not t.is_leaf_branch(b)) child_cost++;

                // Find the highest cost branch, which we will traverse only once.
                worst_cost = std::max(worst_cost, child_cost);

                total_cost += child_cost;
            }

            if (all_children_done)
            {
                // We traverse all branches twice, except for the worst one
                cost.insert({b, 2*total_cost - worst_cost});

                // Notify that b is now done, and we can try (or re-try) handling
                // the branches that depend on it.
                stack1.push_back(b);
            }
        }
    }

    // check that all the costs have been calculated
    for(int b: t.directed_branches())
        assert(cost.contains(b));

    return cost;
}

std::unordered_map<int,int> get_distance(const TreeInterface& t, int n)
{
    std::unordered_map<int,int> D;
    D[n] = 0;

    auto branches = t.branches_out(n);
    int d = 0;
    int i = 0;
    while(i<branches.size())
    {
        d++;
        int j=branches.size();
        for(;i<j;i++)
        {
            D[t.target(branches[i])] = d;
            t.append_branches_after(branches[i], branches);
        }
    }
    return D;
}

vector<int> walk_tree_path_toward_branch(const TreeInterface& t, int b0)
{
    vector<int> branches;
    for(auto b: t.branches_before(b0))
    {
	auto x = walk_tree_path_toward_branch(t, b);
	branches.insert(branches.end(), x.begin(), x.end());
    }
    // Put the incoming branches in last and TOGETHER
    for(auto b: t.branches_before(b0))
	branches.push_back(b);
    return branches;
}

vector<int> walk_tree_path_toward(const TreeInterface& t, int root)
{
    vector<int> branches;
    for(auto b: t.branches_in(root))
    {
	auto x = walk_tree_path_toward_branch(t, b);
	branches.insert(branches.end(), x.begin(), x.end());
    }
    // Put the incoming branches in last and TOGETHER
    for(auto b: t.branches_in(root))
	branches.push_back(b);
    return branches;
}

vector<int> reverse_branch_path(const TreeInterface& t, const vector<int>& branches)
{
    auto branches2 = branches;
    for(auto& b: branches2)
	b = t.reverse(b);
    std::reverse(branches2);
    return branches2;
}

vector<int> walk_tree_path_away(const TreeInterface& t, int node)
{
    return reverse_branch_path(t, walk_tree_path_toward(t, node));
}

vector<int> walk_tree_path_toward_and_away(const TreeInterface& t, int node)
{
    // Branches toward the node
    auto branches = walk_tree_path_toward(t, node);

    vector<int> branches2 = reverse_branch_path(t, branches);

    branches.insert(branches.end(), branches2.begin(), branches2.end());
    return branches;
}


vector<int> walk_tree_path(const TreeInterface& t, int root)
{
    auto cost = get_cost(t);

    auto D = get_distance(t,root);

    auto tcost = cost;
    for(auto& [b,c]: tcost)
        c += D.at(t.target(b));

    vector<int> b_stack;
    b_stack.reserve(t.n_branches());
    vector<int> branches;
    branches.reserve(t.n_branches());
    vector<int> children;
    children.reserve(3);

    // get a leaf branch with minimum 'tcost'
    std::optional<int> best_leaf_branch;
    for(auto b: t.leaf_branches())
    {
        if (not best_leaf_branch)
            best_leaf_branch = b;
        else if (tcost.at(b) < tcost.at(best_leaf_branch.value()))
            best_leaf_branch = b;
    }

    // If the tree has no branches, then quit here.
    if (not best_leaf_branch) return {};

    b_stack.push_back(*best_leaf_branch);

    while(not b_stack.empty())
    {
        // pop stack into list
        branches.push_back(b_stack.back());
        b_stack.pop_back();

        // get children of the result
        children = t.branches_after(branches.back());
        sort(children.begin(), children.end());
        random_shuffle(children);

        // sort children in decrease order of cost
        if (children.size() < 2)
            ;
        else {
            if (children.size() == 2) {
                if (cost.at(children[0]) < cost.at(children[1]))
                    std::swap(children[0],children[1]);
            }
            else
                std::abort();
        }

        // put children onto the stack
        for(int b: children)
            b_stack.push_back(b);
    }

    assert(branches.size() == t.n_branches());

    vector<int> branches2(branches.size());
    for(int i=0;i<branches.size();i++)
        branches2[i] = t.undirected(branches[i]);

    return branches2;
}

void sample_branch_length_(owned_ptr<context>& P,  MoveStats& Stats, int b)
{
    if (log_verbose >= 3) std::cerr<<"\n\n[sample_branch_length_]\n";
    //std::clog<<"Processing branch "<<b<<" with root "<<P.subst_root()<<endl;

    double slice_fraction = get_setting_or("branch_slice_fraction",0.9);

    bool do_slice = (uniform() < slice_fraction);
    if (do_slice)
        slice_sample_branch_length(P,Stats,b);
    else
        change_branch_length(P,Stats,b);
    
    // Find a random direction of this branch, conditional on pointing to an internal node.
    const auto t = P.as<Parameters>()->t();
    auto e = t.edge(b);
    if (uniform() < 0.5)
        e = e.reverse();

    if (t.degree(e.node2) != 3)
        e = e.reverse();

    // NOTE! This pointer might be invalidated after the tree is changed by MH!
    //       We would modify T2 and then do T=T2, thus using the copied structue and destroying the original.

    // FIXME - this might move the accumulator off of the current branch (?)
    // TEST and Check Scaling of # of branches peeled
    if (is_degree3_edge(t,e))
    {
        if (uniform() < 0.5)
            slide_node(P, Stats, t.find_branch(e));
        else 
            change_3_branch_lengths(P,Stats, e.node2);
    }

    if (not do_slice) {
        change_branch_length(P,Stats,b);
        change_branch_length(P,Stats,b);
    }
}

void walk_tree_sample_NNI_and_branch_lengths(owned_ptr<context>& P, MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();
    vector<int> branches = walk_tree_path(PP.t(), PP.subst_root());

    for(int b: branches)
    {
        double U = uniform();

        if (PP.t().has_branch_lengths() and U < 0.1)
            slice_sample_branch_length(P,Stats,b);

        if (PP.t().is_internal_branch(b)) 
        {
            // In theory the 3-way move should have twice the acceptance rate, when the branch length
            // is non-zero, and one of the two other topologies is good while one is bad.
            //
            // This seems to actually occur for the Enolase-48 data set.
            if (uniform() < 0.95)
                three_way_topology_sample(P,Stats,b);
            else
                two_way_NNI_sample(P,Stats,b);
        }

        if (PP.t().has_branch_lengths() and U > 0.9)
            slice_sample_branch_length(P,Stats,b);
    }
}


void walk_time_tree_sample_NNI_and_node_times(owned_ptr<context>& P, MoveStats& Stats)
{
    if (log_verbose >= 3) std::cerr<<"\n\n[walk_time_tree_sample_NNI_and_node_times]\n";
    Parameters& PP = *P.as<Parameters>();
    vector<int> branches = walk_tree_path(PP.t(), PP.subst_root());

    for(int b: branches)
    {
	int n1 = PP.t().source(b);
	int n2 = PP.t().target(b);
        double U = uniform();

        if (U < 0.1)
	{
            slice_sample_node_time(P,Stats,n1);
            slice_sample_node_time(P,Stats,n2);
	}

	three_way_time_tree_NNI_sample(P,Stats,b);

        if (U > 0.9)
	{
            slice_sample_node_time(P,Stats,n2);
            slice_sample_node_time(P,Stats,n1);
	}
    }
}


void walk_tree_sample_NNI(owned_ptr<context>& P, MoveStats& Stats)
{
    Parameters& PP = *P.as<Parameters>();
    vector<int> branches = walk_tree_path(PP.t(), PP.subst_root());

    for(int b: branches)
    {
        if (uniform() < 0.95)
            three_way_topology_sample(P,Stats,b);
        else
            two_way_NNI_sample(P,Stats,b);
    }
}


void walk_tree_sample_NNI_and_A(owned_ptr<context>& P, MoveStats& Stats) 
{
    double NNI_A_fraction = get_setting_or("NNI+A_fraction",0.01);

    Parameters& PP = *P.as<Parameters>();
    vector<int> branches = walk_tree_path(PP.t(), PP.subst_root());

    for(int b: branches)
    {
        if (uniform() < NNI_A_fraction)
            three_way_topology_and_alignment_sample(P,Stats,b);
        else
            if (uniform() < 0.95)
                three_way_topology_sample(P,Stats,b);
            else
                two_way_NNI_sample(P,Stats,b);
    }
}


void walk_tree_sample_alignments(owned_ptr<context>& P, MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();
    vector<int> branches = walk_tree_path(PP.t(), PP.subst_root());

    double cube_fraction = get_setting_or("cube_fraction",0.00);

    for(int b: branches)
    {
        //    std::clog<<"Processing branch "<<b<<" with root "<<P.subst_root()<<endl;

        if ((uniform() < 0.15) and is_degree3_edge(PP.t(),b))
        {
            // FIXME: don't call sample_parameter_and_alignment_on_branch( ): something is wrong.
            if (uniform() < 0.5 or true)
            {
                if (uniform() < cube_fraction)
                    sample_cube_one(P,Stats,b);
                else
                    sample_tri_one(P,Stats,b);
            }
            else
            {
                // sample_parameter_and_alignment_on_branch(P,Stats,b);
            }
        }
        else
            sample_alignments_one(P,Stats,b);
    }
}


// FIXME: Realign from tips basically fails because the distance between sequences is too small!
void realign_from_tips(owned_ptr<context>& P, MoveStats& Stats) 
{
    int AL0 = alignment_length(*P.as<Parameters>());

    if (log_verbose>=3) std::cerr<<"realign_from_tips: |A0| = "<<AL0<<"\n";
    Parameters& PP = *P.as<Parameters>();
    auto t = PP.t();
    int toward_node = (t.n_leaves() > 2) ? uniform_element(t.internal_nodes()) : uniform_element(t.nodes());

    vector<int> branches = walk_tree_path_toward_and_away(t, toward_node);

    for(int b: branches)
    {
        auto t = P.as<Parameters>()->t();
        if (t.has_branch_lengths() and t.can_set_branch_length(b)) sample_branch_length_(P,Stats,b);
        int node1 = t.source(b);
        int node2 = t.target(b);
        if (log_verbose >=4)
        {
            auto length = [&](int node) {return (*P.as<Parameters>())[0].seqlength(node);};
            vector<int> children = t.branches_after(b);

            std::cerr<<"realign_from_tips:   realigning branch "<<b<<"\n";
            std::cerr<<"realign_from_tips:   orig branch lengths = "<<t.branch_length(b);
            if (children.size())
            {
                assert(children.size() == 2);
                std::cerr<<" -> ("<<t.branch_length(children[0])<<","<<t.branch_length(children[1])<<")";
            }
            std::cerr<<"\n";
            std::cerr<<"realign_from_tips:   orig seq lengths = "<<length(node1)<<" -> "<<length(node2);
            if (children.size())
            {
                assert(children.size() == 2);
                int node3 = t.target(children[0]);
                int node4 = t.target(children[1]);
                std::cerr<<" -> ("<<length(node3)<<","<<length(node4)<<")";
            }
            std::cerr<<"\n";
        }
        if (t.degree(node2) == 3)
        {
            if (log_verbose >=3) std::cerr<<"     Performing 3-way alignment\n";
            tri_sample_alignment(*P.as<Parameters>(), node2, node1);
        }
        else
        {
            if (log_verbose >=3) std::cerr<<"     Performing 2-way alignment\n";
            sample_alignment(*P.as<Parameters>(), b);
        }
        if (t.has_branch_lengths() and t.can_set_branch_length(b)) sample_branch_length_(P,Stats,b);

        if (log_verbose >= 4)
        {
            auto t = P.as<Parameters>()->t();
            int node1 = t.source(b);
            int node2 = t.target(b);

            auto length = [&](int node) {return (*P.as<Parameters>())[0].seqlength(node);};
            std::cerr<<"realign_from_tips:   post seq lengths = "<<length(node1)<<" -> "<<length(node2);
            vector<int> children = t.branches_after(b);
            if (children.size())
            {
                assert(children.size() == 2);
                int node3 = t.target(children[0]);
                int node4 = t.target(children[1]);
                std::cerr<<" -> ("<<length(node3)<<","<<length(node4)<<")";
            }
            std::cerr<<"\n\n";
        }

        // We can't do this on a fixed topology.
        // three_way_topology_sample(P,Stats,b);
    }
    int AL1 = alignment_length(*P.as<Parameters>());
    if (log_verbose>=4) std::cerr<<"realign_from_tips: |A1| = "<<AL1<<"\n";

    MCMC::Result result(2);
    result.totals[0] = 1;
    result.totals[1] = std::abs(AL1-AL0);
    Stats.inc("realign_from_tips",result);
}

void walk_tree_sample_branch_lengths(owned_ptr<context>& P, MoveStats& Stats) 
{
    if (log_verbose >= 3) std::cerr<<"\n\n[walk_tree_sample_branch_lengths]\n";

    Parameters& PP = *P.as<Parameters>();
    assert(PP.t().has_branch_lengths());

    vector<int> branches = walk_tree_path(PP.t(), PP.subst_root());

    for(int b: branches)
    {
        // Do a number of changes near branch @b
        sample_branch_length_(P,Stats,b);
    }
}
