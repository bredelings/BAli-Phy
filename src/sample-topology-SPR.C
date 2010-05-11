/*
   Copyright (C) 2004-2009 Benjamin Redelings

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
/// \file sample-topology-SPR.C
///
/// \brief Contains functions for subtree-prune-and-regraft (SPR) transition kernels.
///

#include <cmath>
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "choose.H"
#include "likelihood.H"
#include "probability.H"

#include "3way.H"
#include "tree-util.H"
#include "alignment-sums.H"
#include "alignment-constraint.H"
#include "substitution-index.H"

using MCMC::MoveStats;

using boost::dynamic_bitset;
using std::vector;
using std::string;
using std::map;

int random_int_from_double(double x)
{
  int n = (int)x;
  x -= n;
  n += poisson(x);
  return n;
}

int n_SPR_moves(const Parameters& P)
{
  double f = loadvalue(P.keys,"SPR_amount",0.1);
  int n = random_int_from_double(P.T->n_branches()*f);
  return n+1;
}

void SPR_inc(MoveStats& Stats, MCMC::Result result,const string& name,double L)
{
  Stats.inc(name, result);

  if (L < 0.5)
    Stats.inc(name+"-0.5", result);
  else if (L < 1)
    Stats.inc(name+"-1.0", result);
  else if (L < 2.0)
    Stats.inc(name+"-2.0", result);
  else 
    Stats.inc(name+"-2.0+", result);
}

int topology_sample_SPR(vector<Parameters>& p,const vector<efloat_t>& rho,int n1, int n2) 
{
  assert(p.size() == 2);
  assert(p[0].variable_alignment() == p[1].variable_alignment());

  //----------- Generate the Different node lists ---------//
  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(*p[0].T, n1, n2);     // Using two random orders can lead to different total
  nodes[1] = A3::get_nodes_branch_random(*p[1].T, n1, n2);     //  probabilities for p[i] and p[j] when p[i] == p[j].

  return sample_tri_multi(p,nodes,rho,true,true);
}

#include "slice-sampling.H"

int topology_sample_SPR_slice_connecting_branch(vector<Parameters>& p,int b) 
{
  int b_ = p[0].T->directed_branch(b).undirected_name();

  branch_length_slice_function logp1(p[0],b_);
  branch_length_slice_function logp2(p[1],b_);

  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  //  double Pr1 = log(p[0].probability());
  //  double Pr2 = log(p[1].probability());

  vector<slice_function*> logp;
  logp.push_back(&logp1);
  logp.push_back(&logp2);

  double w = p[0].branch_mean();

  std::pair<int,double> choice = slice_sample_multi(logp,w,-1);

  return choice.first;
}

int topology_sample_SPR_slice_slide_node(vector<Parameters>& p,int b) 
{
  int b_ = p[0].T->directed_branch(b).undirected_name();

  slide_node_slice_function logp1(p[0],b);
  slide_node_slice_function logp2(p[1],b);

  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  //  double Pr1 = log(p[0].probability());
  //  double Pr2 = log(p[1].probability());

  vector<slice_function*> logp;
  logp.push_back(&logp1);
  logp.push_back(&logp2);

  double w = p[0].branch_mean();

  vector<double> X0(2);
  X0[0] = logp1.current_value();
  X0[1] = uniform()*(logp2.upper_bound);

  std::pair<int,double> choice = slice_sample_multi(X0,logp,w,-1);

  return choice.first;
}

/// Do a SPR move on T1, moving the subtree behind b1_ to branch b2
double do_SPR(SequenceTree& T1, int b1_,int b2) 
{
  const_branchview b1 = T1.directed_branch(b1_);

  SequenceTree T2 = T1;

  //------ Generate the new topology ------//
  if (T2.directed_branch(b2).target() == b1.target() or 
      T2.directed_branch(b2).source() == b1.target()) 
    ;
  else
    SPR(T2,b1.reverse(),b2);

  //------ Find the two new branches ------//
  vector<const_branchview> connected1;
  append(T1.directed_branch(b1.source(),b1.target()).branches_after(),connected1);

  vector<const_branchview> connected2;
  append(T2.directed_branch(b1.source(),b1.target()).branches_after(),connected2);

  assert(connected1.size() == 2);
  assert(connected2.size() == 2);

  //------- Place the split randomly -------//
  double L1 = connected1[0].length() + connected1[1].length();
  double L2 = connected2[0].length() + connected2[1].length();

  T2.directed_branch(connected2[0]).set_length( myrandomf() * L2 );
  T2.directed_branch(connected2[1]).set_length( L2 - T2.directed_branch(connected2[0]).length() );

  T1 = T2;

  return L2/L1;
}


double do_SPR(Parameters& P, int b1, int b2)
{
  double ratio = do_SPR(*P.T, b1, b2);
  P.tree_propagate();
  return ratio;
}

vector<double> effective_lengths(const Tree& T)
{
  vector<double> lengths(2*T.n_branches(),0);

  vector<const_branchview> branches = branches_from_leaves(T);

  for(int i=0;i<branches.size();i++)
  {
    lengths[branches[i].name()] = branches[i].length();

    vector<const_branchview> pre_b;
    append(branches[i].branches_before(),pre_b);
    if (pre_b.size() > 0) {
      double Pr_change_on_all = 1;
      for(int j=0;j<pre_b.size();j++)
	Pr_change_on_all *= (1.0-exp(-lengths[pre_b[j].name()]));
      double Pr_no_change_on_at_least_1 = 1.0-Pr_change_on_all;
      lengths[branches[i]] += -log(Pr_no_change_on_at_least_1);
      assert(lengths[branches[i]] >= branches[i].length());
    }
  }

  return lengths;
}

double effective_length(const Tree& T, int b)
{
  return effective_lengths(T)[b];
}

int choose_SPR_target(SequenceTree& T1, int b1_) 
{
  const_branchview b1 = T1.directed_branch(b1_);

  //----- Select the branch to move to ------//
  dynamic_bitset<> subtree_nodes = T1.partition(b1.reverse());
  subtree_nodes[b1.target()] = true;

  vector<int> branches;
  vector<double> lengths;

  for(int i=0;i<T1.n_branches();i++) 
  {
    const_branchview bi = T1.branch(i);

    // skip branch if its contained in the subtree
    if (subtree_nodes[bi.target()] and 
	subtree_nodes[bi.source()])
      continue;

    double L = 1.0;

    // down-weight branch if it is one of the subtree's 2 neighbors
    if (subtree_nodes[bi.target()] or 
	subtree_nodes[bi.source()])
      L = 0.5;

    branches.push_back(i);
    lengths.push_back(L);
  }

  int b2 = branches[ choose(lengths) ];

  return b2;
}

void remove_duplicates(vector<int>& v) {
  for(int i=v.size()-1;i>=0;i--) {
    bool dup=false;
    for(int j=0;j<i and not dup;j++)
      if (v[j] == v[i]) dup=true;
    if (dup)
      v.erase(v.begin()+i);
  }
}

MCMC::Result SPR_stats(const Tree& T1, Tree& T2, bool success, int bins, int b1 = -1)
{
  MCMC::Result result(2+bins,0);

  result.counts[0] = 1;
  if (success) result.totals[0] = 1;

  int dist = topology_distance(T1,T2)/2;

  if (b1 != -1) 
  //---------------- Check if topology changed ----------------//
  {
    int n1 = T1.directed_branch(b1).target();
    int n2 = T1.directed_branch(b1).source();
    assert( n1 == T2.directed_branch(b1).target() );
    assert( n2 == T2.directed_branch(b1).source() );

    vector<const_branchview> connected1;
    append(T1.directed_branch(n2,n1).branches_after(),connected1);

    vector<const_branchview> connected2;
    append(T2.directed_branch(n2,n1).branches_after(),connected2);
 
    bool same_topology = (
			  (connected1[0] == connected2[0] and connected1[1] == connected2[1]) or
			  (connected1[0] == connected2[1] and connected1[1] == connected2[0])
			  );

    if (same_topology)
      assert(dist == 0);
    else
      assert(dist > 0);
  }

  // count dist in [0,bins)
  if (dist > bins) dist = bins;
  for(int i=0;i<=bins;i++) {
    if (dist == i) {
      result.counts[1+i] = 1;
      if (success) result.totals[1+i] = 1;
    }
  }

  return result;
}

/* 
 * What causes things to be invalidated in an SPR move?
 * - Cause #1: branch length changes (propagate away from branch in both directions)
 *   (This therefore includes one direction -- but not both directions -- of every branch in the tree)
 * - Cause #2: pruning and regrafting.  This changes subA indices, and also changes cached likelihoods.
 *
 * What has to be invalidated in an SPR move?
 * - All likelihood caches on and after both directions of the now-united branch from which the subtree was pruned.
 * - All subA indices on and after both directions of the now-united branch from which the subtree was pruned.
 * - Likelihood caches and subA indices for the direction of that branch that was pointed away from the pruned subtree.
 * 
 * - SubA indices on all branches that are after (on the result tree) the branch that the pruned subtree is behind.
 * - Likelihood caches on all the same branches.
 * - SubA indices on the pruned tree that are pointing away from the regrafting point.
 * - Likelihood caches on all the same (directed) branches.

 * - SubA indices and Likelihood caches on and after both directions of the moving branch.

 * Query: is this the same as invalidating all subA indices and likelihood caches on all branches after the attachment node
          (i) first on the initial tree
 *        (ii) and then on the target tree?
 * Answer: Yes, I think so.  At least I don't see what is missing...
 *
 *
 * Also, we have to mark pairwise alignments for updating on the merged-branch and the two parts of the split-branch.
 */

/*
 * Q: Now, the procedure below simply changes the length on the merge and split branches (2 or 3 total)
 *     and also calls invalidate_subA_index_branch( ) on them.
 *    So, why does the procedure below actually work? 
 * A: Well, both of these calls go out bidirectionally and invalidate neighbors on both sides.
 *    o Therefore the fact that we have only one direction of the merged-branch doesn't matter. 
 *    o Therefore the fact that we don't explicitly invalidate the branches behind b1 is OK - they get invalidated
        anyway, from one of the directions of their immediate children.
 *
 * Actually this is could be overkill.  We don't need to invalidate BOTH directions of their children.
 * We just need to invalidate the direction that points away from the attachment node.  Of course, the likelihood
 * caches of both directions are going to get blown away anyway, because the length of the child branches is changing.
 * But we could preserve the subA indices of the directed branches that point toward the attachment node.
 *
 * However, this bi-directional invalidation of the three child branches is fairly simply, and blows away everything with one stone.
 * We don't need to explicitly blow away both directions of the moveable branch. (i.e. the one unduplicated in remove_duplicates)
 */

MCMC::Result sample_SPR(Parameters& P,int b1,int b2,bool slice=false) 
{
  const int bins = 6;

  int n1 = P.T->directed_branch(b1).target();
  int n2 = P.T->directed_branch(b1).source();
  assert(P.T->partition(b1)[P.T->branch(b2).target()]);
  assert(P.T->partition(b1)[P.T->branch(b2).source()]);

  //----- Generate the Different Topologies ----//
  P.set_root(n1);
  vector<Parameters> p(2,P);

  //---------------- find the changed branches ------------------//
  vector<int> branches;
  for(edges_after_iterator i=p[1].T->directed_branch(n2,n1).branches_after();i;i++)
    branches.push_back((*i).undirected_name());
  //  std::cerr<<"before = "<<p[1].T<<endl;

  double ratio = do_SPR(p[1],b1,b2);
  // enforce tree constraints
  if (not extends(*p[1].T, *P.TC))
    return MCMC::Result(2+bins,0);

  //  std::cerr<<"after = "<<p[1].T<<endl;
  for(edges_after_iterator i=p[1].T->directed_branch(n2,n1).branches_after();i;i++)
    branches.push_back((*i).undirected_name());

  remove_duplicates(branches);
    
  //----------- invalidate caches for changed branches -----------//
  assert(branches.size() <= 3);
  for(int i=0;i<branches.size();i++) {
    int bi = branches[i];
    p[1].setlength(bi,p[1].T->directed_branch(bi).length());     // bidirectional effect
    p[1].invalidate_subA_index_branch(bi);              // bidirectional effect
    if (p[1].variable_alignment()) 
      p[1].note_alignment_changed_on_branch(bi); 
  }

  int C;
  if (slice)
  {
    C = topology_sample_SPR_slice_slide_node(p,b1);
    P = p[C];
  }
  else {

    //------------- change connecting branch length ----------------//
    vector<efloat_t> rho(2,1);

    //----------- sample alignments and choose topology -----------//
    C = topology_sample_SPR(p,rho,n1,n2);
    
    if (C != -1) 
    {
      for(int i=0;i<P.n_data_partitions();i++) {
	dynamic_bitset<> s1 = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
	dynamic_bitset<> s2 = constraint_satisfied(p[C][i].alignment_constraint, *p[C][i].A);
	
	report_constraints(s1,s2);
      }
      P = p[C];

    // If the new topology conflicts with the constraints, then it should have P=0
    // and therefore not be chosen.  So the following SHOULD be safe!
    }
  }

  return SPR_stats(*p[0].T, *p[1].T, C>0, bins, b1);
}

int choose_subtree_branch_uniform(const Tree& T) 
{
  int b1 = -1;
  while (true)
  {
    b1 = myrandom(T.n_branches()*2);

    // forbid branches leaf branches - no attachment point!
    if (T.directed_branch(b1).target().is_leaf_node()) continue;

    break;
  }

  return b1;
}

int choose_subtree_branch_uniform2(const Tree& T) 
{
  int b1 = -1;
  while (true)
  {
    b1 = myrandom(T.n_branches()*2);

    // forbid branches leaf branches - no attachment point!
    if (T.directed_branch(b1).target().is_leaf_node()) continue;

    // forbid branches with only 1 attachment point - not very useful.
    vector<const_branchview> after;
    append(T.directed_branch(b1).branches_after(), after);
    bool ok = false;
    for(int i=0;i<after.size();i++)
      if (not after[i].target().is_leaf_node())
	ok = true;
    if (not ok) continue;

    break;
  }

  return b1;
}


void sample_SPR_flat(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  Parameters& PP = *P.as<Parameters>();
  int n = n_SPR_moves(PP);

  double p = loadvalue(P->keys,"SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) 
  {
    int b1 = choose_subtree_branch_uniform(*PP.T);

    int b2 = choose_SPR_target(*PP.T,b1);

    double L_effective = effective_length(*PP.T, b1);

    if (not PP.variable_alignment() and uniform() < p) {
      MCMC::Result result = sample_SPR(PP,b1,b2,true);
      SPR_inc(Stats,result,"SPR (flat/slice)",L_effective);
    }
    else  {
      MCMC::Result result = sample_SPR(PP,b1,b2);
      SPR_inc(Stats,result,"SPR (flat)",L_effective);
    }
  }
}

// 1. Moveable branch: If we do anything with the new tree, then BM might not end up being the movable branch.
// + (This exposes the BM thing as a hack to do things not implemented in the SPR interface.)
// +  This is primarily a problem if we represent the answers in terms of branch names.
//
// 2. Shall we identify branches by the pair of nodes at either end?
// + Yes.  (At least we shall START OUT in that direction.)
// + This makes the RESULTS independent of the branch names.
//
// 3. Note, though, that the cached conditional likelihoods depend on the branch names, not the node 
// + However, hopefully we can deal with that INTERNALLY to the likelihood calculation routine.
// + Also, the "movable branch" thing should also be internal to the likelihood calculation routine.
//
// 4. We will calculate the new attachment probabilities using the NEW tree.
// + CHECK that (for the fixed-alignment case) the probabilities are the same if calculated from
///  the new tree.

/// A sortable branch object
struct spr_branch
{
  int node1;
  int node2;

  bool same_orientation(const spr_branch& b2) const
  {
    if (node1 == b2.node1 and node2 == b2.node2) return true;
    return false;
  }

  bool opposite_orientation(const spr_branch& b2) const
  {
    if (node1 == b2.node2 and node2 == b2.node1) return true;
    return false;
  }

  bool operator==(const spr_branch& b2) const
  {
    if (same_orientation(b2)) return true;
    if (opposite_orientation(b2)) return true;
    return false;
  }

  bool operator<(const spr_branch& b2) const
  {
    int b1n1 = std::min(node1,node2);
    int b1n2 = std::max(node1,node2);

    int b2n1 = std::min(b2.node1, b2.node2);
    int b2n2 = std::max(b2.node1, b2.node2);

    if (b1n1 < b2n1) return true;
    if (b1n1 > b2n1) return false;
    if (b1n2 < b2n2) return true;
    if (b1n2 > b2n2) return false;

    return false;
  }

  spr_branch()
    :node1(-1),node2(-1)
  {}

  spr_branch(int n1, int n2)
    :node1(n1),node2(n2)
  {}
};

spr_branch get_spr_branch(const Tree& T, int b)
{
  int n1 = T.directed_branch(b).source();
  int n2 = T.directed_branch(b).target();
  return spr_branch(n1,n2);
}

struct spr_attachment_points: public map<spr_branch,double>
{
};

struct spr_attachment_probabilities: public map<spr_branch,efloat_t>
/// Perform an SPR move: move the subtree BEHIND \a b1 to the branch indicated by \a b2,
///  and choose the point on the branch specified in \a locations.
int SPR_at_location(Tree& T, int b_subtree, int b_target, const spr_attachment_points& locations)
{
  double total_length_before = length(T);

  // unbroken target branch
  double L = T.directed_branch(b_target).length();
  map<spr_branch, double>::const_iterator record = locations.find(get_spr_branch(T,b_target));
  if (record == locations.end())
  {
    std::cerr<<"Branch not found in spr location object!\n"<<std::endl;
    std::abort();
  }
  spr_branch B_unbroken_target = record->first;
  // U is the fraction of the way from B_unbroken_target.node1 
  // toward B_unbroken_target.node2 to place the new node.
  double U = record->second; 
  
  // node joining the subtree to the rest of the tree
  int n0 = T.directed_branch(b_subtree).target();

  // Perform the SPR operation (specified by a branch TOWARD the pruned subtree)
  int BM = SPR(T, T.directed_branch(b_subtree).reverse(), b_target);

  // Find the names of the branches
  assert(T.is_connected(B_unbroken_target.node1, n0));
  assert(T.is_connected(n0, B_unbroken_target.node2));
  int b1 = T.directed_branch(B_unbroken_target.node1, n0);
  int b2 = T.directed_branch(n0, B_unbroken_target.node2);
  assert(T.directed_branch(b1).undirected_name() == BM or T.directed_branch(b2).undirected_name() == BM);

  // Set the lengths of the two branches
  double L1 = L*U;
  double L2 = L - L1;

  T.directed_branch(b1).set_length(L1);
  T.directed_branch(b2).set_length(L2);

  double total_length_after = length(T);
  assert(std::abs(total_length_after - total_length_before) < 1.0e-9);

  // Return the branch name that moved to the new attachment location.
  return BM;
}

spr_attachment_points get_spr_attachment_points(const Tree& T, int b1)
{
  spr_attachment_points locations;

  // One of the two branches (B1) that it points to will be considered the current attachment branch
  // The other branch (BM) will move around to wherever we are currently attaching b1.
  vector<const_branchview> branches;
  append(T.directed_branch(b1).branches_after(),branches);
  assert(branches.size() == 2);
  int B1 = std::min(branches[0].undirected_name(), branches[1].undirected_name());
  int BM = std::max(branches[0].undirected_name(), branches[1].undirected_name());
  spr_branch B0(branches[0].target(), branches[1].target());
  double L0a = branches[0].length();
  double L0b = branches[1].length();

  locations[B0] = L0a/(L0a+L0b);

  /*----------- get the list of possible attachment points, with [0] being the current one.------- */
  // FIXME - With tree constraints, or with a variable alignment and alignment constraints,
  //          we really should eliminate branches that we couldn't attach to, here.
  branches = branches_after(T,b1);

  branches.erase(branches.begin()); // branches_after(b1) includes b1 -- which we do not want.

  // remove the moving branch name (BM) from the list of attachment branches
  for(int i=branches.size()-1;i>=0;i--)
    if (branches[i].undirected_name() == BM)
      branches.erase(branches.begin()+i);

  // convert the const_branchview's to int names
  //  vector<int> branch_names = directed_names(branches);

  // compute attachment locations for non-current branches
  for(int i=1;i<branches.size();i++)
    locations[get_spr_branch(T, branches[i])] = uniform();

  return locations;
}

/**
 * Sample from a number of SPR attachment points - one per branch.
 * 
 * The tricky idea here is that we want cached (likelihood, subA index) for each directed branch
 * not in the PRUNED subtree to be accurate for the situation that the PRUNED subtree is not
 * behind them.

 * Then we make the attachment point the likelihood root, and all branches that point towards
 * the root are valid... branches that do not point toward the root are not used, but may
 * be used when we attach somewhere else.
 *
 * FIXME - How can we keep the information for upwards-pointing branches cached, and then restore it
 *         after we un-regraft from an attachment branch and move on to the next one? (30% speedup)
 *         
 *         It would seem that most other people have (the possibility of) two caches per branch (or node)
 *         and toggle them if an MH move is rejected.  I should add that capability to the caching object.
 *         Then I could use it here.
 */

/* Don't delete!  Call the NEW code from here, and assert that the results are identical.
 */ 
bool sample_SPR_search_one(Parameters& P,MoveStats& Stats,int b1) 
{
  const int bins = 6;

  if (P.T->directed_branch(b1).target().is_leaf_node()) return false;

  // The attachment node for the pruned subtree.
  // This node will move around, but we will always peel up to this node to calculate the likelihood.
  int root_node = P.T->directed_branch(b1).target(); 
  // Because the attachment node keeps its name, this will stay in effect throughout the likelihood calculations.
  P.set_root(root_node);

  // Compute and cache conditional likelihoods up to the (likelihood) root node.
  P.heated_likelihood();

  spr_attachment_points locations = get_spr_attachment_points(*P.T, b1);

  vector<Parameters> p(2,P);

  // One of the two branches (B1) that it points to will be considered the current attachment branch
  // The other branch (BM) will move around to wherever we are currently attaching b1.
  vector<const_branchview> branches;
  append(p[1].T->directed_branch(b1).branches_after(),branches);
  assert(branches.size() == 2);
  int B1 = std::min(branches[0].undirected_name(), branches[1].undirected_name());
  int BM = std::max(branches[0].undirected_name(), branches[1].undirected_name());
  double L0 = p[1].T->branch(B1).length() + p[1].T->branch(BM).length();

  /*----------- get the list of possible attachment points, with [0] being the current one.------- */
  // FIXME - With tree constraints, or with a variable alignment and alignment constraints,
  //          we really should eliminate branches that we couldn't attach to, here.
  branches = branches_after(*p[1].T,b1);

  branches.erase(branches.begin()); // branches_after(b1) includes b1 -- which we do not want.

  // remove the moving branch name (BM) from the list of attachment branches
  for(int i=branches.size()-1;i>=0;i--)
    if (branches[i].undirected_name() == BM)
      branches.erase(branches.begin()+i);

  if (branches.size() == 1) return false;

  // convert the const_branchview's to int names
  vector<int> branch_names = directed_names(branches);

  /*----------------------- Initialize likelihood for each attachment point ----------------------- */

  // The probability of attaching to each branch, w/o the alignment probability
  vector<efloat_t> Pr(branches.size(), 0);
  Pr[0] = P.heated_likelihood() * P.prior_no_alignment();

#ifndef NDEBUG
  vector<efloat_t> LLL(branches.size(), 0);
  LLL[0] = P.heated_likelihood();
#endif

  // Compute total lengths for each of the possible attachment branches
  vector<double> L(branches.size());
  L[0] = L0;
  for(int i=1;i<branches.size();i++)
    L[i] = p[1].T->directed_branch(branches[i]).length();

  // Actually store the trees, instead of recreating them after picking one.
  vector<SequenceTree> trees(branches.size());
  SequenceTree T0 = *p[1].T;
  trees[0] = T0;

  /*----------- Begin invalidating caches and subA-indices to reflect the pruned state -------------*/

  // At this point, caches for branches pointing to B1 and BM are accurate -- but everything after them
  //  still assumes we haven't pruned and is therefore inaccurate.

  p[1].LC_invalidate_branch(B1);          // invalidate caches       for B1, B1^t and ALL BRANCHES AFTER THEM.
  p[1].invalidate_subA_index_branch(B1);  // invalidate subA-indices for B1, B1^t and ALL BRANCHES AFTER THEM.

  p[1].LC_invalidate_branch(BM);          // invalidate caches       for BM, BM^t and ALL BRANCHES AFTER THEM.
  p[1].invalidate_subA_index_branch(BM);  // invalidate subA-indices for BM, BM^t and ALL BRANCHES AFTER THEM.

  // Temporarily stop checking subA indices of branches that point away from the cache root
  p[1].subA_index_allow_invalid_branches(true);

  // Compute the probability of each attachment point
  // After this point, the LC root will now be the same node: the attachment point.
  for(int i=1;i<branch_names.size();i++) 
  {
    *p[1].T = T0;

    // target branch - pointing away from b1
    int b2 = branch_names[i];

    int BM2 = SPR_at_location(*p[1].T, b1, b2, locations);
    assert(BM2 == BM); // Due to the way the current implementation of SPR works, BM (not B1) should be moved.

    p[1].tree_propagate();

    // The length of B1 should already be L0, but we need to reset the transition probabilities (MatCache)
    assert(std::abs(p[1].T->branch(B1).length() - L0) < 1.0e-9);
    p[1].setlength_no_invalidate_LC(B1,L0);   // The likelihood caches (and subA indices) should be correct for
    //  the situation we are setting up here -- no need to invalidate.
    //  (FIXME - do we need to recompute this EVERY time, or just the first time?)

    // We want caches for each directed branch that is not in the PRUNED subtree to be accurate
    //   for the situation that the PRUNED subtree is not behind them.


    // ** 2. INVALIDATE ** the branch that we just landed on and altered

    /// \todo Do I really need to invalidate BOTH directions of b2?  Or, do I just not know WHICH direction to invalidate?
    /// You'd think I'd just need to invalidate the direction pointing TOWARD the root.

    /// \todo Can I temporarily associate the branch with a NEW token, or copy the info to a new location?
    ///  Then I could swap back the token, after I was done, and not lose work.
    /// If I assume that no more than two version of every branch are ever active, then the interface
    ///  would be more restrictive.
    /// If I allow a more general token-based interface, then the more restrictive "flip" version could be implemented
    ///  on top of it, but I would perhaps have to generalize the interface in substitution-cache so as not to
    ///  assume that all tokens represent a branch.

    // We want to suppress the bidirectional propagation of invalidation for all branches after this branch.
    // It would be nice to save the old exp(tB) and switch back to it later.
    p[1].setlength_no_invalidate_LC(b2,p[1].T->directed_branch(b2).length()); // Recompute the transition matrix
    p[1].LC_invalidate_one_branch(b2);                                        //  ... mark for recomputing.
    p[1].LC_invalidate_one_branch(p[1].T->directed_branch(b2).reverse());     //  ... mark for recomputing.

    p[1].setlength_no_invalidate_LC(BM,p[1].T->directed_branch(BM).length());
    p[1].LC_invalidate_one_branch(BM);
    p[1].LC_invalidate_one_branch(p[1].T->directed_branch(BM).reverse());

    // **3. RECORD** the tree and likelihood
    trees[i] = *p[1].T;
    assert(std::abs(length(*p[1].T) - length(T0)) < 1.0e-9);
    assert(std::abs(length(trees[i]) - length(T0)) < 1.0e-9);
    Pr[i] = p[1].heated_likelihood() * p[1].prior_no_alignment();
#ifndef NDEBUG
    LLL[i] = p[1].heated_likelihood();
    assert(std::abs(log(LLL[i]) - log(p[1].heated_likelihood())) < 1.0e-9);
#endif

    // **4. INVALIDATE** the DIRECTED branch that we just landed on and altered
    p[1].setlength_no_invalidate_LC(b2,L[i]);                               // Put back the old transition matrix
    p[1].LC_invalidate_one_branch(b2);                                      // ... mark likelihood caches for recomputing.
    p[1].LC_invalidate_one_branch(p[1].T->directed_branch(b2).reverse());   // ... mark likelihood caches for recomputing.

    // this is bidirectional, but does not propagate
    p[1].invalidate_subA_index_one_branch(BM);
  }

  // Step N-2: Choose an attachment point

  /*
   *  1. Factoring in branch lengths?
   *
   *  pi(x) = the desired equilibrium probability.
   *  rho(x,S) = the probability of x proposing the set S to be sampled from.
   *  alpha(x,S,y) = the probability of accepting/choosing y from S when S is proposed by x.
   *  L[x] = the length of attachment branch x.
   *
   *  pi(x) * rho(x,S) * alpha(x,S,y) = pi(y) * rho(y,S) * alpha(y,S,x) 
   *  rho(x,S) = prod over branches[b] (1/L[b]) * L[x]
   *           = C * L[x]
   *  pi(x) * C * L[x] * alpha(x,S,y) = pi(y) * C * L[y] * alpha(y,S,x) 
   *  alpha(x,S,y) / alpha(y,S,x) = (pi(y)*L[y])/(pi(x)*L[x])
   *
   *  2. Therefore, the probability of choosing+accepting y should be proportional to Pr[y] * L[y].
   *  In the simplest incarnation, this is Gibbs sampling, and is independent of x.
   * 
   *  3. However, we can also use choose_MH(0,Pr), where Pr[i] = likelihood[i]*prior[i]*L[i]
   *  since this proposal/acceptance function also has the property that
   *
   *       choose_MH_P(i,j,Pr)/choose_MH_P(j,i,Pr) = Pr[j]/Pr[i].
   *
   *  4. Now, if we make this whole procedure into a proposal, the ratio for this 
   *     proposal density is
   *
   *       rho(x,S) * alpha(x,S,y)   (C * L[x]) * (D * pi(y) * L[y] )    pi(y)   1/pi(x)
   *       ----------------------- = -------------------------------- = ----- = -------
   *       rho(y,S) * alpha(y,S,x)   (C * L[y]) * (D * pi(x) * L[x] )    pi(x)   1/pi(y)
   *
   *  While the result is independent of the lengths L (which we want), the procedure for
   *  achieving this result need not be independent of the lengths.
   *
   *  We must also remember that the variable rho[i] is the proposal density for proposing the set
   *  S2 = {x,y} and so is proportional to rho(x,S) * alpha(x,S,y) = pi(y).  We could also use 1/pi(x)
   *  and get the same ratio.
   *
   */
  vector<efloat_t> PrL = Pr;
  for(int i=0;i<PrL.size();i++)
    PrL[i] *= L[i];
  int C = choose_MH(0,PrL);

  // enforce tree constraints
  if (not extends(trees[C], *P.TC))
    C = 0;

  // Step N-1: Attach to that point
  *(p[1].T) = trees[C]; 
  p[1].tree_propagate();

  // Step N: Invalidate subA indices and also likelihood caches that are no longer valid.

  // Note that bi-directional invalidation of BM invalidates b1^t and similarly directed branches in the pruned subtree.
  // (BM is an undirected name, so all effects in the loop below MUST be bi-directional.)
  vector<int> btemp; btemp.push_back(B1) ; btemp.push_back(BM) ; btemp.push_back(branch_names[C]);
  // (These effects go out from the old location (the merged branch B1)
  //  and the new location (the split branches BM and branch_names[C]) )
  for(int i=0;i<btemp.size();i++) {
    int bi = btemp[i];
    p[1].setlength(bi, p[1].T->directed_branch(bi).length());   // bidirectional effect
    p[1].invalidate_subA_index_branch(bi);         // bidirectional effect
    if (p[1].variable_alignment()) 
      p[1].note_alignment_changed_on_branch(bi); 
  }
  p[1].subA_index_allow_invalid_branches(false);

#ifndef NDEBUG    
  assert(std::abs(length(*p[1].T) - length(T0)) < 1.0e-9);
  efloat_t L_1 = p[1].likelihood();
  assert(std::abs(L_1.log() - LLL[C].log()) < 1.0e-9);
#endif

  // Step N+1: Use the chosen tree as a proposal, allowing us to sample the alignment.
  //           (This should always succeed, if the alignment is fixed.)

  // BUG! - We can't calculate the reverse proposal probabilities until we select the new alignment!
  // Mixing: We should realign at at least one other attachment location, if P.variable_alignment()

  bool moved = false;
  // If the tree hasn't changed, we don't have to do anything.
  // So, don't resample the alignment, when we have one.
  if (C != 0)
  {
    vector<efloat_t> rho(2,1);
    rho[0] = Pr[C]; // Pr(proposing 0->C) = Pr(C)
    rho[1] = Pr[0]; // Pr(proposing C->0) = Pr(0)
    
    int n1 = P.T->directed_branch(b1).target();
    int n2 = P.T->directed_branch(b1).source();

    // Even when p[0] == p[1], we could still choose C2==0 because of the different node orders in topology_sample_SPR( ).
    int C2 = topology_sample_SPR(p, rho, n1, n2);

    if (C2 != -1) 
    {
      if (C2 > 0) moved = true;
	  
      for(int i=0;i<P.n_data_partitions();i++) {
	dynamic_bitset<> s1 = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
	dynamic_bitset<> s2 = constraint_satisfied(p[C2][i].alignment_constraint, *p[C2][i].A);
	  
	report_constraints(s1,s2);
      }
      P = p[C2];
	
      // If the new topology conflicts with the constraints, then it should have P=0
      // and therefore not be chosen.  So the following SHOULD be safe!
    }

    // If the alignment is not variable, then we should always accept on this second move.
    if (not P.variable_alignment())
      assert(C2 == 1);
  }
  else
    moved = true;

  MCMC::Result result = SPR_stats(T0, *P.T, moved, bins, b1);
  double L_effective = effective_length(*P.T, b1);
  SPR_inc(Stats, result, "SPR (all)", L_effective);

  return ((C != 0) and moved);
}

void sample_SPR_all(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  Parameters& PP = *P.as<Parameters>();
  int n = n_SPR_moves(PP);

  // double p = loadvalue(P.keys,"SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) 
  {
    // Choose a directed branch to prune and regraft -- pointing away from the pruned subtree.
    int b1 = choose_subtree_branch_uniform2(*PP.T);

    sample_SPR_search_one(PP, Stats, b1);
  }
}

void sample_SPR_search_all(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  int B = P.as<Parameters>()->T->n_branches();

  for(int b=0;b<2*B;b++) {
    slice_sample_branch_length(P,Stats,b);
    bool changed = sample_SPR_search_one(*P.as<Parameters>(),Stats,b);
    if (not changed) three_way_topology_sample(P,Stats,b);
    slice_sample_branch_length(P,Stats,b);
  }
}

vector<int> path_to(const Tree& T,int n1, int n2) 
{
  assert(0 <= n1 and n1 < T.n_leaves());
  assert(0 <= n2 and n2 < T.n_leaves());
  assert(n1 != n2);

  vector<int> path; 
  path.push_back(n1);
  path.push_back(T.branch(n1).target());

  while(path.back() != n2) 
  {
    const_branchview b = T.directed_branch(path[path.size()-2], path[path.size()-1]);

    for(const_edges_after_iterator i=b.branches_after();i;i++) 
    {
      if (T.partition(*i)[n2]) {
	path.push_back((*i).target());
	break;
      }
    }
  }

  return path;
}

int jump() {
  double delta = 0;

  double U = uniform();
  if (U < 0.05)
    delta = 0;
  else if (U < 0.15)
    delta = 1;
  else {
    double a = 1;
    double b = 4;
    if (uniform() < 0.5) {
      a = 0.5;
      b = 4;
    }
    
    delta = 2+gamma(a,b);// mean = 2.5, shape = 2
  }
  if (uniform() < 0.5) delta = -delta;

  // round to the nearest integer
  return (int)floor(delta+0.5);
}



void choose_subtree_branch_nodes(const Tree& T,int & b1, int& b2) 
{
  //------------------- Choose nodes --------------------//
  int n1 = myrandom(T.n_leaves());
  int n2 = -1;
  do { n2 = myrandom(T.n_leaves());} while (n2 == n1);

  vector<int> path = path_to(T,n1,n2);
  assert(path.size() >= 3);

  //-------------- Choose subtree on path ----------------//
  int N = path.size() - 2;
  int A = 1+myrandom(N);

  b1 = -1;
  for(const_neighbors_iterator i=T[path[A]].neighbors();i;i++) {
    if (*i == path[A-1]) continue;
    if (*i == path[A+1]) continue;

    b1 = T.directed_branch(*i,path[A]);
    break;
  }
  assert(b1 != -1);

  //-------------- Choose branch on path ----------------//

  // The allowed branches are [Nl,Nr]
  int Nl = -(A-1) , Nr = N-A;

  // Jump and then reflect so that we stay in the interval
  int delta = wrap(jump(), Nl, Nr);
  assert(Nl <= delta and delta <= Nr);

  // walk 'delta' way from A
  int C2 = delta + A;

  assert(1 <= C2 and C2 <= path.size()-2);

  int C3 = C2;
  if (delta < 0) C3--;
  else C3++;

  assert(0 <= C3 and C3 <= path.size()-1);

  b2 = T.branch(path[C2],path[C3]);
}

void sample_SPR_nodes(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  Parameters& PP = *P.as<Parameters>();
  int n = n_SPR_moves(PP);

  double p = loadvalue(P->keys,"SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) {

    int b1=-1, b2=-1;
    choose_subtree_branch_nodes(*PP.T, b1, b2);

    double L_effective = effective_length(*PP.T, b1);

    if (not PP.variable_alignment() and uniform()< p) {
      MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2,true);
      SPR_inc(Stats,result,"SPR (path/slice)", L_effective);
    }
    else {
      MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2);
      SPR_inc(Stats,result,"SPR (path)", L_effective);
    }
  }
}
