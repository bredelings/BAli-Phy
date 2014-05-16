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
#include "probability/choose.H"
#include "likelihood.H"
#include "probability/probability.H"

#include "dp/3way.H"
#include "tree/tree-util.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-constraint.H"
#include "substitution/substitution-index.H"
#include "substitution/substitution.H"

using MCMC::MoveStats;

using boost::dynamic_bitset;
using std::vector;
using std::string;
using std::map;

using std::cout;
using std::cerr;
using std::endl;

int random_int_from_double(double x)
{
  int n = (int)x;
  x -= n;
  n += poisson(x);
  return n;
}

int n_SPR_moves(const Parameters& P)
{
  double f = P.load_value("SPR_amount",0.1);
  int n = random_int_from_double(P.T().n_branches()*f);
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

int topology_sample_SPR(vector<Parameters>& p,const vector<log_double_t>& rho,int n1, int n2) 
{
  assert(p.size() == 2);
  assert(p[0].variable_alignment() == p[1].variable_alignment());

  //----------- Generate the Different node lists ---------//
  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(p[0].T(), n1, n2);     // Using two random orders can lead to different total
  nodes[1] = A3::get_nodes_branch_random(p[1].T(), n1, n2);     //  probabilities for p[i] and p[j] when p[i] == p[j].

  try {
    return sample_tri_multi(p,nodes,rho,true,true);
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }
}

#include "slice-sampling.H"

int topology_sample_SPR_slice_connecting_branch(vector<Parameters>& p,int b) 
{
  int b_ = p[0].T().directed_branch(b).undirected_name();

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
double do_SPR(Parameters& P, int b1_,int b2) 
{
  const_branchview b1 = P.T().directed_branch(b1_);

  //------ Find the two old branches ------//
  vector<const_branchview> connected1;
  append(P.T().directed_branch(b1.source(),b1.target()).branches_after(),connected1);

  //------ Generate the new topology ------//
  if (P.T().directed_branch(b2).target() == b1.target() or 
      P.T().directed_branch(b2).source() == b1.target()) 
    ;
  else
    P.SPR(b1.reverse(),b2);

  //------ Find the two new branches ------//
  vector<const_branchview> connected2;
  append(P.T().directed_branch(b1.source(),b1.target()).branches_after(),connected2);

  assert(connected1.size() == 2);
  assert(connected2.size() == 2);

  //------- Place the split randomly -------//
  double L1 = connected1[0].length() + connected1[1].length();
  double L2 = connected2[0].length() + connected2[1].length();

  P.setlength_unsafe(P.T().directed_branch(connected2[0]), uniform() * L2 );
  P.setlength_unsafe(P.T().directed_branch(connected2[1]), L2 - P.T().directed_branch(connected2[0]).length() );

  return L2/L1;
}

// Consider penalizing lengths for being too close to equilibrium: branches couldn't get infinitely long.
// Consider using actual substitution matrices.
// Consider measuring similarities/differences by counting.
// Problem: how do we handle multiple partitions?

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

vector<double> effective_lengths_min(const Tree& T)
{
  vector<double> lengths(2*T.n_branches(),0);

  vector<const_branchview> branches = branches_from_leaves(T);

  for(int i=0;i<branches.size();i++)
  {
    lengths[branches[i]] = branches[i].length();

    vector<const_branchview> pre_b;
    append(branches[i].branches_before(),pre_b);
    if (pre_b.size() > 0) 
    {
      double min_prev = pre_b[0].length();
      for(int j=1;j<pre_b.size();j++)
	min_prev = std::min(min_prev, pre_b[j].length());

      lengths[branches[i]] += min_prev;
    }
  }

  return lengths;
}


int choose_SPR_target(const SequenceTree& T1, int b1_) 
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

  try {
    int b2 = branches[ choose(lengths) ];

    return b2;
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }
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

MCMC::Result SPR_stats(const Tree& T1, const Tree& T2, bool success, int bins, int b1 = -1)
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

  int n1 = P.T().directed_branch(b1).target();
  int n2 = P.T().directed_branch(b1).source();
  assert(P.T().partition(b1)[P.T().branch(b2).target()]);
  assert(P.T().partition(b1)[P.T().branch(b2).source()]);

  //----- Generate the Different Topologies ----//
  P.set_root(n1);
  vector<Parameters> p(2,P);

  //---------------- find the changed branches ------------------//
  vector<int> branches;
  for(const_edges_after_iterator i=p[1].T().directed_branch(n2,n1).branches_after();i;i++)
    branches.push_back((*i).undirected_name());
  //  std::cerr<<"before = "<<p[1].T<<endl;

  // FIXME - do we need to USE the ratio anywhere?
  double ratio = do_SPR(p[1],b1,b2);

  const SequenceTree& T0 = p[0].T();
  const SequenceTree& T1 = p[1].T();

  std::vector<int> nodes = A3::get_nodes_branch(T0,n1,n2);
  assert(T0.is_connected(nodes[0],nodes[1]));
  assert(T0.is_connected(nodes[0],nodes[2]));
  assert(T0.is_connected(nodes[0],nodes[3]));

  bool tree_changed = not T1.is_connected(nodes[0],nodes[2]) or not T1.is_connected(nodes[0],nodes[3]);

  // enforce tree constraints
  if (not extends(p[1].T(), *P.TC))
    return MCMC::Result(2+bins,0);

  //  std::cerr<<"after = "<<p[1].T<<endl;
  for(const_edges_after_iterator i=p[1].T().directed_branch(n2,n1).branches_after();i;i++)
    branches.push_back((*i).undirected_name());

  remove_duplicates(branches);
    
  //----------- invalidate caches for changed branches -----------//
  assert(branches.size() <= 3);
  for(int i=0;i<branches.size();i++) {
    int bi = branches[i];
    p[1].setlength(bi,p[1].T().directed_branch(bi).length());     // bidirectional effect
    if (tree_changed)
    {
      p[1].invalidate_subA_index_branch(bi);              // bidirectional effect
      if (p[1].variable_alignment()) 
	p[1].note_alignment_changed_on_branch(bi); 
    }
  }

  // If we reattach on a different branch than we pulled out of...
  assert(tree_changed == (branches.size() != 2));
  if (branches.size() != 2)
  {
    // ... then set the pairwise alignment of the branch we pulled out of.
    vector<const_branchview> branches2;
    for(auto i=p[0].T().directed_branch(n2,n1).branches_after();i;i++)
      branches2.push_back(*i);
    assert(branches2.size() == 2);
    int leaving1 = branches2[0].target();
    int leaving2 = branches2[1].target();
    p[1].recompute_pairwise_alignment(p[1].T().directed_branch(leaving1,leaving2));
  }

  int C;
  if (slice)
  {
    C = topology_sample_SPR_slice_slide_node(p,b1);
    if (C != -1)
      P = p[C];
  }
  else {

    //------------- change connecting branch length ----------------//
    vector<log_double_t> rho(2,1);

    //----------- sample alignments and choose topology -----------//
    C = topology_sample_SPR(p,rho,n1,n2);
    
    if (C != -1) 
    {
      for(int i=0;i<P.n_data_partitions();i++) {
	dynamic_bitset<> s1 = constraint_satisfied(P[i].alignment_constraint, P[i].A());
	dynamic_bitset<> s2 = constraint_satisfied(p[C][i].alignment_constraint, p[C][i].A());
	
	report_constraints(s1,s2,i);
      }
      P = p[C];

    // If the new topology conflicts with the constraints, then it should have P=0
    // and therefore not be chosen.  So the following SHOULD be safe!
    }
  }

  return SPR_stats(p[0].T(), p[1].T(), C>0, bins, b1);
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


void sample_SPR_flat_one(owned_ptr<Probability_Model>& P,MoveStats& Stats,int b1) 
{
  Parameters& PP = *P.as<Parameters>();

  if (PP.T().directed_branch(b1).target().is_leaf_node()) return;

  double p = P->load_value("SPR_slice_fraction",-0.25);

  int b2 = choose_SPR_target(PP.T(),b1);

  double L_effective = effective_length(PP.T(), b1);

  if (not PP.variable_alignment() and uniform() < p) {
    MCMC::Result result = sample_SPR(PP,b1,b2,true);
    SPR_inc(Stats,result,"SPR (flat/slice)",L_effective);
  }
  else  {
    MCMC::Result result = sample_SPR(PP,b1,b2);
    SPR_inc(Stats,result,"SPR (flat)",L_effective);
  }
}

log_double_t likelihood_unaligned_root(const Parameters& P)
{
  log_double_t Pr = 1;

  bool old = P.contains_key("no_unaligned_root");

  for(int i=0;i<P.n_data_partitions();i++)
    if (P[i].variable_alignment() and not old)
      Pr *= substitution::Pr_unaligned_root(P[i]);
    else
      Pr *= P[i].likelihood();
  return Pr;
}

log_double_t heated_likelihood_unaligned_root(const Parameters& P)
{
  return pow(likelihood_unaligned_root(P), P.get_beta());
}

/// Express branch \a b of tree \a T in terms of the nodes at either end
tree_edge get_tree_edge(const Tree& T, int b)
{
  int n1 = T.directed_branch(b).source();
  int n2 = T.directed_branch(b).target();
  return tree_edge(n1,n2);
}

std::ostream& operator<<(std::ostream& o, const tree_edge& b)
{
  o<<"["<<b.node1<<","<<b.node2<<"]";
  return o;
}

/// Represent positions along branches as a fraction in [0,1) from node1 to node2
struct spr_attachment_points: public map<tree_edge,double>
{
};

/// Represent the probability of attaching to a branch
struct spr_attachment_probabilities: public map<tree_edge,log_double_t>
{
  map<tree_edge,log_double_t> LLL;
};

/// Perform an SPR move: move the subtree BEHIND \a b1 to the branch indicated by \a b2,
///  and choose the point on the branch specified in \a locations.
int SPR_at_location(Parameters& P, int b_subtree, int b_target, const spr_attachment_points& locations, int branch_to_move = -1)
{
  double total_length_before = tree_length(P.T());

  // unbroken target branch
  /// \todo Correctly handle moving to the same topology -- but allow branch lengths to change.
  double L = P.T().directed_branch(b_target).length();
  map<tree_edge, double>::const_iterator record = locations.find(get_tree_edge(P.T(),b_target));
  if (record == locations.end())
  {
    std::cerr<<"Branch not found in spr location object!\n"<<std::endl;
    std::abort();
  }
  tree_edge B_unbroken_target = record->first;
  // U is the fraction of the way from B_unbroken_target.node1 
  // toward B_unbroken_target.node2 to place the new node.
  double U = record->second; 
  
  // node joining the subtree to the rest of the tree
  int n0 = P.T().directed_branch(b_subtree).target();

  // Perform the SPR operation (specified by a branch TOWARD the pruned subtree)
  int BM = P.SPR(P.T().directed_branch(b_subtree).reverse(), b_target, branch_to_move);

  // Find the names of the branches
  assert(P.T().is_connected(B_unbroken_target.node1, n0));
  assert(P.T().is_connected(n0, B_unbroken_target.node2));
  int b1 = P.T().directed_branch(B_unbroken_target.node1, n0);
  int b2 = P.T().directed_branch(n0, B_unbroken_target.node2);
  assert(P.T().directed_branch(b1).undirected_name() == BM or P.T().directed_branch(b2).undirected_name() == BM);

  // Set the lengths of the two branches
  double L1 = L*U;
  double L2 = L - L1;

  P.setlength_no_invalidate_LC(b1, L1);
  P.setlength_no_invalidate_LC(b2, L2);

  double total_length_after = tree_length(P.T());
  assert(std::abs(total_length_after - total_length_before) < 1.0e-9);

  // Return the branch name that moved to the new attachment location.
  return BM;
}

/// A struct to compute and store information about attachment points their branch names
struct spr_info
{
public:
  /// The tree on with we are considering SPR moves
  const Tree T;

  /// The branch pointing AWAY from the subtree to prune
  int b_parent;

  /// The two branches that make up the current attachment branch
  vector<const_branchview> child_branches;

  /// The name of the first child branch -- which will remain here
  int B1;

  /// The name of the second child branch -- which will move to wherever we regraft
  int BM;

  /// The current attachment branch, specified in terms of its endpoint nodes
  tree_edge B0;

  /// A list of attachment branches, where the current branch is B1 at index 0
  vector<const_branchview> attachment_branches;

  /// A mapping of directed branches to their index in attachment_branches
  vector<int> branch_to_index_;

  /// The number of places we could regraft, including the current site
  unsigned n_attachment_branches() const {return attachment_branches.size();}

  /// The length of each attachment branch, indexed in the same way as attachment_branches
  vector<double> attachment_branch_lengths() const
  {
    vector<double> L(n_attachment_branches());
    L[0] = child_branches[0].length() + child_branches[1].length();

    for(int i=1;i<n_attachment_branches();i++)
      L[i] = attachment_branches[i].length();
    return L;
  }
  
  /// Express a branch \a in attachment_branches in terms of its endpoint nodes
  tree_edge get_tree_edge(int b) const
  {
    if (not T.subtree_contains_branch(b_parent,b))
      throw myexception()<<"spr_info::get_tree_edge( ): Subtree does not contain branch "<<b;

    int n0 = T.directed_branch(b_parent).target();
    if (T.directed_branch(b).target() == n0 or T.directed_branch(b).source() == n0)
      return B0;

    return ::get_tree_edge(T,b);
  }

  /// Convert a branch \a b to its index in attachment_branches
  int branch_to_index(int b) const
  {
    assert(0 <=b and b <= 2*T.n_branches());
    int index = branch_to_index_[b];
    assert(0 <= index and index < n_attachment_branches());
    return index;
  }

  /// Convert abranch \a s to its index in attachment_branches
  int tree_edge_to_index(const tree_edge& s) const
  {
    if (s == B0)
      return 0;
    const_branchview b = T.directed_branch(s.node1, s.node2);
    assert(b.valid());
    return branch_to_index(b);
  }

  /// Express properties of branches as vectors indexed by their position in attachment_branches
  template<typename U>
  vector<U> convert_to_vector(const map<tree_edge,U>& M) const
  {
    assert(M.size() == n_attachment_branches());
    vector<U> v(n_attachment_branches());

    for(typename map<tree_edge,U>::const_iterator i=M.begin();i != M.end(); i++)
    {
      int index = tree_edge_to_index(i->first);
      v[index] = i->second;
    }

    return v;
  }

  spr_info(const Tree& T_, int b, int branch_to_move = -1);
}; 

spr_info::spr_info(const Tree& T_, int b, int branch_to_move)
  :T(T_),b_parent(b),B1(-1),BM(-1), branch_to_index_(T.n_branches()*2, -1)
{
  child_branches = randomized_branches_after(T.directed_branch(b_parent));
  assert(child_branches.size() == 2);
  B1 = child_branches[0].undirected_name();
  BM = child_branches[1].undirected_name();

  if (branch_to_move == -1) {
    if (BM < B1) std::swap(B1,BM);
  }
  else {
    if (branch_to_move != BM)
      std::swap(B1,BM);
    assert(branch_to_move == BM);
  }
  B1 = std::min(child_branches[0].undirected_name(), child_branches[1].undirected_name());
  BM = std::max(child_branches[0].undirected_name(), child_branches[1].undirected_name());
  B0 = tree_edge(child_branches[0].target(), child_branches[1].target());

  /*----------- get the list of possible attachment points, with [0] being the current one.------- */
  // \todo - With tree constraints, or with a variable alignment and alignment constraints,
  //          we really should eliminate branches that we couldn't attach to, here.

  // FIXME - in order to make this independent of the circular order, we should make
  // a randomized_all_branches_after, or a sorted_all_branches_after.
  attachment_branches = branches_after(T,b_parent);

  // remove the moving branch name (BM) from the list of attachment branches
  for(int i=attachment_branches.size()-1;i>=0;i--)
    if (attachment_branches[i].undirected_name() == BM)
      attachment_branches.erase(attachment_branches.begin()+i);

  // convert the const_branchview's to int names
  //  vector<int> branch_names = directed_names(branches);

  /*--------------Construct a mapping from branch to index -------------*/
  for(int i=0;i<n_attachment_branches();i++)
  {
    int b = attachment_branches[i];
    int b_t = attachment_branches[i].reverse();

    branch_to_index_[b] = i;
    branch_to_index_[b_t] = i;
  }
}

/// Get a list of attachment branches, and a location for attachment on each branch
spr_attachment_points get_spr_attachment_points(const Tree& T, int b1, int branch_to_move = -1)
{
  spr_info I(T, b1, branch_to_move);

  tree_edge B0(I.child_branches[0].target(), I.child_branches[1].target());
  double L0a = I.child_branches[0].length();
  double L0b = I.child_branches[1].length();

  // compute attachment location for current branche
  spr_attachment_points locations;
  locations[B0] = L0a/(L0a+L0b);

  // compute attachment locations for non-current branches
  for(int i=1;i<I.attachment_branches.size();i++)
    locations[get_tree_edge(T, I.attachment_branches[i])] = uniform();

  return locations;
}

/// Compute the probability of pruning b1^t and regraftion at \a locations
///
/// After this routine, likelihood caches and subalignment indices for branches in the
/// non-pruned subtree should reflect the situation where the subtree has been pruned.
///
spr_attachment_probabilities SPR_search_attachment_points(Parameters& P, int b1, const spr_attachment_points& locations, int branch_to_move = -1)
{
  // The attachment node for the pruned subtree.
  // This node will move around, but we will always peel up to this node to calculate the likelihood.
  int root_node = P.T().directed_branch(b1).target(); 
  // Because the attachment node keeps its name, this will stay in effect throughout the likelihood calculations.
  P.set_root(root_node);

  // Compute and cache conditional likelihoods up to the (likelihood) root node.
  P.heated_likelihood();

  const SequenceTree T0 = P.T();

  /* MOVEABLE BRANCH */
  //   One of the two branches (B1) that it (b1) points to will be considered the current attachment branch,
  //    the other branch (BM) will move around to wherever we are currently attaching b1.
  //   This is kind of a limitation of the current SPR routine, which chooses to move the 
  //    branch with the larger name, and leave the other one in place.

  spr_info I(T0, b1, branch_to_move);

  if (I.n_attachment_branches() == 1) return spr_attachment_probabilities();

  vector<double> L = I.attachment_branch_lengths();

  // convert the const_branchview's to int names
  vector<int> branch_names = directed_names(I.attachment_branches);

  /*----------------------- Initialize likelihood for each attachment point ----------------------- */

  // The probability of attaching to each branch, w/o the alignment probability
  spr_attachment_probabilities Pr;
  Pr[I.B0] = P.heated_likelihood() * P.prior_no_alignment();

#ifdef DEBUG_SPR_ALL
  Pr.LLL[I.B0] = P.heated_likelihood();

  log_double_t PR1 = P.heated_likelihood();
  log_double_t PR2 = heated_likelihood_unaligned_root(P);
    
  assert(std::abs(PR1.log() - PR2.log()) < 1.0e-8);
#endif

  /*----------- Begin invalidating caches and subA-indices to reflect the pruned state -------------*/

  // At this point, caches for branches pointing to B1 and BM are accurate -- but everything after them
  //  still assumes we haven't pruned and is therefore inaccurate.

  P.LC_invalidate_branch(I.B1);          // invalidate caches       for B1, B1^t and ALL BRANCHES AFTER THEM.
  P.invalidate_subA_index_branch(I.B1);  // invalidate subA-indices for B1, B1^t and ALL BRANCHES AFTER THEM.

  P.LC_invalidate_branch(I.BM);          // invalidate caches       for BM, BM^t and ALL BRANCHES AFTER THEM.
  P.invalidate_subA_index_branch(I.BM);  // invalidate subA-indices for BM, BM^t and ALL BRANCHES AFTER THEM.

  // Temporarily stop checking subA indices of branches that point away from the cache root
  P.subA_index_allow_invalid_branches(true);

  // Compute the probability of each attachment point
  // After this point, the LC root will now be the same node: the attachment point.
  for(int i=1;i<branch_names.size();i++) 
  {
    // Define target branch b2 - pointing away from b1
    int b2 = branch_names[i];
    tree_edge B2 = I.get_tree_edge(b2);

    // ** 1. SPR ** : alter the tree.
    P.set_tree(T0);
    int BM2 = SPR_at_location(P, b1, b2, locations, I.BM);
    assert(BM2 == I.BM); // Due to the way the current implementation of SPR works, BM (not B1) should be moved.

    // The length of B1 should already be L0, but we need to reset the transition probabilities (MatCache)
    assert(std::abs(P.T().branch(I.B1).length() - L[0]) < 1.0e-9);

    // We want caches for each directed branch that is not in the PRUNED subtree to be accurate
    //   for the situation that the PRUNED subtree is not behind them.


    // ** 2. INVALIDATE ** the branch that we just landed on and altered

    /// \todo Do I really need to invalidate BOTH directions of b2?  Or, do I just not know WHICH direction to invalidate?
    /// You'd think I'd just need to invalidate the direction pointing TOWARD the root.

    /// \todo Can I temporarily associate the branch with a NEW token, or copy the info to a new location?

    // We want to suppress the bidirectional propagation of invalidation for all branches after this branch.
    // It would be nice to save the old exp(tB) and switch back to it later.
    P.setlength_no_invalidate_LC(b2,P.T().directed_branch(b2).length());     // Recompute the transition matrix
    P.LC_invalidate_one_branch(b2);                                         //  ... mark likelihood caches for recomputing.
    P.LC_invalidate_one_branch(P.T().directed_branch(b2).reverse());         //  ... mark likelihood caches for recomputing.

    P.setlength_no_invalidate_LC(I.BM,P.T().directed_branch(I.BM).length()); // Recompute the transition matrix
    P.LC_invalidate_one_branch(I.BM);                                       //  ... mark likelihood caches for recomputing.
    P.LC_invalidate_one_branch(P.T().directed_branch(I.BM).reverse());       //  ... mark likelihood caches for recomputing.

    // **3. RECORD** the tree and likelihood
    Pr[B2] = heated_likelihood_unaligned_root(P) * P.prior_no_alignment();
#ifdef DEBUG_SPR_ALL
    log_double_t PR2 = heated_likelihood_unaligned_root(P);
    Pr.LLL[B2] = PR2;
    //    log_double_t PR1 = P.heated_likelihood();
    //    cerr<<"  PR1 = "<<PR1.log()<<"  PR2 = "<<PR2.log()<<"   diff = "<<PR2.log() - PR1.log()<<endl;
#endif

    // **4. INVALIDATE** the DIRECTED branch that we just landed on and altered
    P.setlength_no_invalidate_LC(b2,L[i]);                               // Put back the old transition matrix
    P.LC_invalidate_one_branch(b2);                                      // ... mark likelihood caches for recomputing.
    P.LC_invalidate_one_branch(P.T().directed_branch(b2).reverse());      // ... mark likelihood caches for recomputing.

    // this is bidirectional, but does not propagate
    P.invalidate_subA_index_one_branch(I.BM);
  }

  // We had better not let this get changed!
  for(int i=0;i<P.n_data_partitions();i++)
    assert(P[i].LC.root == root_node);

  return Pr;
}

/// This just computes nodes and calls sample_tri_multi
bool SPR_accept_or_reject_proposed_tree(Parameters& P, vector<Parameters>& p,
					const vector<log_double_t>& Pr,
					const vector<log_double_t>& PrL,
					const spr_info& I, int C,
					const spr_attachment_points& locations
					)
{
  int b1 = I.b_parent;
  int n1 = P.T().directed_branch(b1).target();
  int n2 = P.T().directed_branch(b1).source();

  assert(p.size() == 2);
  assert(p[0].variable_alignment() == p[1].variable_alignment());

  vector<double> L = I.attachment_branch_lengths();

  //----------------- Generate the Different node lists ---------------//
  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(p[0].T(), n1, n2);     // Using two random orders can lead to different total
  nodes[1] = A3::get_nodes_branch_random(p[1].T(), n1, n2);     //  probabilities for p[0] and p[1] when p[0] == p[1].
  sample_tri_multi_calculation tri(p, nodes, true, true);

  //--------- Compute PrL2: reverse proposal probabilities ---------//

  vector<log_double_t> PrL2 = PrL;
#ifndef DEBUG_SPR_ALL
  if (P.variable_alignment())
#endif
  {
    Parameters P_temp = p[1];
    spr_attachment_probabilities PrB2 = SPR_search_attachment_points(P_temp, b1, locations, I.BM);
    vector<log_double_t> Pr2 = I.convert_to_vector(PrB2);
    
    if (not P.variable_alignment())
      for(int i=0;i<Pr.size();i++)
	assert(std::abs(Pr[i].log() - Pr2[i].log()) < 1.0e-9);
    
    PrL2 = Pr2;
    for(int i=0;i<PrL2.size();i++)
      PrL2[i] *= L[i];
  }

  //----------------- Specify proposal probabilities -----------------//
  vector<log_double_t> rho(2,1);
  rho[0] = L[0]*choose_MH_P(0, C, PrL ); // Pr(proposing 0->C)
  rho[1] = L[C]*choose_MH_P(C, 0, PrL2); // Pr(proposing C->0)
  
  tri.set_proposal_probabilities(rho);

  //------------- Accept or reject the proposed topology -------------//
  int C2 = tri.choose(p);

  // If the alignment is not variable, then we should always accept on this second move.
  //
  // If the alignment is variable, then we might still choose C2==0 because of the
  // different node orders in sample_tri( ).
  if (not P.variable_alignment()) 
    assert(C2 == 1);

  // If the move violates alignment constraints the we can't accept it.
  if (C2 == -1) return false;

  //---------------------- Update P based on choice ------------------//
  for(int i=0;i<P.n_data_partitions();i++) {
    dynamic_bitset<> s1 = constraint_satisfied(P[i].alignment_constraint, P[i].A());
    dynamic_bitset<> s2 = constraint_satisfied(p[C2][i].alignment_constraint, p[C2][i].A());
    
    report_constraints(s1,s2,i);
  }
  P = p[C2];
  
  return (C2 == 1);
}

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

bool sample_SPR_search_one(Parameters& P,MoveStats& Stats,int b1) 
{
  const int bins = 6;

  if (P.T().directed_branch(b1).target().is_leaf_node()) return false;

  // The attachment node for the pruned subtree.
  // This node will move around, but we will always peel up to this node to calculate the likelihood.
  int root_node = P.T().directed_branch(b1).target(); 
  // Because the attachment node keeps its name, this will stay in effect throughout the likelihood calculations.
  P.set_root(root_node);

  // Compute and cache conditional likelihoods up to the (likelihood) root node.
  P.heated_likelihood();

  spr_attachment_points locations = get_spr_attachment_points(P.T(), b1);

  const SequenceTree T0 = P.T();
  vector<Parameters> p(2,P);

  spr_info I(T0, b1);

  // Compute total lengths for each of the possible attachment branches
  vector<double> L = I.attachment_branch_lengths();

  if (I.n_attachment_branches() == 1) return false;

  // convert the const_branchview's to int names
  vector<int> branch_names = directed_names(I.attachment_branches);

  spr_attachment_probabilities PrB = SPR_search_attachment_points(p[1], b1, locations, I.BM);

  vector<log_double_t> Pr = I.convert_to_vector(PrB);
#ifdef DEBUG_SPR_ALL
  vector<log_double_t> LLL = I.convert_to_vector(PrB.LLL);
#endif

  // Step N-2: CHOOSE an attachment point

  vector<log_double_t> PrL = Pr;
  for(int i=0;i<PrL.size();i++)
    PrL[i] *= L[i];

  int C = -1;
  try {
    C = choose_MH(0,PrL);
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }

  // Step N-1: ATTACH to that point

  p[1].set_tree(T0);
  if (C != 0)
    SPR_at_location(p[1], b1, branch_names[C], locations);

  // enforce tree constraints
  if (not extends(p[1].T(), *P.TC))
    return false;

  // Step N: INVALIDATE subA indices and also likelihood caches that are no longer valid.

  // Note that bi-directional invalidation of BM invalidates b1^t and similarly directed branches in the pruned subtree.
  // (BM is an undirected name, so all effects in the loop below MUST be bi-directional.)
  vector<int> btemp = {I.B1, I.BM, branch_names[C]};
  // (These effects go out from the old location (the merged branch B1)
  //  and the new location (the split branches BM and branch_names[C]) )
  for(int i=0;i<btemp.size();i++) {
    int bi = btemp[i];
    p[1].setlength(bi, p[1].T().directed_branch(bi).length());   // bidirectional effect
    p[1].invalidate_subA_index_branch(bi);         // bidirectional effect
    if (p[1].variable_alignment()) 
      p[1].note_alignment_changed_on_branch(bi); 
  }
  p[1].recompute_pairwise_alignment(I.B1);
  p[1].subA_index_allow_invalid_branches(false);

#ifdef DEBUG_SPR_ALL
  // The likelihood for attaching at a particular place should not
  // depend on the initial attachment point.
  log_double_t L_1 = heated_likelihood_unaligned_root(p[1]);
  assert(std::abs(L_1.log() - LLL[C].log()) < 1.0e-9);
#endif

  // Step N+1: Use the chosen tree as a proposal, allowing us to sample the alignment.
  //           (This should always succeed, if the alignment is fixed.)

  // Do we accept the tree proposed above?
  bool accepted = true;

  // Only resample the alignment if we are proposing a different topology.
  /// \todo Mixing: Should we realign at least one other attachment location, if P.variable_alignment()?
  try
  {
    if (C > 0)
      accepted = SPR_accept_or_reject_proposed_tree(P, p, Pr, PrL, I, C, locations);
  }
  catch (std::bad_alloc&) {
    std::cerr<<"Allocation failed in sample_try_multi (in SPR_search_one)!  Proceeding."<<std::endl;
    return false;
  }


  MCMC::Result result = SPR_stats(T0, p[1].T(), accepted, bins, b1);
  double L_effective = effective_length(P.T(), b1);
  SPR_inc(Stats, result, "SPR (all)", L_effective);

  return ((C != 0) and accepted);
}

void sample_SPR_all(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  Parameters& PP = *P.as<Parameters>();
  int n = n_SPR_moves(PP);

  // double p = P.load_value("SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) 
  {
    // Choose a directed branch to prune and regraft -- pointing away from the pruned subtree.
    int b1 = choose_subtree_branch_uniform2(PP.T());

    sample_SPR_search_one(PP, Stats, b1);
  }

  if (P->load_value("SPR_longest", 1.0) > 0.5)
  {
    // Try moving the longest or least-determined branch every time.
    int least_informed_branch = argmax(effective_lengths_min(PP.T()));
    sample_SPR_flat_one(P, Stats, least_informed_branch);
    sample_SPR_search_one(PP, Stats, least_informed_branch);
  }
}

void sample_SPR_search_all(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  int B = P.as<Parameters>()->T().n_branches();

  for(int b=0;b<2*B;b++) {
    slice_sample_branch_length(P,Stats,b);
    bool changed = sample_SPR_search_one(*P.as<Parameters>(),Stats,b);
    if (not changed) three_way_topology_sample(P,Stats,b);
    slice_sample_branch_length(P,Stats,b);
  }
}

void sample_SPR_A_search_all(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  int B = P.as<Parameters>()->T().n_branches();

  for(int b=0;b<2*B;b++) {
    slice_sample_branch_length(P,Stats,b);
    sample_SPR_flat_one(P, Stats, b);
    bool changed = sample_SPR_search_one(*P.as<Parameters>(),Stats,b);
    if (not changed) three_way_topology_sample(P,Stats,b);
    slice_sample_branch_length(P,Stats,b);
    scale_means_only(P,Stats);
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
  for(const_neighbors_iterator i=T.node(path[A]).neighbors();i;i++) {
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

void sample_SPR_flat(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  Parameters& PP = *P.as<Parameters>();
  int n = n_SPR_moves(PP);

  //  double p = P->load_value("SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) 
  {
    int b1 = choose_subtree_branch_uniform(PP.T());

    sample_SPR_flat_one(P, Stats, b1);
  }

  if (P->load_value("SPR_longest", 1.0) > 0.5)
  {
    // Try moving the longest or least-determined branch every time.
    int least_informed_branch = argmax(effective_lengths_min(PP.T()));
    sample_SPR_flat_one(P, Stats, least_informed_branch);
    sample_SPR_search_one(PP, Stats, least_informed_branch);
  }
}

void sample_SPR_nodes(owned_ptr<Probability_Model>& P,MoveStats& Stats) 
{
  Parameters& PP = *P.as<Parameters>();
  int n = n_SPR_moves(PP);

  double p = P->load_value("SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) {

    int b1=-1, b2=-1;
    choose_subtree_branch_nodes(PP.T(), b1, b2);

    double L_effective = effective_length(PP.T(), b1);

    if (not PP.variable_alignment() and uniform()< p) {
      MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2,true);
      SPR_inc(Stats,result,"SPR (path/slice)", L_effective);
    }
    else {
      MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2);
      SPR_inc(Stats,result,"SPR (path)", L_effective);
    }
  }

  if (P->load_value("SPR_longest", 1.0) > 0.5)
  {
    // Try moving the longest or least-determined branch every time.
    int least_informed_branch = argmax(effective_lengths_min(PP.T()));
    sample_SPR_flat_one(P, Stats, least_informed_branch);
    sample_SPR_search_one(PP, Stats, least_informed_branch);
  }
}
