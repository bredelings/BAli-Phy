#include <cmath>
#include <valarray>
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "choose.H"
#include "likelihood.H"

#include "3way.H"
#include "alignment-sums.H"
#include "alignment-constraint.H"
#include "substitution-index.H"

using MCMC::MoveStats;

///Sample between 2 topologies, ignoring gap priors on each case
int sample_SPR_and_A(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho,int n1, int n2) 
{
  //----------- Generate the Different node lists ---------//
  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(p[0].T,n1,n2);
  nodes[1] = A3::get_nodes_branch_random(p[1].T,n1,n2);

  return sample_tri_multi(a,p,nodes,rho,true,true);
}

///Sample between 2 topologies, ignoring gap priors on each case
int topology_sample_SPR_sgaps(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho) 
{
  efloat_t Pr1 = rho[0] * p[0].probability(a[0],p[0]);
  efloat_t Pr2 = rho[1] * p[0].probability(a[1],p[1]);

  return choose2(Pr1,Pr2);
}

int topology_sample_SPR(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho,int n1, int n2) 
{
  assert(p[0].IModel().full_tree == p[1].IModel().full_tree);

  if (p[0].IModel().full_tree)
    return sample_SPR_and_A(a,p,rho,n1,n2);
  else
    return topology_sample_SPR_sgaps(a,p,rho);
}

double do_SPR(SequenceTree& T1, int b1_) 
{
  const_branchview b1 = T1.directed_branch(b1_);

  //----- Select the branch to move to ------//
  valarray<bool> subtree_nodes = T1.partition(b1.reverse());
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

  SequenceTree T2 = T1;

  //------ Generate the new topology ------//
  if (T2.directed_branch(b2).target() == b1.target() or T2.directed_branch(b2).source() == b1.target()) 
    ;
  else
    T2.SPR(b1.reverse(),b2);

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

void remove_duplicates(vector<int>& v) {
  for(int i=v.size()-1;i>=0;i--) {
    bool dup=false;
    for(int j=0;j<i and not dup;j++)
      if (v[j] == v[i]) dup=true;
    if (dup)
      v.erase(v.begin()+i);
  }
}

void sample_SPR(alignment& A,Parameters& P,MoveStats& Stats, int b) 
{
  //----- Get nodes for directed branch ------//

  const_branchview bv = P.T.directed_branch(b).reverse();
  if (myrandomf()< 0.5)
    bv = bv.reverse();
  if (bv.target().is_leaf_node())
    bv = bv.reverse();
  
  b = bv;
  int n1 = bv.target();
  int n2 = bv.source();

  //----- Generate the Different Topologies ----//
  P.LC.root = n1;
  vector<alignment> a(2,A);
  vector<Parameters> p(2,P);

  SequenceTree& T1 = p[0].T;
  SequenceTree& T2 = p[1].T;

  // find the changed branches
  vector<int> branches;
  for(edges_after_iterator i=T2.directed_branch(n2,n1).branches_after();
      i;i++)
    branches.push_back((*i).undirected_name());
  //  std::cerr<<"before = "<<T1<<endl;
  double ratio = do_SPR(T2,b);
  //  std::cerr<<"after = "<<T2<<endl;
  for(edges_after_iterator i=T2.directed_branch(n2,n1).branches_after();
      i;i++)
    branches.push_back((*i).undirected_name());

  remove_duplicates(branches);
    
  // recompute/invalidate caches associated with changed branch lengths
  assert(branches.size() <= 3);
  for(int i=0;i<branches.size();i++) {
    int bi = branches[i];
    p[1].setlength(bi,p[1].T.directed_branch(bi).length());
    invalidate_subA_index_branch(a[1], p[1].T, branches[i]);
  }
  
  vector<efloat_t> rho(2,1);
  rho[1] = ratio;
  int C = topology_sample_SPR(a,p,rho,n1,n2);

  if (C != -1) 
  {
    valarray<bool> s1 = constraint_satisfied(P.alignment_constraint,A);
    A = a[C];
    P = p[C];
    valarray<bool> s2 = constraint_satisfied(P.alignment_constraint,A);

    // If the new topology conflicts with the constraints, then it should have P=0
    // and therefore not be chosen.  So the following SHOULD be safe!
    report_constraints(s1,s2);
  }

  //------ Check if topology changed ------//
  vector<const_branchview> connected1;
  append(p[0].T.directed_branch(n2,n1).branches_after(),connected1);

  vector<const_branchview> connected2;
  append(p[1].T.directed_branch(n2,n1).branches_after(),connected2);
 
  bool same_topology = (
			(connected1[0] == connected2[0] and connected1[1] == connected2[1]) or
			(connected1[0] == connected2[1] and connected1[1] == connected2[0])
			);

  const int bins = 3;
  MCMC::Result result(2+bins,0);

  result.counts[0] = 1;
  if (C>0) result.totals[0] = 1;

  int dist = (int)topology_distance(T1,T2);
  std::cerr<<"NNI distance = "<<topology_distance(T1,T2)<<std::endl;

  std::cerr<<"connected1 = "<<connected1[0]<<"  "<<connected1[1]<<std::endl;
  std::cerr<<"connected2 = "<<connected2[0]<<"  "<<connected2[1]<<std::endl;

  std::cerr<<"T1 = "<<T1<<std::endl;
  std::cerr<<"T2 = "<<T2<<std::endl;

  if (same_topology) 
    assert(dist == 0);
  else
    assert(dist > 0);

  // count dist in [0,bins)
  if (dist > bins) dist = bins;
  for(int i=0;i<=bins;i++) {
    if (dist == i) {
      result.counts[1+i] = 1;
      if (C>0) result.totals[1+i] = 1;
    }
  }

  Stats.inc("SPR", result);
}
